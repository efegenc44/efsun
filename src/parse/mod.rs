pub mod definition;
pub mod expression;
pub mod lex;
pub mod pattern;
pub mod type_expression;

use std::iter::Peekable;

use crate::{
    error::{ReportableError, Result},
    interner::{InternId, Interner},
    location::{Located, Span},
    metadata::IndexState,
    parse::definition::Module,
};

use lex::{Lexer, token::Token};

use definition::Definition;
use expression::Expression;
use pattern::Pattern;
use type_expression::TypeExpression;

const PRIMARY_EXPRESSION_START: &[Token] = &[
    Token::String(InternId::dummy()),
    Token::Identifier(InternId::dummy()),
    Token::LeftParenthesis,
];

const PRIMARY_TYPE_EXPRESSION_START: &[Token] =
    &[Token::Identifier(InternId::dummy()), Token::LeftParenthesis];

const DEFINITION_START: &[Token] = &[
    Token::LetKeyword,
    Token::ModuleKeyword,
    Token::ImportKeyword,
    Token::StructureKeyword,
];

const PATTERN_START: &[Token] = &[
    Token::String(InternId::dummy()),
    Token::Tilde,
    Token::Identifier(InternId::dummy()),
    Token::LeftParenthesis,
];

pub struct Parser<'source, 'interner> {
    tokens: Peekable<Lexer<'source, 'interner>>,
    source_name: &'source str,
    indicies: IndexState,
}

impl<'source, 'interner> Parser<'source, 'interner> {
    pub fn from_source(
        source_name: &'source str,
        source: &'source str,
        indicies: IndexState,
        interner: &'interner mut Interner,
    ) -> Self {
        Self {
            tokens: Lexer::new(source_name, source, interner).peekable(),
            source_name,
            indicies,
        }
    }

    pub fn indicies(&self) -> IndexState {
        self.indicies
    }

    fn peek(&mut self) -> Option<Result<Located<Token>>> {
        self.tokens.peek().cloned()
    }

    fn next(&mut self) -> Option<Result<Located<Token>>> {
        self.tokens.next()
    }

    fn peek_some(&mut self) -> Result<Located<Token>> {
        self.peek().unwrap_or_else(|| {
            Err(ReportableError::eof(
                ParseError::UnexpectedEOF,
                self.source_name.to_string(),
            ))
        })
    }

    fn peek_is_one_of(&mut self, list: &[Token]) -> Result<bool> {
        let Some(peek) = self.peek() else {
            return Ok(false);
        };
        let peek = peek?;

        Ok(list
            .iter()
            .any(|token| std::mem::discriminant(token) == std::mem::discriminant(peek.data())))
    }

    fn peek_is(&mut self, token: Token) -> Result<bool> {
        self.peek_is_one_of(&[token])
    }

    fn next_some(&mut self) -> Result<Located<Token>> {
        self.peek_some()?;
        self.next().unwrap()
    }

    fn next_if_peek(&mut self, expected: Token) -> Result<bool> {
        if self.peek_is(expected)? {
            self.next();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn expect(&mut self, expected: Token) -> Result<Located<Token>> {
        let token = self.next_some()?;

        if std::mem::discriminant(&expected) == std::mem::discriminant(token.data()) {
            Ok(token)
        } else {
            self.error(
                ParseError::UnexpectedToken {
                    unexpected: token.into_data(),
                    expected,
                },
                token.span(),
            )
        }
    }

    fn expect_identifier(&mut self) -> Result<Located<InternId>> {
        let token = self.expect(Token::Identifier(InternId::dummy()))?;
        let Token::Identifier(id) = token.data() else {
            unreachable!()
        };
        Ok(Located::new(*id, token.span()))
    }

    pub fn expression_repl(&mut self) -> Result<Located<Expression>> {
        let expression = self.expression()?;

        // NOTE: Maybe it is not needed
        if self.peek().is_some() {
            return Err(ReportableError::eof(
                ParseError::IllFormedExpression,
                self.source_name.to_string(),
            ));
        }

        Ok(expression)
    }

    pub fn expression(&mut self) -> Result<Located<Expression>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::Backslash => self.lambda(),
            Token::LetKeyword => self.letin(),
            Token::MatchKeyword => self.matchas(),
            _ => self.application(),
        }
    }

    fn lambda(&mut self) -> Result<Located<Expression>> {
        let start = self.expect(Token::Backslash)?.span().start();
        let variable = self.expect_identifier()?;
        let expression = self.expression()?;
        let end = expression.span().end();

        let lambda = expression::Lambda::new(
            variable,
            expression,
            self.indicies.new_capture_id(),
            self.indicies.new_unique_name_id(),
        );
        let expression = Located::new(Expression::Lambda(lambda), Span::new(start, end));

        Ok(expression)
    }

    fn letin(&mut self) -> Result<Located<Expression>> {
        let start = self.expect(Token::LetKeyword)?.span().start();
        let variable = self.expect_identifier()?;
        self.expect(Token::Equals)?;
        let variable_expression = self.expression()?;
        self.expect(Token::InKeyword)?;
        let return_expression = self.expression()?;
        let end = return_expression.span().end();

        let letin = expression::LetIn::new(
            variable,
            variable_expression,
            return_expression,
            self.indicies.new_unique_name_id(),
        );
        let expression = Located::new(Expression::LetIn(letin), Span::new(start, end));

        Ok(expression)
    }

    fn matchas(&mut self) -> Result<Located<Expression>> {
        let start = self.expect(Token::MatchKeyword)?.span().start();
        let expression = self.expression()?;
        let mut end = expression.span().end();

        let mut branches = Vec::new();
        while self.next_if_peek(Token::Bar)? {
            let branch = self.match_branch()?;
            end = branch.span().end();
            branches.push(branch);
        }

        let matchas = expression::MatchAs::new(expression, branches);
        let expression = Located::new(Expression::MatchAs(matchas), Span::new(start, end));

        Ok(expression)
    }

    fn match_branch(&mut self) -> Result<Located<expression::matchas::Branch>> {
        let pattern = self.pattern()?;
        let start = pattern.span().start();
        self.expect(Token::Equals)?;
        let expression = self.expression()?;
        let end = expression.span().end();

        let branch = expression::matchas::Branch::new(pattern, expression);
        let branch = Located::new(branch, Span::new(start, end));

        Ok(branch)
    }

    fn pattern(&mut self) -> Result<Located<Pattern>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::String(string) => self.pattern_literal(Pattern::String(*string)),
            Token::Tilde => self.pattern_any(),
            Token::Identifier(_) => self.pattern_structure(),
            Token::LeftParenthesis => self.pattern_grouping(),
            unexpected => self.error(
                ParseError::UnexpectedTokenStart {
                    unexpected: *unexpected,
                    expected: PATTERN_START,
                },
                token.span(),
            ),
        }
    }

    fn pattern_literal(&mut self, literal: Pattern) -> Result<Located<Pattern>> {
        let span = self.next().unwrap()?.span();
        Ok(Located::new(literal, span))
    }

    fn pattern_any(&mut self) -> Result<Located<Pattern>> {
        let start = self.expect(Token::Tilde)?.span().start();
        let identifier = self.expect_identifier()?;
        let end = identifier.span().end();
        let any = pattern::Any::new(identifier.into_data(), self.indicies.new_unique_name_id());

        Ok(Located::new(Pattern::Any(any), Span::new(start, end)))
    }

    fn pattern_structure(&mut self) -> Result<Located<Pattern>> {
        let parts = self.path_parts()?;
        let start = parts.span().start();
        let mut end = parts.span().end();
        let mut arguments = Vec::new();
        while self.peek_is_one_of(PATTERN_START)? {
            let pattern = self.pattern()?;
            end = pattern.span().end();
            arguments.push(pattern);
        }

        let structure =
            pattern::Structure::new(parts, arguments, self.indicies.new_structure_pattern_id());
        let pattern = Located::new(Pattern::Structure(structure), Span::new(start, end));

        Ok(pattern)
    }

    fn pattern_grouping(&mut self) -> Result<Located<Pattern>> {
        let start = self.expect(Token::LeftParenthesis)?.span().start();
        let pattern = self.pattern()?;
        let end = self.expect(Token::RightParenthesis)?.span().end();
        let pattern = Located::new(pattern.into_data(), Span::new(start, end));

        Ok(pattern)
    }

    fn primary(&mut self) -> Result<Located<Expression>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::String(string) => self.literal(Expression::String(*string)),
            Token::Identifier(_) => self.path(),
            Token::LeftParenthesis => self.grouping(),
            unexpected => self.error(
                ParseError::UnexpectedTokenStart {
                    unexpected: *unexpected,
                    expected: PRIMARY_EXPRESSION_START,
                },
                token.span(),
            ),
        }
    }

    fn literal(&mut self, literal: Expression) -> Result<Located<Expression>> {
        let span = self.next().unwrap()?.span();
        Ok(Located::new(literal, span))
    }

    fn path(&mut self) -> Result<Located<Expression>> {
        let parts = self.path_parts()?;
        let span = parts.span();
        let path = expression::Path::new(
            parts,
            self.indicies.new_bound_id(),
            self.indicies.new_unique_name_id(),
        );
        let expression = Located::new(Expression::Path(path), span);

        Ok(expression)
    }

    fn grouping(&mut self) -> Result<Located<Expression>> {
        let start = self.expect(Token::LeftParenthesis)?.span().start();
        let expression = self.expression()?;
        let end = self.expect(Token::RightParenthesis)?.span().end();
        let expression = Located::new(expression.into_data(), Span::new(start, end));

        Ok(expression)
    }

    fn application(&mut self) -> Result<Located<Expression>> {
        let mut function = self.primary()?;
        let start = function.span().start();

        while self.peek_is_one_of(PRIMARY_EXPRESSION_START)? {
            let argument = self.primary()?;
            let end = argument.span().end();

            let application = expression::Application::new(function, argument);
            let application = Expression::Application(application);

            function = Located::new(application, Span::new(start, end));
        }

        Ok(function)
    }

    fn type_expression(&mut self) -> Result<Located<TypeExpression>> {
        self.type_primary()
    }

    fn type_primary(&mut self) -> Result<Located<TypeExpression>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::Identifier(_) => self.type_path(),
            Token::LeftParenthesis => self.type_grouping(),
            unexpected => self.error(
                ParseError::UnexpectedTokenStart {
                    unexpected: *unexpected,
                    expected: PRIMARY_TYPE_EXPRESSION_START,
                },
                token.span(),
            ),
        }
    }

    fn path_parts(&mut self) -> Result<Located<Vec<InternId>>> {
        let identifier = self.expect_identifier()?;
        let start = identifier.span().start();
        let mut end = identifier.span().end();
        let mut parts = vec![identifier.into_data()];

        while self.next_if_peek(Token::Dot)? {
            let part = self.expect_identifier()?;
            end = part.span().end();
            parts.push(part.into_data());
        }

        Ok(Located::new(parts, Span::new(start, end)))
    }

    fn type_path(&mut self) -> Result<Located<TypeExpression>> {
        let parts = self.path_parts()?;
        let span = parts.span();

        let path = type_expression::Path::new(parts, self.indicies.new_bound_id());
        let mut type_expression = Located::new(TypeExpression::Path(path), span);

        if self.next_if_peek(Token::LeftBracket)? {
            let mut arguments = Vec::new();
            while !self.peek_is(Token::RightBracket)? {
                let argument = self.type_primary()?;
                arguments.push(argument);
            }
            let end = self.expect(Token::RightBracket)?.span().end();

            let application = type_expression::Application::new(type_expression, arguments);

            type_expression = Located::new(
                TypeExpression::Application(application),
                Span::new(span.start(), end),
            );
        }

        Ok(type_expression)
    }

    fn type_grouping(&mut self) -> Result<Located<TypeExpression>> {
        let start = self.expect(Token::LeftParenthesis)?.span().start();
        let type_expression = self.type_expression()?;
        let end = self.expect(Token::RightParenthesis)?.span().end();
        let type_expression = Located::new(type_expression.into_data(), Span::new(start, end));

        Ok(type_expression)
    }

    pub fn module(&mut self) -> Result<Module> {
        let mut definitions = Vec::new();

        while self.peek().is_some() {
            definitions.push(self.definiton()?);
        }

        let module = definition::Module::new(definitions, self.source_name.to_string());

        Ok(module)
    }

    fn definiton(&mut self) -> Result<Definition> {
        let token = self.peek_some()?;

        match token.data() {
            Token::LetKeyword => self.name_definition(),
            Token::ModuleKeyword => self.module_definition(),
            Token::ImportKeyword => self.import_definition(),
            Token::StructureKeyword => self.structure_definition(),
            unexpected => self.error(
                ParseError::UnexpectedTokenStart {
                    unexpected: *unexpected,
                    expected: DEFINITION_START,
                },
                token.span(),
            ),
        }
    }

    fn name_definition(&mut self) -> Result<Definition> {
        self.expect(Token::LetKeyword)?;
        let identifier = self.expect_identifier()?;
        self.expect(Token::Equals)?;
        let expression = self.expression()?;

        let definiton = definition::Name::new(identifier, expression, self.indicies.new_path_id());

        Ok(Definition::Name(definiton))
    }

    fn module_definition(&mut self) -> Result<Definition> {
        self.expect(Token::ModuleKeyword)?;

        let parts = self.path_parts()?;
        let definition = definition::ModulePath::new(parts);

        Ok(Definition::ModulePath(definition))
    }

    fn import_definition(&mut self) -> Result<Definition> {
        self.expect(Token::ImportKeyword)?;
        Ok(Definition::Import(self.import()?))
    }

    fn import(&mut self) -> Result<definition::Import> {
        let parts = self.path_parts()?;

        let subimport = if self.next_if_peek(Token::LeftParenthesis)? {
            let mut imports = vec![];
            while !self.next_if_peek(Token::RightParenthesis)? {
                imports.push(self.import()?);
            }

            Some(definition::import::Subimport::Import(imports))
        } else if self.next_if_peek(Token::AsKeyword)? {
            Some(definition::import::Subimport::As(self.expect_identifier()?))
        } else {
            None
        };

        Ok(definition::Import::new(parts, subimport))
    }

    fn structure_definition(&mut self) -> Result<Definition> {
        self.expect(Token::StructureKeyword)?;
        let name = self.expect_identifier()?;

        let variables = if self.next_if_peek(Token::LeftBracket)? {
            let mut variables = Vec::new();
            while !self.next_if_peek(Token::RightBracket)? {
                variables.push(self.expect_identifier()?);
            }

            variables
        } else {
            Vec::new()
        };

        let mut constructors = Vec::new();
        while self.next_if_peek(Token::Bar)? {
            constructors.push(self.constructor()?);
        }

        let structure =
            definition::Structure::new(name, variables, constructors, self.indicies.new_path_id());

        Ok(Definition::Structure(structure))
    }

    fn constructor(&mut self) -> Result<Located<definition::structure::Constructor>> {
        let name = self.expect_identifier()?;
        let start = name.span().start();
        let mut end = name.span().end();

        let mut arguments = Vec::new();
        while self.peek_is_one_of(PRIMARY_TYPE_EXPRESSION_START)? {
            let primary = self.type_primary()?;
            end = primary.span().end();
            arguments.push(primary);
        }

        let constructor =
            definition::structure::Constructor::new(name, arguments, self.indicies.new_path_id());
        let constructor = Located::new(constructor, Span::new(start, end));

        Ok(constructor)
    }

    fn error<T>(&self, error: ParseError, span: Span) -> Result<T> {
        Err(ReportableError::new(
            error,
            span,
            self.source_name.to_string(),
        ))
    }
}

#[derive(Clone, Debug)]
pub enum ParseError {
    UnexpectedEOF,
    UnexpectedToken {
        unexpected: Token,
        expected: Token,
    },
    UnexpectedTokenStart {
        unexpected: Token,
        expected: &'static [Token],
    },
    IllFormedExpression,
}
