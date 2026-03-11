pub mod definition;
pub mod expression;
pub mod lex;
pub mod type_expression;

use std::iter::Peekable;

use crate::{
    error::{Result, eof_error, located_error},
    interner::{InternId, Interner},
    location::{Located, Span},
    resolution::Unresolved,
};

use lex::{Lexer, token::Token};

use expression::{
    ApplicationExpression, Expression, LambdaExpression, LetExpression, MatchBranch,
    MatchExpression, PathExpression, Pattern, StructurePattern,
};

use type_expression::{ApplicationTypeExpression, PathTypeExpression, TypeExpression};

use definition::{
    Constructor, Definition, ImportDefinition, ImportName, LetDefinition, ModuleDefinition,
    StructureDefinition,
};

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
    source_name: String,
}

impl<'source, 'interner> Parser<'source, 'interner> {
    pub fn from_source(
        source_name: String,
        source: &'source str,
        interner: &'interner mut Interner,
    ) -> Self {
        Self {
            tokens: Lexer::new(source_name.clone(), source, interner).peekable(),
            source_name,
        }
    }

    fn peek(&mut self) -> Option<Result<Located<Token>>> {
        self.tokens.peek().cloned()
    }

    fn next(&mut self) -> Option<Result<Located<Token>>> {
        self.tokens.next()
    }

    fn peek_some(&mut self) -> Result<Located<Token>> {
        self.peek().unwrap_or_else(|| {
            Err(eof_error(
                ParseError::UnexpectedEOF,
                self.source_name.clone(),
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
            let span = token.span();
            let error = ParseError::UnexpectedToken {
                unexpected: token.into_data(),
                expected: vec![expected],
            };
            Err(located_error(error, span, self.source_name.clone()))
        }
    }

    fn expect_identifier(&mut self) -> Result<Located<InternId>> {
        let token = self.expect(Token::Identifier(InternId::dummy()))?;
        let Token::Identifier(id) = token.data() else {
            unreachable!()
        };
        Ok(Located::new(*id, token.span()))
    }

    pub fn expression_repl(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let expression = self.expression()?;

        // NOTE: Maybe it is not needed
        if self.peek().is_some() {
            let error = ParseError::IllFormedExpression;
            return Err(eof_error(error, self.source_name.clone()));
        }

        Ok(expression)
    }

    pub fn expression(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::Backslash => self.lambda(),
            Token::LetKeyword => self.letin(),
            Token::MatchKeyword => self.matc(),
            _ => self.application(),
        }
    }

    fn lambda(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let start = self.expect(Token::Backslash)?.span().start();
        let variable = self.expect_identifier()?;
        let expression = self.expression()?;
        let end = expression.span().end();

        let lambda = LambdaExpression::<Unresolved>::new(variable, expression);
        let expression = Located::new(Expression::Lambda(lambda), Span::new(start, end));

        Ok(expression)
    }

    fn letin(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let start = self.expect(Token::LetKeyword)?.span().start();
        let variable = self.expect_identifier()?;
        self.expect(Token::Equals)?;
        let variable_expression = self.expression()?;
        self.expect(Token::InKeyword)?;
        let return_expression = self.expression()?;
        let end = return_expression.span().end();

        let letin =
            LetExpression::<Unresolved>::new(variable, variable_expression, return_expression);
        let expression = Located::new(Expression::Let(letin), Span::new(start, end));

        Ok(expression)
    }

    fn matc(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let start = self.expect(Token::MatchKeyword)?.span().start();
        let expression = self.expression()?;
        let mut end = expression.span().end();

        let mut branches = Vec::new();
        while self.next_if_peek(Token::Bar)? {
            let branch = self.match_branch()?;
            end = branch.span().end();
            branches.push(branch);
        }

        let matchlet = MatchExpression::new(expression, branches);
        let expression = Located::new(Expression::Match(matchlet), Span::new(start, end));

        Ok(expression)
    }

    fn match_branch(&mut self) -> Result<Located<MatchBranch<Unresolved>>> {
        let pattern = self.pattern()?;
        let start = pattern.span().start();
        self.expect(Token::Equals)?;
        let expression = self.expression()?;
        let end = expression.span().end();
        let branch = MatchBranch::new(pattern, expression);
        let branch = Located::new(branch, Span::new(start, end));

        Ok(branch)
    }

    fn pattern(&mut self) -> Result<Located<Pattern<Unresolved>>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::String(string) => self.pattern_literal(Pattern::String(*string)),
            Token::Tilde => self.pattern_any(),
            Token::Identifier(_) => self.pattern_structure(),
            Token::LeftParenthesis => self.pattern_grouping(),
            unexpected => {
                let error = ParseError::UnexpectedToken {
                    unexpected: *unexpected,
                    expected: PATTERN_START.to_vec(),
                };
                Err(located_error(error, token.span(), self.source_name.clone()))
            }
        }
    }

    fn pattern_literal(
        &mut self,
        literal: Pattern<Unresolved>,
    ) -> Result<Located<Pattern<Unresolved>>> {
        let span = self.next().unwrap()?.span();
        Ok(Located::new(literal, span))
    }

    fn pattern_any(&mut self) -> Result<Located<Pattern<Unresolved>>> {
        let start = self.expect(Token::Tilde)?.span().start();
        let identifier = self.expect_identifier()?;
        let end = identifier.span().end();
        let any = Pattern::Any(identifier.into_data());

        Ok(Located::new(any, Span::new(start, end)))
    }

    fn pattern_structure(&mut self) -> Result<Located<Pattern<Unresolved>>> {
        let parts = self.path_parts()?;
        let start = parts.span().start();
        let mut end = parts.span().end();
        let mut arguments = Vec::new();
        while self.peek_is_one_of(PATTERN_START)? {
            let pattern = self.pattern()?;
            end = pattern.span().end();
            arguments.push(pattern);
        }

        let structure = StructurePattern::<Unresolved>::new(parts, arguments);
        let pattern = Located::new(Pattern::Structure(structure), Span::new(start, end));

        Ok(pattern)
    }

    fn pattern_grouping(&mut self) -> Result<Located<Pattern<Unresolved>>> {
        let start = self.expect(Token::LeftParenthesis)?.span().start();
        let pattern = self.pattern()?;
        let end = self.expect(Token::RightParenthesis)?.span().end();
        let pattern = Located::new(pattern.into_data(), Span::new(start, end));

        Ok(pattern)
    }

    fn primary(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::String(string) => self.literal(Expression::String(*string)),
            Token::Identifier(_) => self.path(),
            Token::LeftParenthesis => self.grouping(),
            unexpected => {
                let error = ParseError::UnexpectedToken {
                    unexpected: *unexpected,
                    expected: PRIMARY_EXPRESSION_START.to_vec(),
                };
                Err(located_error(error, token.span(), self.source_name.clone()))
            }
        }
    }

    fn literal(
        &mut self,
        literal: Expression<Unresolved>,
    ) -> Result<Located<Expression<Unresolved>>> {
        let span = self.next().unwrap()?.span();
        Ok(Located::new(literal, span))
    }

    fn path(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let parts = self.path_parts()?;
        let span = parts.span();
        let path = PathExpression::new(parts);
        let expression = Located::new(Expression::Path(path), span);

        Ok(expression)
    }

    fn grouping(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let start = self.expect(Token::LeftParenthesis)?.span().start();
        let expression = self.expression()?;
        let end = self.expect(Token::RightParenthesis)?.span().end();
        let expression = Located::new(expression.into_data(), Span::new(start, end));

        Ok(expression)
    }

    fn application(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let mut function = self.primary()?;
        let start = function.span().start();

        while self.peek_is_one_of(PRIMARY_EXPRESSION_START)? {
            let argument = self.primary()?;
            let end = argument.span().end();
            let application = ApplicationExpression::new(function, argument);
            function = Located::new(Expression::Application(application), Span::new(start, end));
        }

        Ok(function)
    }

    fn type_expression(&mut self) -> Result<Located<TypeExpression<Unresolved>>> {
        self.type_primary()
    }

    fn type_primary(&mut self) -> Result<Located<TypeExpression<Unresolved>>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::Identifier(_) => self.type_path(),
            Token::LeftParenthesis => self.type_grouping(),
            unexpected => {
                let error = ParseError::UnexpectedToken {
                    unexpected: *unexpected,
                    expected: PRIMARY_TYPE_EXPRESSION_START.to_vec(),
                };
                Err(located_error(error, token.span(), self.source_name.clone()))
            }
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

    fn type_path(&mut self) -> Result<Located<TypeExpression<Unresolved>>> {
        let parts = self.path_parts()?;
        let span = parts.span();

        let path = PathTypeExpression::new(parts);
        let mut type_expression = Located::new(TypeExpression::Path(path), span);

        if self.next_if_peek(Token::LeftBracket)? {
            let mut arguments = Vec::new();
            while !self.peek_is(Token::RightBracket)? {
                let argument = self.type_primary()?;
                arguments.push(argument);
            }
            let end = self.expect(Token::RightBracket)?.span().end();

            let application = ApplicationTypeExpression::new(type_expression, arguments);
            type_expression = Located::new(
                TypeExpression::Application(application),
                Span::new(span.start(), end),
            );
        }

        Ok(type_expression)
    }

    fn type_grouping(&mut self) -> Result<Located<TypeExpression<Unresolved>>> {
        let start = self.expect(Token::LeftParenthesis)?.span().start();
        let type_expression = self.type_expression()?;
        let end = self.expect(Token::RightParenthesis)?.span().end();
        let type_expression = Located::new(type_expression.into_data(), Span::new(start, end));

        Ok(type_expression)
    }

    pub fn module(&mut self) -> Result<(Vec<Definition<Unresolved>>, String)> {
        let mut definitions = Vec::new();

        while self.peek().is_some() {
            definitions.push(self.definiton()?);
        }

        Ok((definitions, self.source_name.clone()))
    }

    fn definiton(&mut self) -> Result<Definition<Unresolved>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::LetKeyword => self.let_definiton(),
            Token::ModuleKeyword => self.module_definition(),
            Token::ImportKeyword => self.import_definition(),
            Token::StructureKeyword => self.structure_definition(),
            unexpected => {
                let error = ParseError::UnexpectedToken {
                    unexpected: *unexpected,
                    expected: DEFINITION_START.to_vec(),
                };
                Err(located_error(error, token.span(), self.source_name.clone()))
            }
        }
    }

    fn let_definiton(&mut self) -> Result<Definition<Unresolved>> {
        self.expect(Token::LetKeyword)?;
        let identifier = self.expect_identifier()?;
        self.expect(Token::Equals)?;
        let expression = self.expression()?;
        let definiton = LetDefinition::<Unresolved>::new(identifier, expression);

        Ok(Definition::Name(definiton))
    }

    fn module_definition(&mut self) -> Result<Definition<Unresolved>> {
        self.expect(Token::ModuleKeyword)?;

        let parts = self.path_parts()?;
        let definition = ModuleDefinition::new(parts);

        Ok(Definition::Module(definition))
    }

    fn import_definition(&mut self) -> Result<Definition<Unresolved>> {
        self.expect(Token::ImportKeyword)?;
        Ok(Definition::Import(self.import()?))
    }

    fn import(&mut self) -> Result<ImportDefinition> {
        let parts = self.path_parts()?;

        let import_name = if self.next_if_peek(Token::LeftParenthesis)? {
            let mut imports = vec![];
            while !self.next_if_peek(Token::RightParenthesis)? {
                imports.push(self.import()?);
            }

            Some(ImportName::Import(imports))
        } else if self.next_if_peek(Token::AsKeyword)? {
            Some(ImportName::As(self.expect_identifier()?.into_data()))
        } else {
            None
        };

        Ok(ImportDefinition::new(parts, import_name))
    }

    fn structure_definition(&mut self) -> Result<Definition<Unresolved>> {
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

        let structure = StructureDefinition::<Unresolved>::new(name, variables, constructors);
        Ok(Definition::Structure(structure))
    }

    fn constructor(&mut self) -> Result<Located<Constructor<Unresolved>>> {
        let name = self.expect_identifier()?;
        let start = name.span().start();
        let mut end = name.span().end();

        let mut arguments = Vec::new();
        while self.peek_is_one_of(PRIMARY_TYPE_EXPRESSION_START)? {
            let primary = self.type_primary()?;
            end = primary.span().end();
            arguments.push(primary);
        }

        let constructor = Constructor::<Unresolved>::new(name, arguments);
        let constructor = Located::new(constructor, Span::new(start, end));

        Ok(constructor)
    }
}

#[derive(Clone, Debug)]
pub enum ParseError {
    UnexpectedEOF,
    UnexpectedToken {
        unexpected: Token,
        expected: Vec<Token>,
    },
    IllFormedExpression,
}
