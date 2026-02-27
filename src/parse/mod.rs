pub mod expression;
pub mod lex;
pub mod definition;

use std::iter::Peekable;

use crate::{
    error::{Error, Result, located_error, eof_error},
    interner::{Interner, InternId},
    location::{Located, Span},
    resolution::Unresolved,
};

use lex::{Lexer, token::Token};

use expression::{
    ApplicationExpression, Expression, LambdaExpression, PathExpression,
    LetExpression, MatchExpression, MatchBranch, Pattern
};

use definition::{
    Definition, ModuleDefinition, NameDefinition, ImportDefinition,
    ImportName
};

pub struct Parser<'source, 'interner> {
    tokens: Peekable<Lexer<'source, 'interner>>,
    source_name: String,
}

impl<'source, 'interner> Parser<'source, 'interner> {
    pub fn from_source(source_name: String, source: &'source str, interner: &'interner mut Interner) -> Self {
        Self {
            tokens: Lexer::new(source_name.clone(), source, interner).peekable(),
            source_name
        }
    }

    fn peek(&mut self) -> Option<Result<Located<Token>>> {
        self.tokens.peek().cloned()
    }

    fn next(&mut self) -> Option<Result<Located<Token>>> {
        self.tokens.next()
    }

    fn peek_some(&mut self) -> Result<Located<Token>> {
        self
            .peek()
            .unwrap_or_else(|| Err(eof_error(ParseError::UnexpectedEOF, self.source_name.clone())))
    }

    fn next_some(&mut self) -> Result<Located<Token>> {
        self.peek_some()?;
        self.next().unwrap()
    }

    fn next_if_peek(&mut self, expected: Token) -> Result<bool> {
        let Some(peek) = self.peek() else {
            return Ok(false);
        };
        let peek = peek?;

        if std::mem::discriminant(&expected) == std::mem::discriminant(peek.data()) {
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
            let error = ParseError::UnexpectedToken(*token.data());
            Err(located_error(error, token.span(), self.source_name.clone()))
        }
    }

    fn expect_identifier(&mut self) -> Result<Located<InternId>> {
        let token = self.expect(Token::Identifier(InternId::dummy()))?;
        let Token::Identifier(id) = token.data() else { unreachable!() };
        Ok(Located::new(*id, token.span()))
    }

    pub fn expression(&mut self) -> Result<Located<Expression<Unresolved>>> {
        // TODO: Check if all tokens are consumed
        let token = self.peek_some()?;

        match token.data() {
            Token::Backslash => self.lambda(),
            Token::LetKeyword => self.letin(),
            Token::MatchKeyword => self.matchlet(),
            _ => self.application()
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

        let letin = LetExpression::<Unresolved>::new(variable, variable_expression, return_expression);
        let expression = Located::new(Expression::Let(letin), Span::new(start, end));

        Ok(expression)
    }

    fn matchlet(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let start = self.expect(Token::MatchKeyword)?.span().start();
        let expression = self.expression()?;
        let mut end = expression.span().end();

        let mut branches = Vec::new();
        while self.next_if_peek(Token::LetKeyword)? {
            let pattern = self.pattern()?;
            let branch_start = pattern.span().start();
            self.expect(Token::Equals)?;
            let branch_expression = self.expression()?;
            end = branch_expression.span().end();
            let branch = Located::new(MatchBranch::new(pattern, branch_expression), Span::new(branch_start, end));
            branches.push(branch);
        }

        let matchlet = MatchExpression::new(expression, branches);
        let expression = Located::new(Expression::Match(matchlet), Span::new(start, end));

        Ok(expression)
    }

    fn pattern(&mut self) -> Result<Located<Pattern>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::String(string) => {
                self.next();
                let literal = Pattern::String(*string);
                Ok(Located::new(literal, token.span()))
            },
            Token::Identifier(id) => {
                self.next();
                let literal = Pattern::Any(*id);
                Ok(Located::new(literal, token.span()))
            },
            unexpected => {
                let error = ParseError::UnexpectedToken(*unexpected);
                Err(located_error(error, token.span(), self.source_name.clone()))
            }
        }
    }

    fn primary(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::String(string) => {
                self.next();
                let literal = Expression::String(*string);
                Ok(Located::new(literal, token.span()))
            },
            Token::Identifier(_) => self.path(),
            Token::LeftParenthesis => self.grouping(),
            unexpected => {
                let error = ParseError::UnexpectedToken(*unexpected);
                Err(located_error(error, token.span(), self.source_name.clone()))
            }
        }
    }

    fn path(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let identifier = self.expect_identifier()?;
        let start = identifier.span().start();
        let mut end = identifier.span().end();
        let mut parts = vec![*identifier.data()];

        while self.next_if_peek(Token::Dot)? {
            let part = self.expect_identifier()?;
            parts.push(*part.data());
            end = part.span().end();
        }

        let path = PathExpression::new(Located::new(parts, Span::new(start, end)));
        let expression = Located::new(Expression::Path(path), Span::new(start, end));

        Ok(expression)
    }

    fn grouping(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let start = self.expect(Token::LeftParenthesis)?.span().start();
        let expression = self.expression()?;
        let end = self.expect(Token::RightParenthesis)?.span().end();
        let expression = Located::new(expression.destruct().0, Span::new(start, end));

        Ok(expression)
    }

    fn application(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let mut function = self.primary()?;
        let start = function.span().start();

        loop {
            let argument = match self.primary() {
                Ok(expression) => expression,
                Err((error, source_name)) => {
                    match error.data() {
                        Error::Parse(ParseError::UnexpectedEOF) |
                        Error::Parse(ParseError::UnexpectedToken(_)) => break,
                        _ => return Err((error, source_name)),
                    }
                }
            };

            let end = argument.span().end();
            let application = ApplicationExpression::new(function, argument);
            function = Located::new(Expression::Application(application), Span::new(start, end));
        }

        Ok(function)
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
            unexpected => {
                let error = ParseError::UnexpectedToken(*unexpected);
                Err(located_error(error, token.span(), self.source_name.clone()))
            }
        }
    }

    fn let_definiton(&mut self) -> Result<Definition<Unresolved>> {
        self.expect(Token::LetKeyword)?;
        let identifier = self.expect_identifier()?;
        self.expect(Token::Equals)?;
        let expression = self.expression()?;
        let definiton = NameDefinition::<Unresolved>::new(identifier, expression);

        Ok(Definition::Name(definiton))
    }

    fn module_definition(&mut self) -> Result<Definition<Unresolved>> {
        self.expect(Token::ModuleKeyword)?;
        let identifier = self.expect_identifier()?;
        let start = identifier.span().start();
        let mut end = identifier.span().end();
        let mut parts = vec![*identifier.data()];
        while self.next_if_peek(Token::Dot)? {
            let part = self.expect_identifier()?;
            parts.push(*part.data());
            end = part.span().end();
        }

        let parts = Located::new(parts, Span::new(start, end));
        let definition = ModuleDefinition::new(parts);

        Ok(Definition::Module(definition))
    }

    fn import_definition(&mut self) -> Result<Definition<Unresolved>> {
        self.expect(Token::ImportKeyword)?;
        Ok(Definition::Import(self.import()?))
    }

    fn import(&mut self) -> Result<ImportDefinition> {
        let identifier = self.expect_identifier()?;
        let mut parts = vec![*identifier.data()];

        while self.next_if_peek(Token::Dot)? {
            let part = self.expect_identifier()?;
            parts.push(*part.data());
        }

        let import_name = if self.next_if_peek(Token::LeftParenthesis)? {
            let mut imports = vec![];
            imports.push(self.import()?);

            while !self.next_if_peek(Token::RightParenthesis)? {
                imports.push(self.import()?);
            }

            Some(ImportName::Import(imports))
        } else if self.next_if_peek(Token::AsKeyword)? {
            Some(ImportName::As(*self.expect_identifier()?.data()))
        } else {
            None
        };

        Ok(ImportDefinition::new(parts, import_name))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ParseError {
    UnexpectedEOF,
    UnexpectedToken(Token),
}
