use std::iter::Peekable;

use crate::{
    expression::{
        ApplicationExpression, Expression, Unresolved,
        LambdaExpression, IdentifierExpression, LetExpression
    },
    interner::InternId,
    lexer::{LexError, Lexer},
    location::{Located, SourceLocation},
    token::Token,
    error::{Error, Result, located_error}
};

pub struct Parser<'source, 'interner> {
    tokens: Peekable<Lexer<'source, 'interner>>,
}

impl<'source, 'interner> Parser<'source, 'interner> {
    pub fn new(lexer: Lexer<'source, 'interner>) -> Self {
        Self { tokens: lexer.peekable() }
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
            .unwrap_or_else(|| {
                Err(located_error(
                    ParseError::UnexpectedEOF,
                    SourceLocation::eof(),
                    SourceLocation::eof()
                ))
            })
    }

    fn next_some(&mut self) -> Result<Located<Token>> {
        self.peek_some()?;
        self.next().unwrap()
    }

    fn expect(&mut self, expected: Token) -> Result<Located<Token>> {
        let token = self.next_some()?;

        if std::mem::discriminant(&expected) == std::mem::discriminant(token.data()) {
            Ok(token)
        } else {
            let error = ParseError::UnexpectedToken(*token.data());
            Err(located_error(error, token.start(), token.end()))
        }
    }

    fn expect_identifier(&mut self) -> Result<Located<InternId>> {
        let token = self.expect(Token::Identifier(InternId::dummy()))?;
        let Token::Identifier(id) = token.data() else { unreachable!() };
        Ok(Located::new(*id, token.start(), token.end()))
    }

    pub fn expression(&mut self) -> Result<Located<Expression<Unresolved>>> {
        // TODO: Check if all tokens are consumed
        let token = self.peek_some()?;

        match token.data() {
            Token::Backslash => self.lambda(),
            Token::LetKeyword => self.letin(),
            _ => self.application()
        }
    }

    fn lambda(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let start = self.expect(Token::Backslash)?.start();
        let variable = self.expect_identifier()?;
        let expression = self.expression()?;
        let end = expression.end();

        let lambda = LambdaExpression::<Unresolved>::new(variable, expression);
        let expression = Located::new(Expression::Lambda(lambda), start, end);

        Ok(expression)
    }

    fn letin(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let start = self.expect(Token::LetKeyword)?.start();
        let variable = self.expect_identifier()?;
        self.expect(Token::Equals)?;
        let variable_expression = self.expression()?;
        self.expect(Token::InKeyword)?;
        let return_expression = self.expression()?;
        let end = return_expression.end();

        let letin = LetExpression::<Unresolved>::new(variable, variable_expression, return_expression);
        let expression = Located::new(Expression::Let(letin), start, end);

        Ok(expression)
    }

    fn primary(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::Identifier(_) => self.identifier(),
            Token::LeftParenthesis => self.grouping(),
            unexpected => {
                let error = ParseError::UnexpectedToken(*unexpected);
                Err(located_error(error, token.start(), token.end()))
            }
        }
    }

    fn identifier(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let identifier = self.expect_identifier()?;
        let start = identifier.start();
        let end = identifier.end();

        let identifier = IdentifierExpression::new(identifier);
        let expression = Located::new(Expression::Identifier(identifier), start, end);

        Ok(expression)
    }

    fn grouping(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let start = self.expect(Token::LeftParenthesis)?.start();
        let expression = self.expression()?;
        let end = self.expect(Token::RightParenthesis)?.end();
        let expression = Located::new(expression.destruct().0, start, end);

        Ok(expression)
    }

    fn application(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let mut function = self.primary()?;
        let start = function.start();

        loop {
            let argument = match self.primary() {
                Ok(expression) => expression,
                Err(error) => {
                    match error.data() {
                        Error::Parse(ParseError::UnexpectedEOF) |
                        Error::Parse(ParseError::UnexpectedToken(_)) => break,
                        _ => return Err(error),
                    }
                }
            };

            let end = argument.end();
            let application = ApplicationExpression::new(function, argument);
            function = Located::new(Expression::Application(application), start, end);
        }

        Ok(function)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ParseError {
    LexError(LexError),
    UnexpectedEOF,
    UnexpectedToken(Token),
}

impl From<Located<LexError>> for Located<ParseError> {
    fn from(value: Located<LexError>) -> Self {
        Located::new(
            ParseError::LexError(*value.data()),
            value.start(),
            value.end()
        )
    }
}
