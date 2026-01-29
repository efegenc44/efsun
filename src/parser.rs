use std::iter::Peekable;

use crate::{
    expression::{
        ApplicationExpression, Expression, Unresolved,
        LambdaExpression, IdentifierExpression
    },
    interner::InternId,
    lexer::{LexError, Lexer},
    location::{Located, SourceLocation},
    token::Token
};

pub struct Parser<'source, 'interner> {
    tokens: Peekable<Lexer<'source, 'interner>>
}

impl<'source, 'interner> Parser<'source, 'interner> {
    pub fn new(lexer: Lexer<'source, 'interner>) -> Self {
        Self {
            tokens: lexer.peekable()
        }
    }

    fn peek(&mut self) -> Option<ParseResult<Token>> {
        self
            .tokens
            .peek()
            .map(|result| result.map_err(Into::into))
    }

    fn next(&mut self) -> Option<ParseResult<Token>> {
        self
            .tokens
            .next()
            .map(|result| result.map_err(Into::into))
    }

    fn peek_some(&mut self) -> ParseResult<Token> {
        self
            .peek()
            .unwrap_or_else(|| {
                let error = Located::new(
                    ParseError::UnexpectedEOF,
                    SourceLocation::eof()
                );

                Err(error)
            })
    }

    fn next_some(&mut self) -> ParseResult<Token> {
        self.peek_some()?;
        self.next().unwrap()
    }

    fn expect(&mut self, expected: Token) -> ParseResult<Token> {
        let token = self.next_some()?;

        if std::mem::discriminant(&expected) == std::mem::discriminant(token.data()) {
            Ok(token)
        } else {
            let error = Located::new(
                ParseError::UnexpectedToken(*token.data()),
                token.location()
            );

            Err(error)
        }
    }

    fn expect_identifier(&mut self) -> ParseResult<InternId> {
        let token = self.expect(Token::Identifier(InternId::dummy()))?;
        let Token::Identifier(id) = token.data() else { unreachable!() };
        Ok(Located::new(*id, token.location()))
    }

    pub fn expression(&mut self) -> ParseResult<Expression<Unresolved>> {
        // TODO: Check if all tokens are consumed
        let token = self.peek_some()?;

        match token.data() {
            Token::Backslash => self.lambda(),
            _ => self.application()
        }
    }

    fn lambda(&mut self) -> ParseResult<Expression<Unresolved>> {
        let location = self.expect(Token::Backslash)?.location();
        let variable = self.expect_identifier()?;
        let expression = self.expression()?;

        let lambda = LambdaExpression::new(variable, expression);
        let expression = Located::new(Expression::Lambda(lambda), location);

        Ok(expression)
    }

    fn primary(&mut self) -> ParseResult<Expression<Unresolved>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::Identifier(_) => self.identifier(),
            Token::LeftParenthesis => self.grouping(),
            unexpected => {
                let error = Located::new(
                    ParseError::UnexpectedToken(*unexpected),
                    token.location()
                );

                Err(error)
            }
        }
    }

    fn identifier(&mut self) -> ParseResult<Expression<Unresolved>> {
        let identifier = self.expect_identifier()?;
        let location = identifier.location();

        let identifier = IdentifierExpression::new(identifier);
        let expression = Located::new(Expression::Identifier(identifier), location);

        Ok(expression)
    }

    fn grouping(&mut self) -> ParseResult<Expression<Unresolved>> {
        let location = self.expect(Token::LeftParenthesis)?.location();
        let expression = self.expression()?;
        self.expect(Token::RightParenthesis)?;
        let expression = Located::new(expression.destruct().0, location);

        Ok(expression)
    }

    fn application(&mut self) -> ParseResult<Expression<Unresolved>> {
        let mut function = self.primary()?;
        let location = function.location();

        loop {
            let argument = match self.primary() {
                Ok(expression) => expression,
                Err(error) => {
                    match error.data() {
                        ParseError::UnexpectedEOF |
                        ParseError::UnexpectedToken(_) => break,
                        _ => return Err(error),
                    }
                }
            };

            let application = ApplicationExpression::new(function, argument);
            function = Located::new(Expression::Application(application), location);
        }

        Ok(function)
    }
}

#[allow(unused)]
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
            value.location()
        )
    }
}

type ParseResult<T> = Result<Located<T>, Located<ParseError>>;