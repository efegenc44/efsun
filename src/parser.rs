use std::iter::Peekable;

use crate::{
    expression::{
        ApplicationExpression, Expression, Unresolved,
        LambdaExpression, IdentifierExpression
    },
    interner::InternId,
    lexer::{LexError, Lexer},
    location::{Located, SourceLocation},
    token::Token,
    error::{Error, Result}
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
                let error: Error = ParseError::UnexpectedEOF.into();
                let error = error.span(
                    SourceLocation::eof(),
                    SourceLocation::eof()
                );

                Err(error)
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
            let start = token.location();
            let end = start.add(token.data().length());

            let error: Error = ParseError::UnexpectedToken(*token.data()).into();
            let error = error.span(start, end);

            Err(error)
        }
    }

    fn expect_identifier(&mut self) -> Result<(Located<InternId>, usize)> {
        let token = self.expect(Token::Identifier(InternId::dummy(), 0))?;
        let Token::Identifier(id, length) = token.data() else { unreachable!() };
        Ok((Located::new(*id, token.location()), *length))
    }

    pub fn expression(&mut self) -> Result<Located<Expression<Unresolved>>> {
        // TODO: Check if all tokens are consumed
        let token = self.peek_some()?;

        match token.data() {
            Token::Backslash => self.lambda(),
            _ => self.application()
        }
    }

    fn lambda(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let location = self.expect(Token::Backslash)?.location();
        let (variable, _) = self.expect_identifier()?;
        let expression = self.expression()?;

        let lambda = LambdaExpression::new(variable, expression);
        let expression = Located::new(Expression::Lambda(lambda), location);

        Ok(expression)
    }

    fn primary(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let token = self.peek_some()?;

        match token.data() {
            Token::Identifier(_, _) => self.identifier(),
            Token::LeftParenthesis => self.grouping(),
            unexpected => {
                let start = token.location();
                let end = start.add(token.data().length());

                let error: Error = ParseError::UnexpectedToken(*unexpected).into();
                let error = error.span(start, end);

                Err(error)
            }
        }
    }

    fn identifier(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let (identifier, length) = self.expect_identifier()?;
        let location = identifier.location();

        let identifier = IdentifierExpression::new(identifier, length);
        let expression = Located::new(Expression::Identifier(identifier), location);

        Ok(expression)
    }

    fn grouping(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let location = self.expect(Token::LeftParenthesis)?.location();
        let expression = self.expression()?;
        self.expect(Token::RightParenthesis)?;
        let expression = Located::new(expression.destruct().0, location);

        Ok(expression)
    }

    fn application(&mut self) -> Result<Located<Expression<Unresolved>>> {
        let mut function = self.primary()?;
        let location = function.location();

        loop {
            let argument = match self.primary() {
                Ok(expression) => expression,
                Err(error) => {
                    match error.error() {
                        Error::Parse(ParseError::UnexpectedEOF) |
                        Error::Parse(ParseError::UnexpectedToken(_)) => break,
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
