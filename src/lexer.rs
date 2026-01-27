use std::{iter::Peekable, str::Chars};

use crate::{interner::Interner, token::Token, location::{Located, SourceLocation}};

pub struct Lexer<'source, 'interner> {
    chars: Peekable<Chars<'source>>,
    interner: &'interner mut Interner,
    location: SourceLocation,
}

impl<'source, 'interner> Lexer<'source, 'interner> {
    pub fn new(source: &'source str, interner: &'interner mut Interner) -> Self {
        Self {
            chars: source.chars().peekable(),
            interner,
            location: SourceLocation::start()
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.chars.next()?;

        if ch == '\n' {
            self.location.newline();
        } else {
            self.location.increment();
        }

        Some(ch)
    }

    fn identifier(&mut self) -> Located<Token> {
        let location = self.location;

        let mut lexeme = String::new();
        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() {
                lexeme.push(self.next().unwrap());
            } else {
                break;
            }
        }

        let id = self.interner.intern(lexeme);
        Located::new(Token::Identifier(id), location)
    }

    fn single(&mut self, token: Token) -> Located<Token> {
        let location = self.location;

        self.next();
        Located::new(token, location)
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.next();
            } else {
                return;
            }
        }
    }
}

impl<'source, 'interner> Iterator for Lexer<'source, 'interner> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let token = match self.peek()? {
            ch if ch.is_alphabetic() => self.identifier(),
            '(' => self.single(Token::LeftParenthesis),
            ')' => self.single(Token::RightParenthesis),
            unknown => {
                let location = self.location;
                let error = Located::new(
                    LexError::UnknownStartOfAToken(unknown),
                    location
                );
                self.next();
                return Some(Err(error))
            }
        };

        Some(Ok(token))
    }
}

#[derive(Debug)]
pub enum LexError {
    UnknownStartOfAToken(char)
}

type LexResult = Result<Located<Token>, Located<LexError>>;