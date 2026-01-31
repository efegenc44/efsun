use std::{iter::Peekable, str::Chars};

use crate::{
    interner::Interner,
    token::Token,
    location::{Located, SourceLocation},
    error::{Result, located_error}
};

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

    fn keyword_or_identifier(&mut self) -> Located<Token> {
        let start = self.location;
        let mut lexeme = String::new();
        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() {
                lexeme.push(self.next().unwrap());
            } else {
                break;
            }
        }
        let end = self.location;

        let token = match lexeme.as_str() {
            "let" => Token::LetKeyword,
            "in" => Token::InKeyword,
            _ => {
                let id = self.interner.intern(lexeme);
                Token::Identifier(id)
            }
        };

        Located::new(token, start, end)
    }

    fn single(&mut self, token: Token) -> Located<Token> {
        let start = self.location;
        self.next();
        let end = self.location;

        Located::new(token, start, end)
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
    type Item = Result<Located<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let token = match self.peek()? {
            ch if ch.is_alphabetic() => self.keyword_or_identifier(),
            '(' => self.single(Token::LeftParenthesis),
            ')' => self.single(Token::RightParenthesis),
            '\\' => self.single(Token::Backslash),
            '=' => self.single(Token::Equals),
            unknown => {
                let start = self.location;
                self.next();
                let end = self.location;

                let error = LexError::UnknownStartOfAToken(unknown);
                return Some(Err(located_error(error, start, end)));
            }
        };

        Some(Ok(token))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum LexError {
    UnknownStartOfAToken(char)
}
