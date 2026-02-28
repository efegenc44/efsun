pub mod token;

use std::{iter::Peekable, str::Chars};

use crate::{
    interner::Interner,
    location::{Located, SourceLocation, Span},
    error::{Result, located_error}
};

use token::Token;

pub struct Lexer<'source, 'interner> {
    chars: Peekable<Chars<'source>>,
    source_name: String,
    interner: &'interner mut Interner,
    location: SourceLocation,
}

impl<'source, 'interner> Lexer<'source, 'interner> {
    pub fn new(source_name: String, source: &'source str, interner: &'interner mut Interner) -> Self {
        Self {
            chars: source.chars().peekable(),
            source_name,
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
            "module" => Token::ModuleKeyword,
            "import" => Token::ImportKeyword,
            "as" => Token::AsKeyword,
            "match" => Token::MatchKeyword,
            "structure" => Token::StructureKeyword,
            _ => {
                let id = self.interner.intern(lexeme);
                Token::Identifier(id)
            }
        };

        Located::new(token, Span::new(start, end))
    }

    fn string(&mut self) -> Result<Located<Token>> {
        let start = self.location;
        self.next(); // Starting `"`
        let mut string = String::new();
        while let Some(ch) = self.peek() {
            if ch == '"' {
                break;
            } else {
                string.push(self.next().unwrap());
            }
        }

        let Some('"') = self.next() else {
            let end = {
                let mut end = start;
                end.increment();
                end
            };

            let error = LexError::UnterminatedStringLiteral;
            return Err(located_error(error, Span::new(start, end), self.source_name.clone()));
        };

        let end = self.location;

        let string = Token::String(self.interner.intern(string));
        Ok(Located::new(string, Span::new(start, end)))
    }

    fn single(&mut self, token: Token) -> Located<Token> {
        let start = self.location;
        self.next();
        let end = self.location;

        Located::new(token, Span::new(start, end))
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
            '"' => {
                match self.string() {
                    Ok(token) => token,
                    Err(error) => return Some(Err(error)),
                }
            },
            '(' => self.single(Token::LeftParenthesis),
            ')' => self.single(Token::RightParenthesis),
            '[' => self.single(Token::LeftBracket),
            ']' => self.single(Token::RightBracket),
            '\\' => self.single(Token::Backslash),
            '=' => self.single(Token::Equals),
            '.' => self.single(Token::Dot),
            '|' => self.single(Token::Bar),
            '~' => self.single(Token::Tilde),
            unknown => {
                let start = self.location;
                self.next();
                let end = self.location;

                let error = LexError::UnknownStartOfAToken(unknown);
                return Some(Err(located_error(error, Span::new(start, end), self.source_name.clone())));
            }
        };

        Some(Ok(token))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum LexError {
    UnknownStartOfAToken(char),
    UnterminatedStringLiteral,
}
