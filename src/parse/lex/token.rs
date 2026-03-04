use std::fmt::Display;

use crate::interner::{Interner, InternId, WithInterner};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(InternId),
    String(InternId),
    LeftParenthesis,
    RightParenthesis,
    LeftBracket,
    RightBracket,
    Backslash,
    Equals,
    Dot,
    Bar,
    Tilde,
    LetKeyword,
    InKeyword,
    ModuleKeyword,
    ImportKeyword,
    AsKeyword,
    MatchKeyword,
    StructureKeyword,
}

impl Display for WithInterner<'_, &Token> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            Token::Identifier(id) => write!(f, "{}", self.interner().lookup(id)),
            Token::String(string) => write!(f, "\"{}\"", self.interner().lookup(string)),
            Token::LeftParenthesis => write!(f, "("),
            Token::RightParenthesis => write!(f, ")"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::Backslash => write!(f, "\\"),
            Token::Equals => write!(f, "="),
            Token::Dot => write!(f, "."),
            Token::Bar => write!(f, "|"),
            Token::Tilde => write!(f, "~"),
            Token::LetKeyword => write!(f, "let"),
            Token::InKeyword => write!(f, "in"),
            Token::ModuleKeyword => write!(f, "module"),
            Token::ImportKeyword => write!(f, "import"),
            Token::AsKeyword => write!(f, "as"),
            Token::MatchKeyword => write!(f, "match"),
            Token::StructureKeyword => write!(f, "structure"),
        }
    }
}

impl Token {
    pub fn display<'interner>(&self, interner: &'interner Interner) -> &'interner str {
        match self {
            Self::Identifier(id) => interner.lookup(id),
            Self::String(_id) => todo!(), // format!("\"{}\"", interner.lookup(*id)),
            Self::LeftParenthesis => "(",
            Self::RightParenthesis => ")",
            Self::LeftBracket => "[",
            Self::RightBracket => "]",
            Self::Backslash => "\\",
            Self::Equals => "=",
            Self::Dot => ".",
            Self::Bar => "|",
            Self::Tilde => "~",
            Self::LetKeyword => "let",
            Self::InKeyword => "in",
            Self::ModuleKeyword => "module",
            Self::ImportKeyword => "import",
            Self::AsKeyword => "as",
            Self::MatchKeyword => "match",
            Self::StructureKeyword => "structure",
        }
    }
}
