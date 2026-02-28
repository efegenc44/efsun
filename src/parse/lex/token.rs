use crate::interner::{Interner, InternId};

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

impl Token {
    pub fn display<'interner>(&self, interner: &'interner Interner) -> &'interner str {
        match self {
            Self::Identifier(id) => interner.lookup(*id),
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
