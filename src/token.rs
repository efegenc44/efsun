use crate::interner::{Interner, InternId};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(InternId),
    LeftParenthesis,
    RightParenthesis,
    Backslash,
    Equals,
    LetKeyword,
    InKeyword,
}

impl Token {
    pub fn display<'interner>(&self, interner: &'interner Interner) -> &'interner str {
        match self {
            Self::Identifier(id) => interner.lookup(*id),
            Self::LeftParenthesis => "(",
            Self::RightParenthesis => ")",
            Self::Backslash => "\\",
            Self::Equals => "=",
            Self::LetKeyword => "let",
            Self::InKeyword => "in",
        }
    }
}
