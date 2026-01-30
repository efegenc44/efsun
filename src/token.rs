use crate::interner::{Interner, InternId};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(InternId, usize),
    LeftParenthesis,
    RightParenthesis,
    Backslash,
}

impl Token {
    pub fn display<'interner>(&self, interner: &'interner Interner) -> &'interner str {
        match self {
            Self::Identifier(id, _) => interner.lookup(*id),
            Self::LeftParenthesis => "(",
            Self::RightParenthesis => ")",
            Self::Backslash => "\\",
        }
    }

    pub fn length(&self) -> usize {
        match self {
            Self::Identifier(_, length) => *length,
            Self::LeftParenthesis |
            Self::RightParenthesis |
            Self::Backslash => 1
        }
    }
}
