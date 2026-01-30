use crate::interner::{Interner, InternId};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(InternId),
    LeftParenthesis,
    RightParenthesis,
    Backslash,
}

impl Token {
    pub fn display<'interner>(&self, interner: &'interner Interner) -> &'interner str {
        match self {
            Self::Identifier(id) => interner.lookup(*id),
            Self::LeftParenthesis => "(",
            Self::RightParenthesis => ")",
            Self::Backslash => "\\",
        }
    }
}
