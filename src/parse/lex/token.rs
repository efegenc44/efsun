use crate::interner::{Interner, InternId};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(InternId),
    String(InternId),
    LeftParenthesis,
    RightParenthesis,
    Backslash,
    Equals,
    Dot,
    LetKeyword,
    InKeyword,
    ModuleKeyword,
}

impl Token {
    pub fn display<'interner>(&self, interner: &'interner Interner) -> &'interner str {
        match self {
            Self::Identifier(id) => interner.lookup(*id),
            Self::String(_id) => todo!(), // format!("\"{}\"", interner.lookup(*id)),
            Self::LeftParenthesis => "(",
            Self::RightParenthesis => ")",
            Self::Backslash => "\\",
            Self::Equals => "=",
            Self::Dot => ".",
            Self::LetKeyword => "let",
            Self::InKeyword => "in",
            Self::ModuleKeyword => "module",
        }
    }
}
