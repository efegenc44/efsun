use std::fmt::Display;

use crate::interner::InternId;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(InternId),
    LeftParenthesis,
    RightParenthesis,
    Backslash,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(id) => write!(f, "identifier: {id}"),
            Self::LeftParenthesis => write!(f, "("),
            Self::RightParenthesis => write!(f, ")"),
            Self::Backslash => write!(f, "\\"),
        }
    }
}