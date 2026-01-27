use std::fmt::Display;

use crate::interner::InternId;

#[derive(Clone, Copy)]
pub enum Token {
    Identifier(InternId),
    LeftParenthesis,
    RightParenthesis,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(id) => write!(f, "identifier: {id}"),
            Self::LeftParenthesis => write!(f, "("),
            Self::RightParenthesis => write!(f, ")"),
        }
    }
}