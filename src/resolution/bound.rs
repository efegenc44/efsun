use std::fmt::{Debug, Display};

#[derive(Clone, Copy, Debug)]
pub enum Bound {
    Local(BoundId),
    Capture(BoundId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BoundId(usize);

impl BoundId {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn value(&self) -> usize {
        self.0
    }
}

impl Display for Bound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(id) => write!(f, "{}", id.0),
            Self::Capture(id) => write!(f, "captured({})", id.0),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Capture {
    Local(BoundId),
    Outer(BoundId)
}

impl Display for Capture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(id) => write!(f, "local({})", id.0),
            Self::Outer(id) => write!(f, "outer({})", id.0),
        }
    }
}

impl Debug for Capture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
