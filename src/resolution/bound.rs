use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

use crate::{
    interner::{InternId, WithInterner},
    location::Located,
};

#[derive(Clone, Debug)]
pub enum Bound {
    Local(BoundId),
    Capture(BoundId),
    Absolute(Path),
}

impl<'interner> Display for WithInterner<'interner, &Bound> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;

        match &self.data {
            Bound::Local(id) => write!(f, "{}", id.0),
            Bound::Capture(id) => write!(f, "captured({})", id.0),
            Bound::Absolute(path) => {
                let path = WithInterner {
                    data: path,
                    interner,
                };

                write!(f, "{}", path)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BoundId(usize);

impl BoundId {
    pub(super) fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn value(&self) -> usize {
        self.0
    }
}

/// Hint for self capturing local let in definition
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SelfCaptureHint {
    Possible,
    No,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Capture {
    Local(BoundId, SelfCaptureHint),
    Outer(BoundId, SelfCaptureHint),
}

impl Capture {
    pub fn self_capture_hint(&self) -> SelfCaptureHint {
        match self {
            Capture::Local(_, hint) => *hint,
            Capture::Outer(_, hint) => *hint,
        }
    }
}

impl Display for Capture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(id, _) => write!(f, "local({})", id.0),
            Self::Outer(id, _) => write!(f, "outer({})", id.0),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Path(Vec<InternId>);

impl Path {
    pub fn from_parts(parts: impl Into<Vec<InternId>>) -> Self {
        Self(parts.into())
    }

    pub fn pop(&mut self) -> InternId {
        self.0.pop().unwrap()
    }

    pub fn push<I>(&mut self, identifiers: I)
    where
        I: IntoIterator,
        I::Item: AsRef<InternId>,
        Vec<InternId>: Extend<<I as IntoIterator>::Item>,
    {
        self.0.extend(identifiers);
    }

    pub fn append<I>(&self, identifiers: I) -> Self
    where
        I: IntoIterator,
        I::Item: AsRef<InternId>,
        Vec<InternId>: Extend<<I as IntoIterator>::Item>,
    {
        let mut clone = self.clone();
        clone.0.extend(identifiers);
        clone
    }
}

impl<'interner> Display for WithInterner<'interner, &Path> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;

        match self.data.0.as_slice() {
            [] => unreachable!(),
            [x, xs @ ..] => {
                write!(f, "{}", interner.lookup(x))?;
                for x in xs {
                    write!(f, ".{}", interner.lookup(x))?;
                }

                Ok(())
            }
        }
    }
}

pub struct Module {
    pub names: HashSet<InternId>,
    pub types: HashMap<InternId, Vec<InternId>>,
    pub name_imports: HashMap<InternId, Located<Path>>,
    pub type_imports: HashMap<InternId, Located<Path>>,
    pub module_imports: HashMap<InternId, Located<Path>>,
    pub source_name: String,
}

impl Module {
    pub fn empty(source_name: String) -> Self {
        Self {
            names: HashSet::new(),
            types: HashMap::new(),
            name_imports: HashMap::new(),
            type_imports: HashMap::new(),
            module_imports: HashMap::new(),
            source_name,
        }
    }
}
