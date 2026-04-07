use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

use crate::interner::{InternId, WithInterner};

#[derive(Clone, Debug)]
pub enum Bound {
    Local(BoundId),
    Capture(BoundId),
    Absolute(Path),
}

impl<'interner> Display for WithInterner<'interner, &Bound> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            Bound::Local(id) => write!(f, "{}", id.0),
            Bound::Capture(id) => write!(f, "captured({})", id.0),
            Bound::Absolute(path) => write!(f, "{}", WithInterner::new(path, self.interner())),
        }
    }
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Capture {
    Local(BoundId),
    Outer(BoundId),
}

impl Display for Capture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(id) => write!(f, "local({})", id.0),
            Self::Outer(id) => write!(f, "outer({})", id.0),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Path(Vec<InternId>);

impl Path {
    pub fn empty() -> Self {
        Path(Vec::new())
    }

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
        match self.data().0.as_slice() {
            [] => unreachable!(),
            [identifier] => {
                write!(f, "{}", self.interner().lookup(identifier))
            }
            [x, xs @ ..] => {
                write!(f, "{}", self.interner().lookup(x))?;
                for x in xs {
                    write!(f, ".{}", self.interner().lookup(x))?;
                }

                Ok(())
            }
        }
    }
}

pub struct Module {
    names: HashSet<InternId>,
    types: HashMap<InternId, Vec<InternId>>,
    imports: HashMap<InternId, Path>,
    source_name: String,
}

impl Module {
    pub fn empty(source_name: String) -> Self {
        Self {
            names: HashSet::new(),
            types: HashMap::new(),
            imports: HashMap::new(),
            source_name,
        }
    }

    pub fn names_mut(&mut self) -> &mut HashSet<InternId> {
        &mut self.names
    }

    pub fn types(&self) -> &HashMap<InternId, Vec<InternId>> {
        &self.types
    }

    pub fn types_mut(&mut self) -> &mut HashMap<InternId, Vec<InternId>> {
        &mut self.types
    }

    pub fn imports(&self) -> &HashMap<InternId, Path> {
        &self.imports
    }

    pub fn imports_mut(&mut self) -> &mut HashMap<InternId, Path> {
        &mut self.imports
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }
}
