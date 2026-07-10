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
            Bound::Absolute(path) => write!(
                f,
                "{}",
                WithInterner {
                    data: path,
                    interner
                }
            ),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    names: HashSet<InternId>,
    types: HashMap<InternId, Vec<InternId>>,
    name_imports: HashMap<InternId, Located<Path>>,
    type_imports: HashMap<InternId, Located<Path>>,
    module_imports: HashMap<InternId, Located<Path>>,
    source_name: String,
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

    pub fn names(&self) -> &HashSet<InternId> {
        &self.names
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

    pub fn name_imports(&self) -> &HashMap<InternId, Located<Path>> {
        &self.name_imports
    }

    pub fn name_imports_mut(&mut self) -> &mut HashMap<InternId, Located<Path>> {
        &mut self.name_imports
    }

    pub fn type_imports(&self) -> &HashMap<InternId, Located<Path>> {
        &self.type_imports
    }

    pub fn type_imports_mut(&mut self) -> &mut HashMap<InternId, Located<Path>> {
        &mut self.type_imports
    }

    pub fn module_imports(&self) -> &HashMap<InternId, Located<Path>> {
        &self.module_imports
    }

    pub fn module_imports_mut(&mut self) -> &mut HashMap<InternId, Located<Path>> {
        &mut self.module_imports
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }
}
