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
    imports: HashMap<InternId, Located<Path>>,
    // import_alls: HashMap<Path, Located<HashSet<InternId>>>,
    source_name: String,
}

impl Module {
    pub fn empty(source_name: String) -> Self {
        Self {
            names: HashSet::new(),
            types: HashMap::new(),
            imports: HashMap::new(),
            // import_alls: HashMap::new(),
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

    pub fn imports(&self) -> &HashMap<InternId, Located<Path>> {
        &self.imports
    }

    pub fn imports_mut(&mut self) -> &mut HashMap<InternId, Located<Path>> {
        &mut self.imports
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }

    // pub fn import_alls(&self) -> &HashMap<Path, Located<HashSet<InternId>>> {
    //     &self.import_alls
    // }

    // pub fn import_alls_mut(&mut self) -> &mut HashMap<Path, Located<HashSet<InternId>>> {
    //     &mut self.import_alls
    // }
}
