use std::{collections::{HashMap, HashSet}, fmt::{Debug, Display}};

use crate::interner::{InternId, Interner};

#[derive(Clone, Debug)]
pub enum Bound {
    Local(BoundId),
    Capture(BoundId),
    Absolute(Path)
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

impl Bound {
    pub fn display(&self, interner: &Interner) -> String {
        match self {
            Bound::Local(id) => format!("{}", id.0),
            Bound::Capture(id) => format!("captured({})", id.0),
            Bound::Absolute(path) => path.display(interner),
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Path(Vec<InternId>);

impl Path {
    pub fn empty() -> Self {
        Path(Vec::new())
    }

    pub fn from_parts(parts: Vec<InternId>) -> Self {
        Self(parts)
    }

    pub fn push(&mut self, identifier: InternId) {
        self.0.push(identifier);
    }

    pub fn pop(&mut self) -> InternId {
        self.0.pop().unwrap()
    }

    pub fn append(&self, identifier: InternId) -> Self {
        let mut clone = self.clone();
        clone.0.push(identifier);
        clone
    }

    pub fn append_parts(&self, identifiers: Vec<InternId>) -> Self {
        let mut clone = self.clone();
        clone.0.extend(identifiers);
        clone
    }

    pub fn display(&self, interner: &Interner) -> String {
        self.0
            .iter()
            .map(|id| interner.lookup(*id))
            .collect::<Vec<_>>()
            .join(".")
    }
}

pub struct Module {
    names: HashSet<InternId>,
    imports: HashMap<InternId, Path>,
    source_name: String,
}

impl Module {
    pub fn empty(source_name: String) -> Self {
        Self {
            names: HashSet::new(),
            imports: HashMap::new(),
            source_name
        }
    }

    pub fn names(&self) -> &HashSet<InternId> {
        &self.names
    }

    pub fn names_mut(&mut self) -> &mut HashSet<InternId> {
        &mut self.names
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

