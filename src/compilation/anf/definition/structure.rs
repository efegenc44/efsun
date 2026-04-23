use crate::{interner::InternId, resolution::bound::Path};

pub struct Structure {
    constructors: Vec<Constructor>,
}

impl Structure {
    pub fn new(constructors: Vec<Constructor>) -> Self {
        Self { constructors }
    }

    pub fn constructors(&self) -> &[Constructor] {
        &self.constructors
    }
}

pub struct Constructor {
    name: InternId,
    path: Path,
    arity: usize,
}

impl Constructor {
    pub fn new(name: InternId, path: Path, arity: usize) -> Self {
        Self { name, path, arity }
    }

    pub fn name(&self) -> InternId {
        self.name
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn arity(&self) -> usize {
        self.arity
    }
}
