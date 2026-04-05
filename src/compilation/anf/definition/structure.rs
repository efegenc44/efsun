use crate::resolution::bound::Path;

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
    path: Path,
    arity: usize,
}

impl Constructor {
    pub fn new(path: Path, arity: usize) -> Self {
        Self { path, arity }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn arity(&self) -> usize {
        self.arity
    }
}
