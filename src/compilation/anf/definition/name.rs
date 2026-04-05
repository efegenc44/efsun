use crate::{compilation::anf, interner::InternId, resolution::bound::Path};

pub struct Name<State> {
    identifier: InternId,
    expression: anf::Expression<State>,
    path: Path,
}

pub struct Observation<State> {
    pub identifier: InternId,
    pub expression: anf::Expression<State>,
    pub path: Path,
}

impl<State> From<Observation<State>> for Name<State> {
    fn from(value: Observation<State>) -> Self {
        Self {
            identifier: value.identifier,
            expression: value.expression,
            path: value.path,
        }
    }
}

impl<State> Name<State> {
    pub fn identifier(&self) -> InternId {
        self.identifier
    }

    pub fn expression(&self) -> &anf::Expression<State> {
        &self.expression
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn observe(self) -> Observation<State> {
        Observation {
            identifier: self.identifier,
            expression: self.expression,
            path: self.path,
        }
    }
}
