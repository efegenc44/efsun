use crate::{interner::InternId, metadata::PathMetadataId};

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
    arity: usize,
    path_id: PathMetadataId,
}

impl Constructor {
    pub fn new(name: InternId, arity: usize, path_id: PathMetadataId) -> Self {
        Self {
            name,
            arity,
            path_id,
        }
    }

    pub fn name(&self) -> InternId {
        self.name
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn path_id(&self) -> PathMetadataId {
        self.path_id
    }
}
