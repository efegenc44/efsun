use crate::{compilation::anf, interner::InternId, metadata::PathMetadataId};

pub struct Name {
    identifier: InternId,
    expression: anf::Expression,
    path_id: PathMetadataId,
}

impl Name {
    pub fn new(identifier: InternId, expression: anf::Expression, path_id: PathMetadataId) -> Self {
        Self {
            identifier,
            expression,
            path_id,
        }
    }

    pub fn identifier(&self) -> InternId {
        self.identifier
    }

    pub fn expression(&self) -> &anf::Expression {
        &self.expression
    }

    pub fn path_id(&self) -> PathMetadataId {
        self.path_id
    }
}
