use crate::{compilation::anf, interner::InternId};

pub struct Name {
    identifier: InternId,
    expression: anf::Expression,
    path_id: usize,
}

impl Name {
    pub fn new(identifier: InternId, expression: anf::Expression, path_id: usize) -> Self {
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

    pub fn path_id(&self) -> usize {
        self.path_id
    }
}
