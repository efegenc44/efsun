use crate::{interner::InternId, location::Located, parse::expression::Expression};

pub struct Name {
    pub identifier: Located<InternId>,
    pub expression: Located<Expression>,
    pub path_id: usize,
}

impl Name {
    pub fn new(
        identifier: Located<InternId>,
        expression: Located<Expression>,
        path_id: usize,
    ) -> Self {
        Self {
            identifier,
            expression,
            path_id,
        }
    }

    pub fn identifier(&self) -> Located<InternId> {
        self.identifier
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn path_id(&self) -> usize {
        self.path_id
    }
}
