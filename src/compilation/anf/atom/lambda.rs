use crate::{compilation::anf, resolution::renamer::UniqueName};

pub struct Lambda {
    variable: UniqueName,
    expression: Box<anf::Expression>,
    anf_capture_id: usize,
}

impl Lambda {
    pub fn new(variable: UniqueName, expression: anf::Expression, anf_capture_id: usize) -> Self {
        Self {
            variable,
            expression: Box::new(expression),
            anf_capture_id,
        }
    }

    pub fn variable(&self) -> UniqueName {
        self.variable
    }

    pub fn expression(&self) -> &anf::Expression {
        &self.expression
    }

    pub fn anf_capture_id(&self) -> usize {
        self.anf_capture_id
    }
}
