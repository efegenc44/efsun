use crate::compilation::anf::{self, Local};

pub struct Join {
    label: usize,
    variable: Local,
    join: Box<anf::Expression>,
    expression: Box<anf::Expression>,
}

impl Join {
    pub fn new(
        label: usize,
        variable: Local,
        join: anf::Expression,
        expression: anf::Expression,
    ) -> Self {
        Self {
            label,
            variable,
            join: Box::new(join),
            expression: Box::new(expression),
        }
    }

    pub fn label(&self) -> usize {
        self.label
    }

    pub fn variable(&self) -> Local {
        self.variable
    }

    pub fn join(&self) -> &anf::Expression {
        &self.join
    }

    pub fn expression(&self) -> &anf::Expression {
        &self.expression
    }
}
