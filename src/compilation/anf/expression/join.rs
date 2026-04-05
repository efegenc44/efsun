use crate::compilation::anf::{self, ANFLocal};

pub struct Join<State> {
    label: usize,
    variable: ANFLocal,
    join: Box<anf::Expression<State>>,
    expression: Box<anf::Expression<State>>,
}

pub struct Observation<State> {
    pub label: usize,
    pub variable: ANFLocal,
    pub join: anf::Expression<State>,
    pub expression: anf::Expression<State>,
}

impl<State> From<Observation<State>> for Join<State> {
    fn from(value: Observation<State>) -> Self {
        Self {
            label: value.label,
            variable: value.variable,
            join: Box::new(value.join),
            expression: Box::new(value.expression),
        }
    }
}

impl<State> Join<State> {
    pub fn label(&self) -> usize {
        self.label
    }

    pub fn variable(&self) -> ANFLocal {
        self.variable
    }

    pub fn join(&self) -> &anf::Expression<State> {
        &self.join
    }

    pub fn expression(&self) -> &anf::Expression<State> {
        &self.expression
    }

    pub fn observe(self) -> Observation<State> {
        Observation {
            label: self.label,
            variable: self.variable,
            join: *self.join,
            expression: *self.expression,
        }
    }
}
