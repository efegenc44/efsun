use crate::{compilation::anf, interner::InternId};

pub struct LetIn<T> {
    variable: InternId,
    variable_expression: anf::Atom<T>,
    return_expression: Box<anf::Expression<T>>,
}

pub struct Observation<T> {
    pub variable: InternId,
    pub variable_expression: anf::Atom<T>,
    pub return_expression: anf::Expression<T>,
}

impl<State> From<Observation<State>> for LetIn<State> {
    fn from(value: Observation<State>) -> Self {
        Self {
            variable: value.variable,
            variable_expression: value.variable_expression,
            return_expression: Box::new(value.return_expression),
        }
    }
}

impl<State> LetIn<State> {
    pub fn variable(&self) -> InternId {
        self.variable
    }

    pub fn variable_expression(&self) -> &anf::Atom<State> {
        &self.variable_expression
    }

    pub fn return_expression(&self) -> &anf::Expression<State> {
        &self.return_expression
    }

    pub fn observe(self) -> Observation<State> {
        Observation {
            variable: self.variable,
            variable_expression: self.variable_expression,
            return_expression: *self.return_expression,
        }
    }
}
