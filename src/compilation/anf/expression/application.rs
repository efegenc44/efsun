use crate::compilation::anf::{self, ANFLocal};

pub struct Application<State> {
    variable: ANFLocal,
    function: anf::Atom<State>,
    argument: anf::Atom<State>,
    expression: Box<anf::Expression<State>>,
}

pub struct Observation<State> {
    pub variable: ANFLocal,
    pub function: anf::Atom<State>,
    pub argument: anf::Atom<State>,
    pub expression: anf::Expression<State>,
}

impl<State> From<Observation<State>> for Application<State> {
    fn from(value: Observation<State>) -> Self {
        Self {
            variable: value.variable,
            function: value.function,
            argument: value.argument,
            expression: Box::new(value.expression),
        }
    }
}

impl<State> Application<State> {
    pub fn variable(&self) -> ANFLocal {
        self.variable
    }

    pub fn function(&self) -> &anf::Atom<State> {
        &self.function
    }

    pub fn argument(&self) -> &anf::Atom<State> {
        &self.argument
    }

    pub fn expression(&self) -> &anf::Expression<State> {
        &self.expression
    }

    pub fn observe(self) -> Observation<State> {
        Observation {
            variable: self.variable,
            function: self.function,
            argument: self.argument,
            expression: *self.expression,
        }
    }
}
