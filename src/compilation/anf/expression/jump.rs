use crate::compilation::anf::Atom;

pub struct Jump {
    to: usize,
    expression: Atom,
}

impl Jump {
    pub fn new(to: usize, expression: Atom) -> Self {
        Self { to, expression }
    }

    pub fn to(&self) -> usize {
        self.to
    }

    pub fn expression(&self) -> &Atom {
        &self.expression
    }
}
