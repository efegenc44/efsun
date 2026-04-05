use crate::compilation::anf::Atom;

pub struct Jump<State> {
    to: usize,
    expression: Atom<State>,
}

pub struct Observation<State> {
    pub to: usize,
    pub expression: Atom<State>,
}

impl<State> From<Observation<State>> for Jump<State> {
    fn from(value: Observation<State>) -> Self {
        Self {
            to: value.to,
            expression: value.expression,
        }
    }
}

impl<State> Jump<State> {
    pub fn to(&self) -> usize {
        self.to
    }

    pub fn expression(&self) -> &Atom<State> {
        &self.expression
    }

    pub fn observe(self) -> Observation<State> {
        Observation {
            to: self.to,
            expression: self.expression,
        }
    }
}
