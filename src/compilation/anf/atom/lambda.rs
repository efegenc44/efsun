use crate::{
    compilation::anf,
    interner::InternId,
    resolution::{Resolved, Unresolved, bound::Capture},
};

pub struct Lambda<T> {
    variable: InternId,
    expression: Box<anf::Expression<T>>,
    captures: Option<Vec<Capture>>,
}

pub struct UnresolvedObservation {
    pub variable: InternId,
    pub expression: anf::Expression<Unresolved>,
}

impl From<UnresolvedObservation> for Lambda<Unresolved> {
    fn from(value: UnresolvedObservation) -> Self {
        Self {
            variable: value.variable,
            expression: Box::new(value.expression),
            captures: None,
        }
    }
}

pub struct ResolvedObservation {
    pub variable: InternId,
    pub expression: anf::Expression<Resolved>,
    pub captures: Vec<Capture>,
}

impl From<ResolvedObservation> for Lambda<Resolved> {
    fn from(value: ResolvedObservation) -> Self {
        Self {
            variable: value.variable,
            expression: Box::new(value.expression),
            captures: Some(value.captures),
        }
    }
}

impl<State> Lambda<State> {
    pub fn variable(&self) -> InternId {
        self.variable
    }

    pub fn expression(&self) -> &anf::Expression<State> {
        &self.expression
    }
}

impl Lambda<Unresolved> {
    pub fn observe(self) -> UnresolvedObservation {
        UnresolvedObservation {
            variable: self.variable,
            expression: *self.expression,
        }
    }
}

impl Lambda<Resolved> {
    pub fn captures(&self) -> &[Capture] {
        self.captures.as_ref().unwrap()
    }
}
