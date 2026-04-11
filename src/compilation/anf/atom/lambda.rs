use crate::{
    compilation::anf,
    resolution::{bound::Capture, renamer::UniqueName},
    state::{Resolved, Unresolved},
};

pub struct Lambda<T> {
    variable: UniqueName,
    expression: Box<anf::Expression<T>>,
    captures: Option<Vec<Capture>>,
}

pub struct UnresolvedObservation {
    pub variable: UniqueName,
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
    pub variable: UniqueName,
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
    pub fn variable(&self) -> UniqueName {
        self.variable
    }

    pub fn expression(&self) -> &anf::Expression<State> {
        &self.expression
    }

    pub fn try_captures(&self) -> Option<&Vec<Capture>> {
        self.captures.as_ref()
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
