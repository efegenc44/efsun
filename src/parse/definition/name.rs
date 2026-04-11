use crate::{
    interner::InternId,
    location::Located,
    parse::expression::Expression,
    resolution::bound::Path,
    state::{AfterUnresolved, Unresolved},
};

pub struct Name<T> {
    identifier: Located<InternId>,
    expression: Located<Expression<T>>,
    path: Option<Path>,
}

pub struct UnresolvedObservation {
    pub identifier: Located<InternId>,
    pub expression: Located<Expression<Unresolved>>,
}

impl From<UnresolvedObservation> for Name<Unresolved> {
    fn from(value: UnresolvedObservation) -> Self {
        Self {
            identifier: value.identifier,
            expression: value.expression,
            path: None,
        }
    }
}

pub struct Observation<State: AfterUnresolved> {
    pub identifier: Located<InternId>,
    pub expression: Located<Expression<State>>,
    pub path: Path,
}

impl<S: AfterUnresolved> From<Observation<S>> for Name<S> {
    fn from(value: Observation<S>) -> Self {
        Self {
            identifier: value.identifier,
            expression: value.expression,
            path: Some(value.path),
        }
    }
}

impl<T> Name<T> {
    pub fn identifier(&self) -> Located<InternId> {
        self.identifier
    }

    pub fn expression(&self) -> &Located<Expression<T>> {
        &self.expression
    }
}

impl Name<Unresolved> {
    pub fn observe(self) -> UnresolvedObservation {
        UnresolvedObservation {
            identifier: self.identifier,
            expression: self.expression,
        }
    }
}

impl<S: AfterUnresolved> Name<S> {
    pub fn path(&self) -> &Path {
        self.path.as_ref().unwrap()
    }

    pub fn observe(self) -> Observation<S> {
        Observation {
            identifier: self.identifier,
            expression: self.expression,
            path: self.path.unwrap(),
        }
    }
}
