use crate::{
    interner::InternId,
    location::Located,
    parse::expression::Expression,
    resolution::{Renamed, Resolved, Unresolved, bound::Path},
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

pub struct ResolvedObservation {
    pub identifier: Located<InternId>,
    pub expression: Located<Expression<Resolved>>,
    pub path: Path,
}

impl From<ResolvedObservation> for Name<Resolved> {
    fn from(value: ResolvedObservation) -> Self {
        Self {
            identifier: value.identifier,
            expression: value.expression,
            path: Some(value.path),
        }
    }
}

pub struct RenamedObservation {
    pub identifier: Located<InternId>,
    pub expression: Located<Expression<Renamed>>,
    pub path: Path,
}

impl From<RenamedObservation> for Name<Renamed> {
    fn from(value: RenamedObservation) -> Self {
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

impl Name<Resolved> {
    pub fn path(&self) -> &Path {
        self.path.as_ref().unwrap()
    }

    pub fn observe(self) -> ResolvedObservation {
        ResolvedObservation {
            identifier: self.identifier,
            expression: self.expression,
            path: self.path.unwrap(),
        }
    }
}

impl Name<Renamed> {
    pub fn observe(self) -> RenamedObservation {
        RenamedObservation {
            identifier: self.identifier,
            expression: self.expression,
            path: self.path.unwrap(),
        }
    }
}
