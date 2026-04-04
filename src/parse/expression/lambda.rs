use crate::{
    interner::InternId,
    location::Located,
    parse::expression::Expression,
    resolution::{Renamed, Resolved, Unresolved, bound::Capture},
};

pub struct Lambda<State> {
    variable: Located<InternId>,
    expression: Box<Located<Expression<State>>>,
    captures: Option<Vec<Capture>>,
}

pub struct UnresolvedObservation {
    pub variable: Located<InternId>,
    pub expression: Located<Expression<Unresolved>>,
}

impl From<UnresolvedObservation> for Lambda<Unresolved> {
    fn from(val: UnresolvedObservation) -> Self {
        Lambda {
            variable: val.variable,
            expression: Box::new(val.expression),
            captures: None,
        }
    }
}

pub struct ResolvedObservation {
    pub variable: Located<InternId>,
    pub expression: Located<Expression<Resolved>>,
    pub captures: Vec<Capture>,
}

impl From<ResolvedObservation> for Lambda<Resolved> {
    fn from(val: ResolvedObservation) -> Self {
        Lambda {
            variable: val.variable,
            expression: Box::new(val.expression),
            captures: Some(val.captures),
        }
    }
}

pub struct RenamedObservation {
    pub variable: Located<InternId>,
    pub expression: Located<Expression<Renamed>>,
    pub captures: Vec<Capture>,
}

impl From<RenamedObservation> for Lambda<Renamed> {
    fn from(val: RenamedObservation) -> Self {
        Lambda {
            variable: val.variable,
            expression: Box::new(val.expression),
            captures: Some(val.captures),
        }
    }
}

impl<State> Lambda<State> {
    pub fn variable(&self) -> Located<InternId> {
        self.variable
    }

    pub fn expression(&self) -> &Located<Expression<State>> {
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

    pub fn observe(self) -> ResolvedObservation {
        ResolvedObservation {
            variable: self.variable,
            expression: *self.expression,
            captures: self.captures.unwrap(),
        }
    }
}

impl Lambda<Renamed> {
    pub fn observe(self) -> RenamedObservation {
        RenamedObservation {
            variable: self.variable,
            expression: *self.expression,
            captures: self.captures.unwrap(),
        }
    }
}
