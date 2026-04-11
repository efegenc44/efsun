use crate::{
    interner::InternId,
    location::Located,
    parse::expression::Expression,
    resolution::renamer::UniqueName,
    state::{BeforeRenamed, Renamed},
};

pub struct LetIn<State> {
    variable: Located<InternId>,
    variable_expression: Box<Located<Expression<State>>>,
    return_expression: Box<Located<Expression<State>>>,
    unique_variable: Option<UniqueName>,
}

pub struct Observation<State: BeforeRenamed> {
    pub variable: Located<InternId>,
    pub variable_expression: Located<Expression<State>>,
    pub return_expression: Located<Expression<State>>,
}

impl<S: BeforeRenamed> From<Observation<S>> for LetIn<S> {
    fn from(val: Observation<S>) -> Self {
        LetIn {
            variable: val.variable,
            variable_expression: Box::new(val.variable_expression),
            return_expression: Box::new(val.return_expression),
            unique_variable: None,
        }
    }
}

pub struct RenamedObservation {
    pub variable: Located<InternId>,
    pub variable_expression: Located<Expression<Renamed>>,
    pub return_expression: Located<Expression<Renamed>>,
    pub unique_variable: UniqueName,
}

impl From<RenamedObservation> for LetIn<Renamed> {
    fn from(value: RenamedObservation) -> Self {
        LetIn {
            variable: value.variable,
            variable_expression: Box::new(value.variable_expression),
            return_expression: Box::new(value.return_expression),
            unique_variable: Some(value.unique_variable),
        }
    }
}

impl<State> LetIn<State> {
    pub fn variable(&self) -> Located<InternId> {
        self.variable
    }

    pub fn variable_expression(&self) -> &Located<Expression<State>> {
        &self.variable_expression
    }

    pub fn return_expression(&self) -> &Located<Expression<State>> {
        &self.return_expression
    }
}

impl<S: BeforeRenamed> LetIn<S> {
    pub fn observe(self) -> Observation<S> {
        Observation {
            variable: self.variable,
            variable_expression: *self.variable_expression,
            return_expression: *self.return_expression,
        }
    }
}

impl LetIn<Renamed> {
    pub fn observe(self) -> RenamedObservation {
        RenamedObservation {
            variable: self.variable,
            variable_expression: *self.variable_expression,
            return_expression: *self.return_expression,
            unique_variable: self.unique_variable.unwrap(),
        }
    }
}
