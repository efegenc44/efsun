use crate::{interner::InternId, location::Located, parse::expression::Expression};

pub struct LetIn<State> {
    variable: Located<InternId>,
    variable_expression: Box<Located<Expression<State>>>,
    return_expression: Box<Located<Expression<State>>>,
}

pub struct Observation<State> {
    pub variable: Located<InternId>,
    pub variable_expression: Located<Expression<State>>,
    pub return_expression: Located<Expression<State>>,
}

impl<State> From<Observation<State>> for LetIn<State> {
    fn from(val: Observation<State>) -> Self {
        LetIn {
            variable: val.variable,
            variable_expression: Box::new(val.variable_expression),
            return_expression: Box::new(val.return_expression),
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

    pub fn observe(self) -> Observation<State> {
        Observation {
            variable: self.variable,
            variable_expression: *self.variable_expression,
            return_expression: *self.return_expression,
        }
    }
}
