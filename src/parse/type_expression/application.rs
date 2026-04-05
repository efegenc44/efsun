use crate::{location::Located, parse::type_expression::TypeExpression};

pub struct Application<State> {
    function: Box<Located<TypeExpression<State>>>,
    arguments: Vec<Located<TypeExpression<State>>>,
}

pub struct Observation<State> {
    pub function: Located<TypeExpression<State>>,
    pub arguments: Vec<Located<TypeExpression<State>>>,
}

impl<State> From<Observation<State>> for Application<State> {
    fn from(value: Observation<State>) -> Self {
        Self {
            function: Box::new(value.function),
            arguments: value.arguments,
        }
    }
}

impl<State> Application<State> {
    pub fn function(&self) -> &Located<TypeExpression<State>> {
        &self.function
    }

    pub fn arguments(&self) -> &[Located<TypeExpression<State>>] {
        &self.arguments
    }

    pub fn observe(self) -> Observation<State> {
        Observation {
            function: *self.function,
            arguments: self.arguments,
        }
    }
}
