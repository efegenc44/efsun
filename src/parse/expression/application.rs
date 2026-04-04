use crate::{location::Located, parse::expression::Expression};

pub struct Application<State> {
    function: Box<Located<Expression<State>>>,
    argument: Box<Located<Expression<State>>>,
}

pub struct Observation<State> {
    pub function: Located<Expression<State>>,
    pub argument: Located<Expression<State>>,
}

impl<State> From<Observation<State>> for Application<State> {
    fn from(val: Observation<State>) -> Self {
        Application {
            function: Box::new(val.function),
            argument: Box::new(val.argument),
        }
    }
}

impl<State> Application<State> {
    pub fn function(&self) -> &Located<Expression<State>> {
        &self.function
    }

    pub fn argument(&self) -> &Located<Expression<State>> {
        &self.argument
    }

    pub fn observe(self) -> Observation<State> {
        Observation {
            function: *self.function,
            argument: *self.argument,
        }
    }
}
