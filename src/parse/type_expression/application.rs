use crate::{location::Located, parse::type_expression::TypeExpression};

pub struct Application {
    function: Box<Located<TypeExpression>>,
    arguments: Vec<Located<TypeExpression>>,
}

impl Application {
    pub fn new(function: Located<TypeExpression>, arguments: Vec<Located<TypeExpression>>) -> Self {
        Self {
            function: Box::new(function),
            arguments,
        }
    }

    pub fn function(&self) -> &Located<TypeExpression> {
        &self.function
    }

    pub fn arguments(&self) -> &[Located<TypeExpression>] {
        &self.arguments
    }
}
