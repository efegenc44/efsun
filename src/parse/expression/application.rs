use crate::{location::Located, parse::expression::Expression};

pub struct Application {
    pub function: Box<Located<Expression>>,
    pub argument: Box<Located<Expression>>,
}

impl Application {
    pub fn new(function: Located<Expression>, argument: Located<Expression>) -> Self {
        Self {
            function: Box::new(function),
            argument: Box::new(argument),
        }
    }

    pub fn function(&self) -> &Located<Expression> {
        &self.function
    }

    pub fn argument(&self) -> &Located<Expression> {
        &self.argument
    }
}
