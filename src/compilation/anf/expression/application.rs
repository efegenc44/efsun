use crate::compilation::anf::{self, Local};

pub struct Application {
    variable: Local,
    function: anf::Atom,
    argument: anf::Atom,
    expression: Box<anf::Expression>,
}

impl Application {
    pub fn new(
        variable: Local,
        function: anf::Atom,
        argument: anf::Atom,
        expression: anf::Expression,
    ) -> Self {
        Self {
            variable,
            function,
            argument,
            expression: Box::new(expression),
        }
    }

    pub fn variable(&self) -> Local {
        self.variable
    }

    pub fn function(&self) -> &anf::Atom {
        &self.function
    }

    pub fn argument(&self) -> &anf::Atom {
        &self.argument
    }

    pub fn expression(&self) -> &anf::Expression {
        &self.expression
    }
}
