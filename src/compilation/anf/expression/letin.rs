use crate::{compilation::anf, resolution::renamer::UniqueName};

pub struct LetIn {
    variable: UniqueName,
    variable_expression: anf::Atom,
    return_expression: Box<anf::Expression>,
}

impl LetIn {
    pub fn new(
        variable: UniqueName,
        variable_expression: anf::Atom,
        return_expression: anf::Expression,
    ) -> Self {
        Self {
            variable,
            variable_expression,
            return_expression: Box::new(return_expression),
        }
    }

    pub fn variable(&self) -> UniqueName {
        self.variable
    }

    pub fn variable_expression(&self) -> &anf::Atom {
        &self.variable_expression
    }

    pub fn return_expression(&self) -> &anf::Expression {
        &self.return_expression
    }
}
