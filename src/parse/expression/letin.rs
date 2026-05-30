use crate::{interner::InternId, location::Located, parse::expression::Expression};

pub struct LetIn {
    pub variable: Located<InternId>,
    pub variable_expression: Box<Located<Expression>>,
    pub return_expression: Box<Located<Expression>>,
    pub unique_name_id: usize,
}

impl LetIn {
    pub fn new(
        variable: Located<InternId>,
        variable_expression: Located<Expression>,
        return_expression: Located<Expression>,
        unique_name_id: usize,
    ) -> Self {
        Self {
            variable,
            variable_expression: Box::new(variable_expression),
            return_expression: Box::new(return_expression),
            unique_name_id,
        }
    }

    pub fn variable(&self) -> Located<InternId> {
        self.variable
    }

    pub fn variable_expression(&self) -> &Located<Expression> {
        &self.variable_expression
    }

    pub fn return_expression(&self) -> &Located<Expression> {
        &self.return_expression
    }

    pub fn unique_name_id(&self) -> usize {
        self.unique_name_id
    }
}
