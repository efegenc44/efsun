use crate::{interner::InternId, location::Located, parse::expression::Expression};

pub struct Lambda {
    pub variable: Located<InternId>,
    pub expression: Box<Located<Expression>>,
    pub capture_id: usize,
    pub unique_name_id: usize,
}

impl Lambda {
    pub fn new(
        variable: Located<InternId>,
        expression: Located<Expression>,
        capture_id: usize,
        unique_name_id: usize,
    ) -> Self {
        Self {
            variable,
            expression: Box::new(expression),
            capture_id,
            unique_name_id,
        }
    }

    pub fn variable(&self) -> Located<InternId> {
        self.variable
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn capture_id(&self) -> usize {
        self.capture_id
    }

    pub fn unique_name_id(&self) -> usize {
        self.unique_name_id
    }
}
