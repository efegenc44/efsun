use crate::{
    location::Located,
    parse::{expression::Expression, pattern::Pattern},
};

pub struct MatchAs {
    pub expression: Box<Located<Expression>>,
    pub branches: Vec<Located<Branch>>,
}

impl MatchAs {
    pub fn new(expression: Located<Expression>, branches: Vec<Located<Branch>>) -> Self {
        Self {
            expression: Box::new(expression),
            branches,
        }
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn branches(&self) -> &[Located<Branch>] {
        &self.branches
    }
}

pub struct Branch {
    pub pattern: Located<Pattern>,
    pub expression: Located<Expression>,
}

impl Branch {
    pub fn new(pattern: Located<Pattern>, expression: Located<Expression>) -> Self {
        Self {
            pattern,
            expression,
        }
    }

    pub fn pattern(&self) -> &Located<Pattern> {
        &self.pattern
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }
}
