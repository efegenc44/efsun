use crate::{
    compilation::anf::{self, Expression},
    parse::pattern::Pattern,
};

pub struct MatchAs {
    expression: anf::Atom,
    branches: Vec<Branch>,
}

impl MatchAs {
    pub fn new(expression: anf::Atom, branches: Vec<Branch>) -> Self {
        Self {
            expression,
            branches,
        }
    }

    pub fn expression(&self) -> &anf::Atom {
        &self.expression
    }

    pub fn branches(&self) -> &[Branch] {
        &self.branches
    }
}

pub struct Branch {
    pattern: Pattern,
    expression: Expression,
}

impl Branch {
    pub fn new(pattern: Pattern, expression: Expression) -> Self {
        Self {
            pattern,
            expression,
        }
    }

    pub fn pattern(&self) -> &Pattern {
        &self.pattern
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}
