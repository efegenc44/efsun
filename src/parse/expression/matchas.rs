use crate::{location::Located, parse::expression::Expression};

pub type Branch<State> = branch::Branch<State>;

pub struct MatchAs<State> {
    expression: Box<Located<Expression<State>>>,
    branches: Vec<Located<Branch<State>>>,
}

pub struct Observation<State> {
    pub expression: Located<Expression<State>>,
    pub branches: Vec<Located<Branch<State>>>,
}

impl<State> From<Observation<State>> for MatchAs<State> {
    fn from(val: Observation<State>) -> Self {
        MatchAs {
            expression: Box::new(val.expression),
            branches: val.branches,
        }
    }
}

impl<State> MatchAs<State> {
    pub fn expression(&self) -> &Located<Expression<State>> {
        &self.expression
    }

    pub fn branches(&self) -> &[Located<Branch<State>>] {
        &self.branches
    }

    pub fn observe(self) -> Observation<State> {
        Observation {
            expression: *self.expression,
            branches: self.branches,
        }
    }
}

pub mod branch {
    use crate::{
        location::Located,
        parse::{expression::Expression, pattern::Pattern},
    };

    pub struct Branch<State> {
        pattern: Located<Pattern<State>>,
        expression: Located<Expression<State>>,
    }

    pub struct Observation<State> {
        pub pattern: Located<Pattern<State>>,
        pub expression: Located<Expression<State>>,
    }

    impl<State> From<Observation<State>> for Branch<State> {
        fn from(val: Observation<State>) -> Self {
            Branch {
                pattern: val.pattern,
                expression: val.expression,
            }
        }
    }

    impl<State> Branch<State> {
        pub fn pattern(&self) -> &Located<Pattern<State>> {
            &self.pattern
        }

        pub fn expression(&self) -> &Located<Expression<State>> {
            &self.expression
        }

        pub fn observe(self) -> Observation<State> {
            Observation {
                pattern: self.pattern,
                expression: self.expression,
            }
        }
    }
}
