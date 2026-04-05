use crate::compilation::anf::{self, ANFLocal};

pub type Branch<State> = branch::Branch<State>;

pub struct MatchAs<T> {
    variable: ANFLocal,
    variable_expression: anf::Atom<T>,
    branches: Vec<Branch<T>>,
}

pub struct Observation<State> {
    pub variable: ANFLocal,
    pub variable_expression: anf::Atom<State>,
    pub branches: Vec<Branch<State>>,
}

impl<State> From<Observation<State>> for MatchAs<State> {
    fn from(value: Observation<State>) -> Self {
        Self {
            variable: value.variable,
            variable_expression: value.variable_expression,
            branches: value.branches,
        }
    }
}

impl<State> MatchAs<State> {
    pub fn variable(&self) -> ANFLocal {
        self.variable
    }

    pub fn variable_expression(&self) -> &anf::Atom<State> {
        &self.variable_expression
    }

    pub fn branches(&self) -> &[Branch<State>] {
        &self.branches
    }

    pub fn observe(self) -> Observation<State> {
        Observation {
            variable: self.variable,
            variable_expression: self.variable_expression,
            branches: self.branches,
        }
    }
}

pub mod branch {
    use crate::{
        compilation::anf::{Atom, Expression},
        parse::pattern::Pattern,
        resolution::Renamed,
    };

    pub struct Branch<T> {
        pattern: Pattern<Renamed>,
        matched: Atom<T>,
        expression: Expression<T>,
    }

    pub struct Observation<State> {
        pub pattern: Pattern<Renamed>,
        pub matched: Atom<State>,
        pub expression: Expression<State>,
    }

    impl<State> From<Observation<State>> for Branch<State> {
        fn from(value: Observation<State>) -> Self {
            Self {
                pattern: value.pattern,
                matched: value.matched,
                expression: value.expression,
            }
        }
    }

    impl<State> Branch<State> {
        pub fn pattern(&self) -> &Pattern<Renamed> {
            &self.pattern
        }

        pub fn matched(&self) -> &Atom<State> {
            &self.matched
        }

        pub fn expression(&self) -> &Expression<State> {
            &self.expression
        }

        pub fn observe(self) -> Observation<State> {
            Observation {
                pattern: self.pattern,
                matched: self.matched,
                expression: self.expression,
            }
        }
    }
}
