use crate::compilation::anf;

pub type Branch<State> = branch::Branch<State>;

pub struct MatchAs<State> {
    expression: anf::Atom<State>,
    branches: Vec<Branch<State>>,
}

pub struct Observation<State> {
    pub expression: anf::Atom<State>,
    pub branches: Vec<Branch<State>>,
}

impl<State> From<Observation<State>> for MatchAs<State> {
    fn from(value: Observation<State>) -> Self {
        Self {
            expression: value.expression,
            branches: value.branches,
        }
    }
}

impl<State> MatchAs<State> {
    pub fn expression(&self) -> &anf::Atom<State> {
        &self.expression
    }

    pub fn branches(&self) -> &[Branch<State>] {
        &self.branches
    }

    pub fn observe(self) -> Observation<State> {
        Observation {
            expression: self.expression,
            branches: self.branches,
        }
    }
}

pub mod branch {
    use crate::{compilation::anf::Expression, parse::pattern::Pattern, state::Renamed};

    pub struct Branch<T> {
        pattern: Pattern<Renamed>,
        expression: Expression<T>,
    }

    pub struct Observation<State> {
        pub pattern: Pattern<Renamed>,
        pub expression: Expression<State>,
    }

    impl<State> From<Observation<State>> for Branch<State> {
        fn from(value: Observation<State>) -> Self {
            Self {
                pattern: value.pattern,
                expression: value.expression,
            }
        }
    }

    impl<State> Branch<State> {
        pub fn pattern(&self) -> &Pattern<Renamed> {
            &self.pattern
        }

        pub fn expression(&self) -> &Expression<State> {
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
