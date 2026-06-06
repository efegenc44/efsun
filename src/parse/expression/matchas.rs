use crate::{
    compilation::anf::{self, Atom},
    location::Located,
    parse::{expression::Expression, pattern::Pattern},
};

pub struct MatchAs {
    expression: Box<Located<Expression>>,
    branches: Vec<Located<Branch>>,
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

    pub fn into_anf(self, transformer: &anf::Transformer, k: anf::Continuation) -> anf::Expression {
        let Self {
            expression,
            branches,
        } = self;

        #[rustfmt::skip]
        let result = expression.into_data().into_anf(transformer, Box::new(|expression| {
            let label = transformer.new_local_id();

            let branch_k = |expression| {
                let jump = anf::expression::Jump::new(label, expression);
                anf::Expression::Jump(jump)
            };

            let branches = branches
                .into_iter()
                .map(|branch| branch.into_data().into_anf(transformer, Box::new(branch_k)))
                .collect();

            let matchas = anf::expression::MatchAs::new(expression, branches);

            let (variable, path) = transformer.new_anf_local();

            let join = anf::expression::Join::new(
                label,
                variable,
                anf::Expression::Match(matchas),
                k(Atom::Path(path)),
            );

            anf::Expression::Join(join)
        }));

        result
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

    pub fn into_anf(
        self,
        transformer: &anf::Transformer,
        k: anf::Continuation,
    ) -> anf::expression::matchas::Branch {
        let Self {
            pattern,
            expression,
        } = self;

        anf::expression::matchas::Branch::new(
            pattern.into_data(),
            expression.into_data().into_anf(transformer, k),
        )
    }
}
