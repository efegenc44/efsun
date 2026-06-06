use crate::{
    compilation::anf, interner::InternId, location::Located, parse::expression::Expression,
};

pub struct LetIn {
    variable: Located<InternId>,
    variable_expression: Box<Located<Expression>>,
    return_expression: Box<Located<Expression>>,
    unique_name_id: usize,
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

    pub fn into_anf(self, transformer: &anf::Transformer, k: anf::Continuation) -> anf::Expression {
        let Self {
            variable_expression,
            return_expression,
            unique_name_id,
            ..
        } = self;

        #[rustfmt::skip]
        let result = variable_expression.into_data().into_anf(transformer, Box::new(|variable_expression| {
            let letin = anf::expression::LetIn::new(
                transformer.metadata().get_unique_name(unique_name_id).unwrap(),
                variable_expression,
                return_expression.into_data().into_anf(transformer, k),
            );

            anf::Expression::LetIn(letin)
        }));

        result
    }
}
