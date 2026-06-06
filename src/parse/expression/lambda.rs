use crate::{
    compilation::anf::{self, Atom, atom},
    interner::InternId,
    location::Located,
    metadata::{CaptureMetadataId, Generator, UniqueNameMetadataId},
    parse::expression::Expression,
};

pub struct Lambda {
    variable: Located<InternId>,
    expression: Box<Located<Expression>>,
    capture_id: CaptureMetadataId,
    unique_name_id: UniqueNameMetadataId,
}

impl Lambda {
    pub fn new(
        variable: Located<InternId>,
        expression: Located<Expression>,
        capture_id: CaptureMetadataId,
        unique_name_id: UniqueNameMetadataId,
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

    pub fn capture_id(&self) -> CaptureMetadataId {
        self.capture_id
    }

    pub fn unique_name_id(&self) -> UniqueNameMetadataId {
        self.unique_name_id
    }

    pub fn into_anf(self, transformer: &anf::Transformer, k: anf::Continuation) -> anf::Expression {
        let Self {
            expression,
            unique_name_id,
            ..
        } = self;

        let variable = transformer.metadata()[unique_name_id].unwrap();

        let lambda = atom::Lambda::new(
            variable,
            transformer.transform(expression.into_data()),
            transformer.indicies_mut().get(),
        );

        k(Atom::Lambda(lambda))
    }
}
