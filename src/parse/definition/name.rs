use crate::{
    compilation::anf, interner::InternId, location::Located, metadata::PathMetadataId,
    parse::expression::Expression,
};

pub struct Name {
    pub identifier: Located<InternId>,
    pub expression: Located<Expression>,
    pub path_id: PathMetadataId,
}

impl Name {
    pub fn new(
        identifier: Located<InternId>,
        expression: Located<Expression>,
        path_id: PathMetadataId,
    ) -> Self {
        Self {
            identifier,
            expression,
            path_id,
        }
    }

    pub fn identifier(&self) -> Located<InternId> {
        self.identifier
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn path_id(&self) -> PathMetadataId {
        self.path_id
    }

    pub fn into_anf(self, transformer: &anf::Transformer) -> anf::Definition {
        let Self {
            identifier,
            expression,
            path_id,
        } = self;

        let name = anf::definition::Name::new(
            identifier.into_data(),
            transformer.transform(expression.into_data()),
            path_id,
        );

        anf::Definition::Name(name)
    }
}
