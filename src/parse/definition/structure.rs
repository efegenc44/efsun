use crate::{interner::InternId, location::Located, parse::type_expression::TypeExpression};

pub struct Structure {
    pub name: Located<InternId>,
    pub variables: Vec<Located<InternId>>,
    pub constructors: Vec<Located<Constructor>>,
    pub path_id: usize,
}

impl Structure {
    pub fn new(
        name: Located<InternId>,
        variables: Vec<Located<InternId>>,
        constructors: Vec<Located<Constructor>>,
        path_id: usize,
    ) -> Self {
        Self {
            name,
            variables,
            constructors,
            path_id,
        }
    }

    pub fn name(&self) -> Located<InternId> {
        self.name
    }

    pub fn variables(&self) -> &[Located<InternId>] {
        &self.variables
    }

    pub fn constructors(&self) -> &[Located<Constructor>] {
        &self.constructors
    }

    pub fn path_id(&self) -> usize {
        self.path_id
    }
}

pub struct Constructor {
    pub name: Located<InternId>,
    pub arguments: Vec<Located<TypeExpression>>,
    pub path_id: usize,
}

impl Constructor {
    pub fn new(
        name: Located<InternId>,
        arguments: Vec<Located<TypeExpression>>,
        path_id: usize,
    ) -> Self {
        Self {
            name,
            arguments,
            path_id,
        }
    }

    pub fn name(&self) -> Located<InternId> {
        self.name
    }

    pub fn arguments(&self) -> &[Located<TypeExpression>] {
        &self.arguments
    }

    pub fn path_id(&self) -> usize {
        self.path_id
    }
}
