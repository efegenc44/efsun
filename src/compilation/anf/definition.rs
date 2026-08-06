use crate::{compilation::anf, data_table::PathDataId, interner::InternId};

pub enum Definition {
    Name(Name),
    Structure(Structure),
}

pub struct Name {
    pub expression: anf::Expression,
    pub path_id: PathDataId,
}

pub struct Structure {
    pub constructors: Vec<Constructor>,
}

pub struct Constructor {
    pub name: InternId,
    pub arity: usize,
    pub path_id: PathDataId,
}

pub struct Module {
    definitions: Vec<Definition>,
}

impl Module {
    pub fn new(definitions: Vec<Definition>) -> Self {
        Self { definitions }
    }

    pub fn definitions(&self) -> &[Definition] {
        &self.definitions
    }
}

pub struct Program {
    modules: Vec<Module>,
}

impl Program {
    pub fn new(modules: Vec<Module>) -> Self {
        Self { modules }
    }

    pub fn modules(&self) -> &[Module] {
        &self.modules
    }
}
