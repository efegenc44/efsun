use crate::{interner::InternId, location::Located};

pub struct Import {
    module_path: Located<Vec<InternId>>,
    subimport: Option<Subimport>,
}

impl Import {
    pub fn new(module_path: Located<Vec<InternId>>, subimport: Option<Subimport>) -> Self {
        Self {
            module_path,
            subimport,
        }
    }

    pub fn module_path(&self) -> &Located<Vec<InternId>> {
        &self.module_path
    }

    pub fn subimport(&self) -> Option<&Subimport> {
        self.subimport.as_ref()
    }
}

pub enum Subimport {
    As(Located<InternId>),
    Import(Vec<Import>),
}
