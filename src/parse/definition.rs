use crate::{
    interner::InternId,
    location::{Located, Span},
    metadata::PathMetadataId,
    parse::{expression::Expression, type_expression::TypeExpression},
};

pub enum Definition {
    ModulePath(ModulePath),
    Name(Name),
    Import(Import),
    Structure(Structure),
}

pub struct ModulePath {
    pub parts: Located<Vec<InternId>>,
}

pub struct Import {
    pub module_path: Located<Vec<InternId>>,
    pub subimport: Option<Subimport>,
}

pub enum Subimport {
    As(Located<InternId>),
    Import(Vec<Import>),
    All(Span),
}

pub struct Name {
    pub identifier: Located<InternId>,
    pub expression: Located<Expression>,
    pub path_id: PathMetadataId,
}

pub struct Structure {
    pub name: Located<InternId>,
    pub variables: Vec<Located<InternId>>,
    pub constructors: Vec<Located<Constructor>>,
    pub path_id: PathMetadataId,
}

pub struct Constructor {
    pub name: Located<InternId>,
    pub arguments: Vec<Located<TypeExpression>>,
    pub path_id: PathMetadataId,
}

pub struct Module {
    pub definitions: Vec<Definition>,
    pub source_name: String,
}

pub struct Program {
    pub modules: Vec<Module>,
}
