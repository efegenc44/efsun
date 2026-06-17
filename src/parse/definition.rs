use std::fmt::Display;

use crate::{
    interner::{InternId, Interner},
    location::Located,
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

impl Definition {
    #[allow(unused)]
    pub fn print(&self, interner: &Interner, depth: usize) {
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Self::ModulePath(module_path) => {
                let path_string = module_path
                    .parts
                    .data
                    .iter()
                    .map(|id| interner.lookup(id))
                    .collect::<Vec<_>>()
                    .join(".");

                indent(format!("Module Path: {}", path_string), depth);
            }
            Self::Name(name) => {
                indent("Let:", depth);
                indent(interner.lookup(&name.identifier.data), depth + 1);
                name.expression.data.print(depth + 1, interner);
            }
            Self::Import(import) => {
                let path_string = import
                    .module_path
                    .data
                    .iter()
                    .map(|id| interner.lookup(id))
                    .collect::<Vec<_>>()
                    .join(".");

                indent(format!("Import: {}", path_string), depth);
            }
            Self::Structure(structure) => {
                indent("Structure:", depth);
                indent(interner.lookup(&structure.name.data), depth + 1);
                indent("Constructors:", depth + 1);
                for constructor in &structure.constructors {
                    indent(interner.lookup(&constructor.data.name.data), depth + 2);
                    for argument in &constructor.data.arguments {
                        argument.data.print(interner, depth + 3);
                    }
                }
            }
        }
    }
}

pub struct Module {
    pub definitions: Vec<Definition>,
    pub source_name: String,
}

pub struct Program {
    pub modules: Vec<Module>,
}
