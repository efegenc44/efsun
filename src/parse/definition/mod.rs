pub mod import;
pub mod module_path;
pub mod name;
pub mod structure;

use std::fmt::Display;

use crate::{compilation::anf, interner::Interner};

pub type ModulePath = module_path::ModulePath;
pub type Name = name::Name;
pub type Import = import::Import;
pub type Structure = structure::Structure;

pub enum Definition {
    ModulePath(ModulePath),
    Name(Name),
    Import(Import),
    Structure(Structure),
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
                    .parts()
                    .data()
                    .iter()
                    .map(|id| interner.lookup(id))
                    .collect::<Vec<_>>()
                    .join(".");

                indent(format!("Module Path: {}", path_string), depth);
            }
            Self::Name(name) => {
                indent("Let:", depth);
                indent(interner.lookup(name.identifier().data()), depth + 1);
                name.expression().data().print(depth + 1, interner);
            }
            Self::Import(import) => {
                let path_string = import
                    .module_path()
                    .data()
                    .iter()
                    .map(|id| interner.lookup(id))
                    .collect::<Vec<_>>()
                    .join(".");

                indent(format!("Import: {}", path_string), depth);
            }
            Self::Structure(structure) => {
                indent("Structure:", depth);
                indent(interner.lookup(structure.name().data()), depth + 1);
                indent("Constructors:", depth + 1);
                for constructor in structure.constructors() {
                    indent(interner.lookup(constructor.data().name().data()), depth + 2);
                    for argument in constructor.data().arguments() {
                        argument.data().print(interner, depth + 3);
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

impl Module {
    pub fn new(definitions: Vec<Definition>, source_name: String) -> Self {
        Self {
            definitions,
            source_name,
        }
    }

    pub fn definitions(&self) -> &[Definition] {
        &self.definitions
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }

    pub fn into_anf(self, transformer: &anf::Transformer) -> anf::Module {
        let Self { definitions, .. } = self;

        let mut anf_module = Vec::new();

        for definition in definitions {
            match definition {
                Definition::Name(name_definition) => {
                    anf_module.push(name_definition.into_anf(transformer));
                }
                Definition::Structure(structure_definition) => {
                    anf_module.push(structure_definition.into_anf());
                }
                _ => (),
            }
        }

        anf::definition::Module::new(anf_module)
    }
}

pub struct Program {
    pub modules: Vec<Module>,
}

impl Program {
    pub fn new(modules: Vec<Module>) -> Self {
        Self { modules }
    }

    pub fn modules(&self) -> &[Module] {
        &self.modules
    }

    pub fn into_anf(self, transformer: &anf::Transformer) -> anf::Program {
        let Self { modules } = self;

        let modules = modules
            .into_iter()
            .map(|module| module.into_anf(transformer))
            .collect::<Vec<_>>();

        anf::definition::Program::new(modules)
    }
}
