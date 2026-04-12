pub mod import;
pub mod module_path;
pub mod name;
pub mod structure;

use std::fmt::Display;

use crate::interner::Interner;

pub type ModulePath = module_path::ModulePath;
pub type Name<State> = name::Name<State>;
pub type Import = import::Import;
pub type Structure<State> = structure::Structure<State>;

pub type Module<State> = module::Module<State>;
pub type Program<State> = program::Program<State>;

pub enum Definition<State> {
    ModulePath(ModulePath),
    Name(Name<State>),
    Import(Import),
    Structure(Structure<State>),
}

impl<T> Definition<T> {
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

pub mod module {
    use crate::parse::definition::Definition;

    pub struct Module<State> {
        definitions: Vec<Definition<State>>,
        source_name: String,
    }

    pub struct Observation<State> {
        pub definitions: Vec<Definition<State>>,
        pub source_name: String,
    }

    impl<State> From<Observation<State>> for Module<State> {
        fn from(value: Observation<State>) -> Self {
            Self {
                definitions: value.definitions,
                source_name: value.source_name,
            }
        }
    }

    impl<State> Module<State> {
        pub fn definitions(&self) -> &[Definition<State>] {
            &self.definitions
        }

        pub fn source_name(&self) -> &str {
            &self.source_name
        }

        pub fn observe(self) -> Observation<State> {
            Observation {
                definitions: self.definitions,
                source_name: self.source_name,
            }
        }
    }
}

pub mod program {
    use crate::parse::definition::module::Module;

    pub struct Program<State> {
        modules: Vec<Module<State>>,
    }

    pub struct Observation<State> {
        pub modules: Vec<Module<State>>,
    }

    impl<State> From<Observation<State>> for Program<State> {
        fn from(value: Observation<State>) -> Self {
            Self {
                modules: value.modules,
            }
        }
    }

    impl<State> Program<State> {
        pub fn modules(&self) -> &[Module<State>] {
            &self.modules
        }

        pub fn observe(self) -> Observation<State> {
            Observation {
                modules: self.modules,
            }
        }
    }
}
