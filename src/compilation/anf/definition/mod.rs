pub mod name;
pub mod structure;

use std::fmt::Display;

use crate::interner::{Interner, WithInterner};

pub type Name<State> = name::Name<State>;
pub type Structure = structure::Structure;

pub enum Definition<State> {
    Name(Name<State>),
    Structure(Structure),
}

impl<T> Definition<T> {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Definition::Name(name) => {
                indent("Name Definition:", depth);
                indent(interner.lookup(&name.identifier()), depth + 1);
                name.expression().print(depth + 1, interner);
            }
            Definition::Structure(structure) => {
                indent("Structure Definition:", depth);
                for constructor in structure.constructors() {
                    indent(WithInterner::new(constructor.path(), interner), depth + 1);
                }
            }
        }
    }
}

pub mod module {
    use crate::compilation::anf::Definition;

    pub struct Module<State> {
        definitions: Vec<Definition<State>>,
    }

    pub struct Observation<State> {
        pub definitions: Vec<Definition<State>>,
    }

    impl<State> From<Observation<State>> for Module<State> {
        fn from(value: Observation<State>) -> Self {
            Self {
                definitions: value.definitions,
            }
        }
    }

    impl<State> Module<State> {
        pub fn definitions(&self) -> &[Definition<State>] {
            &self.definitions
        }

        pub fn observe(self) -> Observation<State> {
            Observation {
                definitions: self.definitions,
            }
        }
    }
}

pub mod program {
    use crate::compilation::anf::Module;

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
