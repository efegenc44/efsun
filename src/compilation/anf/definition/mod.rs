pub mod name;
pub mod structure;

use std::fmt::Display;

use crate::interner::Interner;

pub type Name = name::Name;
pub type Structure = structure::Structure;

pub enum Definition {
    Name(Name),
    Structure(Structure),
}

impl Definition {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Self::Name(name) => {
                indent("Name Definition:", depth);
                indent(interner.lookup(&name.identifier()), depth + 1);
                name.expression().print(depth + 1, interner);
            }
            Self::Structure(structure) => {
                indent("Structure Definition:", depth);
                for constructor in structure.constructors() {
                    indent(interner.lookup(&constructor.name()), depth + 1);
                    // indent(WithInterner::new(constructor.path(), interner), depth + 1);
                }
            }
        }
    }
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
