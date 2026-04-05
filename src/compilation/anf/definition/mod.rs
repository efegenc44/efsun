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

pub type Module<T> = Vec<Definition<T>>;
pub type Program<T> = Vec<Module<T>>;
