pub mod any;
pub mod structure;

use std::fmt::Display;

use crate::interner::{InternId, Interner};

pub type Structure = structure::Structure;
pub type Any = any::Any;

#[derive(Clone)]
pub enum Pattern {
    Any(Any),
    Structure(Structure),
    String(InternId),
}

impl Pattern {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Self::Any(any) => {
                todo!()
                // let name = if let Some(unique_name) = any.try_unique_name() {
                //     &unique_name.to_string()
                // } else {
                //     interner.lookup(&any.identifier())
                // };

                // indent(format!("Any: {}", name), depth)
            }
            Self::Structure(structure) => {
                indent("Structure Pattern", depth);
                for argument in structure.arguments() {
                    argument.data().print(depth + 1, interner);
                }
            }
            Self::String(string) => indent(format!("\"{}\"", interner.lookup(string)), depth),
        }
    }

    pub fn local_count(&self) -> usize {
        match self {
            Self::Any(_) => 1,
            Self::Structure(structure) => structure
                .arguments()
                .iter()
                .fold(0, |acc, x| acc + x.data().local_count()),
            Self::String(_) => 0,
        }
    }
}
