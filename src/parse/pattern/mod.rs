pub mod any;
pub mod structure;

use crate::interner::{InternId, Interner};

pub type Structure<State> = structure::Structure<State>;
pub type Any<State> = any::Any<State>;

#[derive(Clone)]
pub enum Pattern<State> {
    Any(Any<State>),
    Structure(Structure<State>),
    String(InternId),
}

impl<State> Pattern<State> {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        let indent = depth * 2;

        match self {
            Pattern::Any(id) => todo!(), // println!("{:indent$}{}", "", interner.lookup(id)),
            Pattern::Structure(structure) => {
                println!("{:indent$}Structure Pattern:", "");
                for argument in structure.arguments() {
                    argument.data().print(depth + 1, interner);
                }
            }
            Pattern::String(string) => println!("{:indent$}\"{}\"", "", interner.lookup(string)),
        }
    }
}
