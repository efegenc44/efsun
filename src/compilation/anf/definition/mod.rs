pub mod name;
pub mod structure;

use crate::interner::Interner;

pub type Name<State> = name::Name<State>;
pub type Structure = structure::Structure;

pub enum Definition<State> {
    Name(Name<State>),
    Structure(Structure),
}

impl<T> Definition<T> {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        let indent = depth * 2;

        match self {
            Definition::Name(name) => {
                println!("{:indent$}Let:", "");
                println!(
                    "{:indent$}{}",
                    "",
                    interner.lookup(&name.identifier()),
                    indent = indent + 2
                );
                name.expression().print(depth + 1, interner);
            }
            Definition::Structure(_) => todo!(),
        }
    }
}

pub type Module<T> = Vec<Definition<T>>;
pub type Program<T> = Vec<Module<T>>;
