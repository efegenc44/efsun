pub mod name;
pub mod module_path;
pub mod import;
pub mod structure;

use crate::interner::Interner;

pub type ModulePath       = module_path::ModulePath;
pub type Name<State>      = name::Name<State>;
pub type Import           = import::Import;
pub type Structure<State> = structure::Structure<State>;

pub enum Definition<State> {
    ModulePath(ModulePath),
    Name(Name<State>),
    Import(Import),
    Structure(Structure<State>),
}

impl<T> Definition<T> {
    #[allow(unused)]
    pub fn print(&self, interner: &Interner, depth: usize) {
        let indent = depth * 2;

        match self {
            Definition::ModulePath(module_path) => {
                let path_string = module_path
                    .parts()
                    .data()
                    .iter()
                    .map(|id| interner.lookup(id))
                    .collect::<Vec<_>>()
                    .join(".");

                println!("{:indent$}module {}", "", path_string);
            }
            Definition::Name(name) => {
                println!("{:indent$}Let:", "");
                println!(
                    "{:indent$}{}",
                    "",
                    interner.lookup(name.identifier().data()),
                    indent = indent + 2
                );
                name.expression().data().print(depth + 1, interner);
            }
            Definition::Import(import) => {
                println!("{:indent$}Import:", "");
            }
            Definition::Structure(_) => todo!(),
        }
    }
}

pub type Module<T> = Vec<Definition<T>>;
pub type Program<T> = Vec<Module<T>>;
