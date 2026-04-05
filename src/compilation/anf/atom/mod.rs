use crate::interner::{InternId, Interner, WithInterner};

pub mod lambda;
pub mod path;

pub type Path<State> = path::Path<State>;
pub type Lambda<State> = lambda::Lambda<State>;

pub enum Atom<State> {
    String(InternId),
    Path(Path<State>),
    Lambda(Lambda<State>),
}

impl<State> Atom<State> {
    pub fn print(&self, depth: usize, interner: &Interner) {
        let indent = 2 * depth;

        match self {
            Atom::String(id) => print!("{:indent$}\"{}\"", "", interner.lookup(id)),
            Atom::Path(path) => {
                print!(
                    "{:indent$}{}",
                    "",
                    WithInterner::new(path.path(), interner),
                    // if let Some(bound) = path.bound() {
                    //     format!("#{}", WithInterner::new(bound, interner))
                    // } else {
                    //     "".to_string()
                    // }
                )
            }
            Atom::Lambda(lambda) => {
                print!("{:indent$}\\{} ", "", interner.lookup(&lambda.variable()));
                // if let Some(captures) = lambda.captures()
                //     && !captures.is_empty()
                // {
                //     print!("[ ");
                //     for capture in captures {
                //         print!("{} ", capture);
                //     }
                //     print!("]");
                // }
                lambda.expression().print(0, interner);
            }
        }
    }
}
