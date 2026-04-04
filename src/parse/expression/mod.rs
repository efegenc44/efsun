pub mod application;
pub mod lambda;
pub mod letin;
pub mod matchas;
pub mod path;

use crate::interner::{InternId, Interner};

pub type Application<State> = application::Application<State>;
pub type Lambda<State> = lambda::Lambda<State>;
pub type LetIn<State> = letin::LetIn<State>;
pub type MatchAs<State> = matchas::MatchAs<State>;
pub type Path<State> = path::Path<State>;

pub enum Expression<State> {
    String(InternId),
    Path(Path<State>),
    Application(Application<State>),
    Lambda(Lambda<State>),
    LetIn(LetIn<State>),
    MatchAs(MatchAs<State>),
}

impl<T> Expression<T> {
    pub fn print(&self, depth: usize, interner: &Interner) {
        let indent = depth * 2;

        match self {
            Self::String(string) => {
                println!("{:indent$}\"{}\"", "", interner.lookup(string));
            }
            Self::Path(path) => {
                let path_string = path
                    .parts()
                    .data()
                    .iter()
                    .map(|id| interner.lookup(id))
                    .collect::<Vec<_>>()
                    .join(".");

                println!(
                    "{:indent$}Identifier: {}",
                    "",
                    path_string,
                    // if let Some(bound) = &path.bound {
                    //     format!("#{}", WithInterner::new(bound, interner))
                    // } else {
                    //     "".to_string()
                    // }
                )
            }
            Self::Lambda(lambda) => {
                println!("{:indent$}Lambda:", "");
                // if let Some(captures) = &lambda.captures
                //     && !captures.is_empty()
                // {
                //     print!("{:indent$}Captures: ", "", indent = indent + 2);
                //     for capture in captures {
                //         print!("{} ", capture);
                //     }
                //     println!()
                // }
                println!(
                    "{:indent$}{}",
                    "",
                    interner.lookup(lambda.variable().data()),
                    indent = indent + 2
                );
                lambda.expression().data().print(depth + 1, interner);
            }
            Self::Application(application) => {
                println!("{:indent$}Application:", "");
                application.function().data().print(depth + 1, interner);
                application.argument().data().print(depth + 1, interner);
            }
            Self::LetIn(letin) => {
                println!("{:indent$}Let:", "");
                println!(
                    "{:indent$}{}",
                    "",
                    interner.lookup(letin.variable().data()),
                    indent = indent + 2
                );
                letin
                    .variable_expression()
                    .data()
                    .print(depth + 2, interner);
                letin.return_expression().data().print(depth + 1, interner);
            }
            Self::MatchAs(_) => todo!(),
        }
    }
}
