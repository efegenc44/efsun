pub mod application;
pub mod lambda;
pub mod letin;
pub mod matchas;
pub mod path;

use std::fmt::Display;

use crate::interner::{InternId, Interner, WithInterner};

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
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Self::String(string) => {
                indent(format!("\"{}\"", interner.lookup(string)), depth);
            }
            Self::Path(path) => {
                let path_string = path
                    .parts()
                    .data()
                    .iter()
                    .map(|id| interner.lookup(id))
                    .collect::<Vec<_>>()
                    .join(".");

                let bound = path
                    .try_bound()
                    .map(|bound| format!("#{}", WithInterner::new(bound, interner)));

                indent(
                    format!(
                        "Identifier: {}{}",
                        path_string,
                        bound.unwrap_or(String::new())
                    ),
                    depth,
                );
            }
            Self::Lambda(lambda) => {
                let captures = lambda.try_captures().and_then(|captures| {
                    (!captures.is_empty()).then(|| {
                        let mut string = String::from("Captures: [");
                        for capture in captures {
                            string.push_str(&capture.to_string());
                        }
                        string.push(']');
                        string
                    })
                });

                indent("Lambda:", depth);
                if let Some(captures) = captures {
                    indent(captures, depth + 1);
                }
                indent(interner.lookup(lambda.variable().data()), depth + 1);
                lambda.expression().data().print(depth + 1, interner);
            }
            Self::Application(application) => {
                indent("Application:", depth);
                application.function().data().print(depth + 1, interner);
                application.argument().data().print(depth + 1, interner);
            }
            Self::LetIn(letin) => {
                indent("Let:", depth);
                indent(interner.lookup(letin.variable().data()), depth + 1);
                letin
                    .variable_expression()
                    .data()
                    .print(depth + 2, interner);
                letin.return_expression().data().print(depth + 1, interner);
            }
            Self::MatchAs(matchas) => {
                indent("Match:", depth);
                matchas.expression().data().print(depth + 1, interner);
                for branch in matchas.branches() {
                    indent("Branch:", depth + 1);
                    branch.data().pattern().data().print(depth + 2, interner);
                    branch.data().expression().data().print(depth + 2, interner);
                }
            }
        }
    }
}
