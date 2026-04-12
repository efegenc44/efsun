use std::fmt::Display;

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
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Self::String(id) => indent(format!("\"{}\"", interner.lookup(id)), depth),
            Self::Path(path) => {
                let bound = path
                    .try_bound()
                    .map(|bound| format!("#{}", WithInterner::new(bound, interner)));

                indent(
                    format!(
                        "{}{}",
                        WithInterner::new(path.path(), interner),
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
                indent(lambda.variable(), depth + 1);
                lambda.expression().print(depth + 1, interner);
            }
        }
    }
}
