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
            Atom::String(id) => indent(format!("\"{}\"", interner.lookup(id)), depth),
            Atom::Path(path) => {
                let bound_string = if let Some(bound) = path.try_bound() {
                    format!("#{}", WithInterner::new(bound, interner))
                } else {
                    "".to_string()
                };

                indent(
                    format!(
                        "{}{}",
                        WithInterner::new(path.path(), interner),
                        bound_string
                    ),
                    depth,
                );
            }
            Atom::Lambda(lambda) => {
                let mut captures_string = String::new();
                if let Some(captures) = lambda.try_captures()
                    && !captures.is_empty()
                {
                    captures_string.push_str("Captures: [");
                    for capture in captures {
                        captures_string.push_str(&capture.to_string());
                    }
                    captures_string.push(']');
                }

                indent("Lambda:", depth);
                if !captures_string.is_empty() {
                    indent(captures_string, depth + 1);
                }
                indent(lambda.variable(), depth + 1);
                lambda.expression().print(depth + 1, interner);
            }
        }
    }
}
