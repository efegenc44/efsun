use std::fmt::Display;

use crate::{
    interner::{InternId, Interner},
    location::Located,
    metadata::BoundMetadataId,
};

pub enum TypeExpression {
    Path(Path),
    Application(Application),
}

pub struct Path {
    pub parts: Located<Vec<InternId>>,
    pub bound_id: BoundMetadataId,
}

pub struct Application {
    pub function: Box<Located<TypeExpression>>,
    pub arguments: Vec<Located<TypeExpression>>,
}

impl TypeExpression {
    #[allow(unused)]
    pub fn print(&self, interner: &Interner, depth: usize) {
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Self::Path(path) => {
                let path_string = path
                    .parts
                    .data
                    .iter()
                    .map(|id| interner.lookup(id))
                    .collect::<Vec<_>>()
                    .join(".");

                // let bound = path
                //     .try_bound()
                //     .map(|bound| format!("#{}", WithInterner::new(bound, interner)));

                indent(
                    format!(
                        "Type Identifier: {}",
                        path_string,
                        // bound.unwrap_or(String::new())
                    ),
                    depth,
                );
            }
            Self::Application(application) => {
                indent("Type Application", depth);
                application.function.data.print(interner, depth + 1);
                for argument in &application.arguments {
                    argument.data.print(interner, depth + 1);
                }
            }
        }
    }
}
