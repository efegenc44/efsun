use std::fmt::Display;

use crate::{
    compilation::anf,
    interner::{InternId, Interner, WithInterner},
    metadata::{BoundMetadataId, CaptureMetadataId},
    resolution::{bound::Bound, renamer::UniqueName},
};

pub enum Atom {
    String(InternId),
    Path(Path),
    Lambda(Lambda),
}

pub struct Path {
    pub path: anf::Path,
    pub bound: Option<Bound>,
    pub anf_bound_id: BoundMetadataId,
}

pub struct Lambda {
    pub variables: Vec<UniqueName>,
    pub expression: Box<anf::Expression>,
    pub anf_capture_id: CaptureMetadataId,
}

impl Atom {
    pub fn print(&self, depth: usize, interner: &Interner) {
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Self::String(id) => indent(format!("\"{}\"", interner.lookup(id)), depth),
            Self::Path(path) => {
                // let bound = path
                //     .try_bound()
                //     .map(|bound| format!("#{}", WithInterner::new(bound, interner)));

                indent(
                    format!(
                        "{}",
                        WithInterner {
                            data: &path.path,
                            interner
                        },
                        // bound.unwrap_or(String::new())
                    ),
                    depth,
                );
            }
            Self::Lambda(lambda) => {
                // let captures = lambda.try_captures().and_then(|captures| {
                //     (!captures.is_empty()).then(|| {
                //         let mut string = String::from("Captures: [");
                //         for capture in captures {
                //             string.push_str(&capture.to_string());
                //         }
                //         string.push(']');
                //         string
                //     })
                // });

                indent("Lambda:", depth);
                // if let Some(captures) = captures {
                //     indent(captures, depth + 1);
                // }
                for variable in &lambda.variables {
                    indent(variable, depth + 1);
                }
                lambda.expression.print(depth + 1, interner);
            }
        }
    }
}
