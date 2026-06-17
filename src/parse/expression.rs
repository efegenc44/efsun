use std::fmt::Display;

use crate::{
    interner::{InternId, Interner},
    location::Located,
    metadata::{BoundMetadataId, CaptureMetadataId, UniqueNameMetadataId},
    parse::pattern::Pattern,
};

pub enum Expression {
    String(InternId),
    Path(Path),
    Application(Application),
    Lambda(Lambda),
    LetIn(LetIn),
    MatchAs(MatchAs),
}

pub struct Path {
    pub parts: Located<Vec<InternId>>,
    pub bound_id: BoundMetadataId,
    pub unique_name_id: UniqueNameMetadataId,
}

pub struct Application {
    pub function: Box<Located<Expression>>,
    pub argument: Box<Located<Expression>>,
}

pub struct Lambda {
    pub variable: Located<InternId>,
    pub expression: Box<Located<Expression>>,
    pub capture_id: CaptureMetadataId,
    pub unique_name_id: UniqueNameMetadataId,
}

pub struct LetIn {
    pub variable: Located<InternId>,
    pub variable_expression: Box<Located<Expression>>,
    pub return_expression: Box<Located<Expression>>,
    pub unique_name_id: UniqueNameMetadataId,
}

pub struct MatchAs {
    pub expression: Box<Located<Expression>>,
    pub branches: Vec<Located<Branch>>,
}

pub struct Branch {
    pub pattern: Located<Pattern>,
    pub expression: Located<Expression>,
}

impl Expression {
    pub fn print(&self, depth: usize, interner: &Interner) {
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match &self {
            Self::String(string) => {
                indent(format!("\"{}\"", interner.lookup(string)), depth);
            }
            Self::Path(_path) => {
                todo!()
                // let path_string = path
                //     .parts()
                //     .data
                //     .iter()
                //     .map(|id| interner.lookup(id))
                //     .collect::<Vec<_>>()
                //     .join(".");

                // let bound = path
                //     .try_bound()
                //     .map(|bound| format!("#{}", WithInterner::new(bound, interner)));

                // indent(
                //     format!(
                //         "Identifier: {}{}",
                //         path_string,
                //         bound.unwrap_or(String::new())
                //     ),
                //     depth,
                // );
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
                indent(interner.lookup(&lambda.variable.data), depth + 1);
                lambda.expression.data.print(depth + 1, interner);
            }
            Self::Application(application) => {
                indent("Application:", depth);
                application.function.data.print(depth + 1, interner);
                application.argument.data.print(depth + 1, interner);
            }
            Self::LetIn(letin) => {
                indent("Let:", depth);
                indent(interner.lookup(&letin.variable.data), depth + 1);
                letin.variable_expression.data.print(depth + 2, interner);
                letin.return_expression.data.print(depth + 1, interner);
            }
            Self::MatchAs(matchas) => {
                indent("Match:", depth);
                matchas.expression.data.print(depth + 1, interner);
                for branch in &matchas.branches {
                    indent("Branch:", depth + 1);
                    branch.data.pattern.data.print(depth + 2, interner);
                    branch.data.expression.data.print(depth + 2, interner);
                }
            }
        }
    }
}
