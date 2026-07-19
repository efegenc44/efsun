use std::fmt::Display;

use crate::{
    compilation::anf::{self, atom},
    interner::{Interner, WithInterner},
    parse::pattern::Pattern,
    resolution::renamer::UniqueName,
};

pub enum Expression {
    LetIn(LetIn),
    Application(Application),
    MatchAs(MatchAs),
    Join(Join),
    Jump(Jump),
    Atom(atom::Atom),
}

pub struct Application {
    pub variable: anf::Local,
    pub function: anf::Atom,
    pub arguments: Vec<anf::Atom>,
    pub expression: Box<anf::Expression>,
}

pub struct LetIn {
    pub variable: UniqueName,
    pub variable_expression: anf::Atom,
    pub return_expression: Box<anf::Expression>,
}

pub struct MatchAs {
    pub expression: anf::Atom,
    pub branches: Vec<Branch>,
}

pub struct Branch {
    pub pattern: Pattern,
    pub expression: anf::Expression,
}

pub struct Join {
    pub label: usize,
    pub variable: anf::Local,
    pub join: Box<anf::Expression>,
    pub expression: Box<anf::Expression>,
}

pub struct Jump {
    pub to: usize,
    pub expression: anf::Atom,
}

impl Expression {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Self::LetIn(letin) => {
                indent(format!("let {} =", letin.variable), depth);
                letin.variable_expression.print(depth + 1, interner);
                indent("in", depth);
                letin.return_expression.print(depth, interner);
            }
            Self::Application(application) => {
                indent(
                    format!(
                        "let application {} =",
                        WithInterner {
                            data: &application.variable,
                            interner
                        }
                    ),
                    depth,
                );
                application.function.print(depth + 1, interner);
                for argument in &application.arguments {
                    argument.print(depth + 1, interner);
                }
                indent("in", depth);
                application.expression.print(depth, interner);
            }
            Self::MatchAs(matchas) => {
                indent("match", depth);
                matchas.expression.print(depth + 1, interner);
                for branch in &matchas.branches {
                    indent("branch:", depth + 1);
                    branch.pattern.print(depth + 2, interner);
                    branch.expression.print(depth + 2, interner);
                }
            }
            Self::Join(join) => {
                indent(
                    format!(
                        "let join({}) {} =",
                        join.label,
                        WithInterner {
                            data: &join.variable,
                            interner
                        }
                    ),
                    depth,
                );
                join.expression.print(depth + 1, interner);
                indent("in", depth);
                join.join.print(depth, interner);
            }
            Self::Jump(jump) => {
                indent(format!("jump({})", jump.to), depth);
                jump.expression.print(depth + 1, interner);
            }
            Self::Atom(atom) => {
                atom.print(depth, interner);
            }
        }
    }
}
