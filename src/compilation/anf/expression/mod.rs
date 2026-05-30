use std::fmt::Display;

use crate::{
    compilation::anf::atom,
    interner::{Interner, WithInterner},
};

pub mod application;
pub mod join;
pub mod jump;
pub mod letin;
pub mod matchas;

pub type LetIn = letin::LetIn;
pub type Application = application::Application;
pub type MatchAs = matchas::MatchAs;
pub type Join = join::Join;
pub type Jump = jump::Jump;

pub enum Expression {
    LetIn(LetIn),
    Application(Application),
    Match(MatchAs),
    Join(Join),
    Jump(Jump),
    Atom(atom::Atom),
}

impl Expression {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Self::LetIn(letin) => {
                indent(format!("let {} =", letin.variable()), depth);
                letin.variable_expression().print(depth + 1, interner);
                indent("in", depth);
                letin.return_expression().print(depth, interner);
            }
            Self::Application(application) => {
                indent(
                    format!(
                        "let application {} =",
                        WithInterner::new(&application.variable(), interner)
                    ),
                    depth,
                );
                application.function().print(depth + 1, interner);
                application.argument().print(depth + 1, interner);
                indent("in", depth);
                application.expression().print(depth, interner);
            }
            Self::Match(matchlet) => {
                indent("match", depth);
                matchlet.expression().print(depth + 1, interner);
                for branch in matchlet.branches() {
                    indent("branch:", depth + 1);
                    branch.pattern().print(depth + 2, interner);
                    branch.expression().print(depth + 2, interner);
                }
            }
            Self::Join(join) => {
                indent(
                    format!(
                        "let join({}) {} =",
                        join.label(),
                        WithInterner::new(&join.variable(), interner)
                    ),
                    depth,
                );
                join.expression().print(depth + 1, interner);
                indent("in", depth);
                join.join().print(depth, interner);
            }
            Self::Jump(jump) => {
                indent(format!("jump({})", jump.to()), depth);
                jump.expression().print(depth + 1, interner);
            }
            Self::Atom(atom) => {
                atom.print(depth, interner);
            }
        }
    }
}
