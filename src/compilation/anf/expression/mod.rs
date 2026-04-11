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

pub type LetIn<State> = letin::LetIn<State>;
pub type Application<State> = application::Application<State>;
pub type MatchAs<State> = matchas::MatchAs<State>;
pub type Join<State> = join::Join<State>;
pub type Jump<State> = jump::Jump<State>;

pub enum Expression<State> {
    LetIn(LetIn<State>),
    Application(Application<State>),
    Match(MatchAs<State>),
    Join(Join<State>),
    Jump(Jump<State>),
    Atom(atom::Atom<State>),
}

impl<State> Expression<State> {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        let level = 2 * depth;

        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Expression::LetIn(letin) => {
                indent(format!("let {} =", letin.variable()), depth);
                letin.variable_expression().print(depth + 1, interner);
                indent("in", depth);
                letin.return_expression().print(depth, interner);
            }
            Expression::Application(application) => {
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
            Expression::Match(matchlet) => {
                indent("match", depth);
                matchlet.expression().print(depth + 1, interner);
                for branch in matchlet.branches() {
                    indent("branch:", depth + 1);
                    branch.expression().print(depth + 2, interner);
                }
            }
            Expression::Join(join) => {
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
            Expression::Jump(jump) => {
                indent(format!("jump({})", jump.to()), depth);
                jump.expression().print(depth + 1, interner);
            }
            Expression::Atom(atom) => {
                atom.print(depth, interner);
            }
        }
    }
}
