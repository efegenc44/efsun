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
    Let(LetIn<State>),
    Application(Application<State>),
    Match(MatchAs<State>),
    Join(Join<State>),
    Jump(Jump<State>),
    Atom(atom::Atom<State>),
}

impl<State> Expression<State> {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        let indent = 2 * depth;

        match self {
            Expression::Let(letin) => {
                print!(
                    "{:indent$}let {} = ",
                    "",
                    interner.lookup(&letin.variable())
                );
                letin.variable_expression().print(depth, interner);
                println!(" in");
                letin.return_expression().print(depth + 1, interner);
                println!();
            }
            Expression::Application(application) => {
                print!(
                    "{:indent$}let {} = ",
                    "",
                    WithInterner::new(&application.variable(), interner)
                );
                application.function().print(depth, interner);
                application.argument().print(depth, interner);
                println!(" in");
                application.expression().print(depth + 1, interner);
                println!();
            }
            Expression::Match(matchlet) => {
                println!("{:indent$}Match:", "");
                print!(
                    "{:indent$}let {} = ",
                    "",
                    WithInterner::new(&matchlet.variable(), interner)
                );
                matchlet.variable_expression().print(depth, interner);
                println!(" in");
                for branch in matchlet.branches() {
                    branch.expression().print(depth + 1, interner);
                }
                println!();
            }
            Expression::Join(join) => {
                join.join().print(depth + 1, interner);
                println!("{:indent$}Join({}):", "", join.label());
                print!(
                    "{:indent$}let {} = ",
                    "",
                    WithInterner::new(&join.variable(), interner)
                );
                println!(" in");
                join.expression().print(depth + 1, interner);
            }
            Expression::Jump(jump) => {
                println!("{:indent$}Jump: {}", "", jump.to());
                jump.expression().print(depth + 1, interner);
            }
            Expression::Atom(atom) => {
                atom.print(depth, interner);
                println!();
            }
        }
    }
}
