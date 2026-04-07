pub mod atom;
pub mod definition;
pub mod expression;

use std::{cell::RefCell, fmt::Display};

use definition::{self as anf_definition};
use expression::{self as anf_expression};

use crate::{
    interner::{InternId, WithInterner},
    parse::{
        definition::{
            self as ast_definition, Definition as ASTDefinition, Module as ASTModule,
            Program as ASTProgram,
        },
        expression::{self as ast_expression, Expression as ASTExpression},
    },
    resolution::{Renamed, Unresolved, bound::Bound},
};

pub type Definition<State> = definition::Definition<State>;
pub type Atom<State> = atom::Atom<State>;
pub type Expression<State> = expression::Expression<State>;

pub type Module<State> = definition::Module<State>;
pub type Program<State> = definition::Program<State>;

// TODO: Maybe don't seperate ANF and standard as in enum Local
// and enum Path, but keep generating unique InternId's (or something)
// it can simplify somethings but will also hide others

/// Possible local variables in an ANF expression
/// Either an ANF-intorduced local or a standard
/// lexical local name from the AST
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Local {
    ANFLocal(usize),
    Standard(InternId),
}

impl<'interner> Display for WithInterner<'interner, &Local> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            Local::ANFLocal(id) => write!(f, "<ANF{id}>"),
            Local::Standard(identifier) => write!(f, "{identifier}"),
        }
    }
}

/// Possible _lexical path_ element in an ANF expression
/// Either an ANF-introduced local or a standart
/// syntactic path element from the AST
#[derive(Clone, PartialEq, Eq)]
pub enum Path {
    ANFLocal(usize),
    // bool field indicates if the bound is local or not
    //   for purely debug purposes
    Standard(Vec<InternId>, bool),
}

impl<'interner> Display for WithInterner<'interner, &Path> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            Path::ANFLocal(id) => write!(f, "<ANF{id}>"),
            Path::Standard(parts, is_local) => match parts.as_slice() {
                [] => unreachable!(),
                [identifier] => {
                    if *is_local {
                        write!(f, "{identifier}")
                    } else {
                        write!(f, "{}", self.interner().lookup(identifier))
                    }
                }
                [x, xs @ ..] => {
                    write!(f, "{}", self.interner().lookup(x))?;
                    for x in xs {
                        write!(f, ".{}", self.interner().lookup(x))?;
                    }

                    Ok(())
                }
            },
        }
    }
}

type Continuation<'a> = Box<dyn FnOnce(Atom<Unresolved>) -> Expression<Unresolved> + 'a>;

/// AST to ANF Transformer
pub struct Transformer {
    /// State for ANF-introduced local variables and jump labels
    counter: RefCell<usize>,
}

impl Transformer {
    pub fn new() -> Self {
        Self {
            counter: RefCell::new(0),
        }
    }

    fn new_local_id(&self) -> usize {
        let id = *self.counter.borrow();
        *self.counter.borrow_mut() += 1;
        id
    }

    fn new_anf_local(&self) -> (Local, atom::Path<Unresolved>) {
        let id = self.new_local_id();

        let local = Local::ANFLocal(id);
        let path = atom::path::UnresolvedObservation {
            path: Path::ANFLocal(id),
            bound: None,
        };

        (local, path.into())
    }

    pub fn program(&self, program: ASTProgram<Renamed>) -> Program<Unresolved> {
        program
            .into_iter()
            .map(|module| self.module(module))
            .collect()
    }

    pub fn module(&self, module: ASTModule<Renamed>) -> Module<Unresolved> {
        let mut anf_module = Vec::new();

        for definition in module {
            match definition {
                ASTDefinition::Name(let_definition) => {
                    anf_module.push(self.name_definition(let_definition));
                }
                ASTDefinition::Structure(structure_definition) => {
                    anf_module.push(self.structure_definition(structure_definition));
                }
                _ => (),
            }
        }

        anf_module
    }

    fn name_definition(&self, name: ast_definition::Name<Renamed>) -> Definition<Unresolved> {
        let ast_definition::name::RenamedObservation {
            identifier,
            expression,
            path,
        } = name.observe();

        let name = anf_definition::name::Observation {
            identifier: identifier.into_data(),
            expression: self.transform(expression.into_data()),
            path,
        };

        Definition::Name(name.into())
    }

    fn structure_definition(
        &self,
        structure: ast_definition::Structure<Renamed>,
    ) -> Definition<Unresolved> {
        let ast_definition::structure::RenamedObservation { constructors, .. } =
            structure.observe();

        let constructors = constructors
            .into_iter()
            .map(|constructor| {
                let ast_definition::structure::constructor::RenamedObservation {
                    arguments,
                    path,
                    ..
                } = constructor.into_data().observe();

                anf_definition::structure::Constructor::new(path, arguments.len())
            })
            .collect();

        Definition::Structure(anf_definition::structure::Structure::new(constructors))
    }

    fn expression(
        &self,
        expression: ASTExpression<Renamed>,
        k: Continuation,
    ) -> Expression<Unresolved> {
        match expression {
            ASTExpression::String(id) => k(Atom::String(id)),
            ASTExpression::Path(path) => self.path(path, k),
            ASTExpression::Application(application) => self.application(application, k),
            ASTExpression::Lambda(lambda) => self.lambda(lambda, k),
            ASTExpression::LetIn(letin) => self.letin(letin, k),
            ASTExpression::MatchAs(matchas) => self.matchas(matchas, k),
        }
    }

    fn path(&self, path: ast_expression::Path<Renamed>, k: Continuation) -> Expression<Unresolved> {
        let ast_expression::path::RenamedObservation { parts, bound } = path.observe();

        let bound = match &bound {
            Bound::Absolute(_) => Some(bound),
            Bound::Local(_) | Bound::Capture(_) => None,
        };

        let path = atom::path::UnresolvedObservation {
            path: Path::Standard(parts.into_data(), bound.is_none()),
            bound,
        };

        k(Atom::Path(path.into()))
    }

    fn application(
        &self,
        application: ast_expression::Application<Renamed>,
        k: Continuation,
    ) -> Expression<Unresolved> {
        let ast_expression::application::Observation { function, argument } = application.observe();

        #[rustfmt::skip]
        let result = self.expression(argument.into_data(), Box::new(|argument| {
            self.expression(function.into_data(), Box::new(|function| {
                let (variable, path) = self.new_anf_local();

                let application = anf_expression::application::Observation {
                    variable,
                    function,
                    argument,
                    expression: k(Atom::Path(path)),
                };

                Expression::Application(application.into())
            }))
        }));

        result
    }

    fn lambda(
        &self,
        lambda: ast_expression::Lambda<Renamed>,
        k: Continuation,
    ) -> Expression<Unresolved> {
        let ast_expression::lambda::RenamedObservation {
            variable,
            expression,
            ..
        } = lambda.observe();

        let lambda = atom::lambda::UnresolvedObservation {
            variable: variable.into_data(),
            expression: self.transform(expression.into_data()),
        };

        k(Atom::Lambda(lambda.into()))
    }

    fn letin(
        &self,
        letin: ast_expression::LetIn<Renamed>,
        k: Continuation,
    ) -> Expression<Unresolved> {
        let ast_expression::letin::Observation {
            variable,
            variable_expression,
            return_expression,
        } = letin.observe();

        #[rustfmt::skip]
        let result = self.expression(variable_expression.into_data(), Box::new(|variable_expression| {
            let letin = anf_expression::letin::Observation {
                variable: variable.into_data(),
                variable_expression,
                return_expression: self.expression(return_expression.into_data(), k),
            };

            Expression::LetIn(letin.into())
        }));

        result
    }

    fn matchas(
        &self,
        matchas: ast_expression::MatchAs<Renamed>,
        k: Continuation,
    ) -> Expression<Unresolved> {
        let ast_expression::matchas::Observation {
            expression,
            branches,
        } = matchas.observe();

        #[rustfmt::skip]
        let result = self.expression(expression.into_data(), Box::new(|expression| {
            let label = self.new_local_id();

            let branch_k = |expression| {
                let jump = anf_expression::jump::Observation {
                    to: label,
                    expression,
                };

                Expression::Jump(jump.into())
            };

            let branches = branches
                .into_iter()
                .map(|branch| self.match_branch(branch.into_data(), Box::new(branch_k)))
                .collect();

            let matchas = anf_expression::matchas::Observation {
                expression,
                branches,
            };

            let (variable, path) = self.new_anf_local();

            let join = anf_expression::join::Observation {
                label,
                variable,
                join: Expression::Match(matchas.into()),
                expression: k(Atom::Path(path)),
            };

            Expression::Join(join.into())
        }));

        result
    }

    fn match_branch(
        &self,
        branch: ast_expression::matchas::Branch<Renamed>,
        k: Continuation,
    ) -> anf_expression::matchas::Branch<Unresolved> {
        let ast_expression::matchas::branch::Observation {
            pattern,
            expression,
        } = branch.observe();

        anf_expression::matchas::branch::Observation {
            pattern: pattern.into_data(),
            expression: self.expression(expression.into_data(), k),
        }
        .into()
    }

    pub fn transform(&self, expression: ASTExpression<Renamed>) -> Expression<Unresolved> {
        self.expression(expression, Box::new(Expression::Atom))
    }
}
