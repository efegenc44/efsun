pub mod atom;
pub mod definition;
pub mod expression;

use std::{cell::RefCell, fmt::Display};

use definition::{self as anf_definition};
use expression::{self as anf_expression};

use crate::{
    interner::{InternId, WithInterner},
    parse::{
        definition::{self as ast_definition, Definition as ASTDefinition, Module, Program},
        expression::{self as ast_expression, Expression as ASTExpression},
    },
    resolution::{Renamed, Unresolved, bound::Bound},
};

pub type Definition<State> = definition::Definition<State>;
pub type Atom<State> = atom::Atom<State>;
pub type Expression<State> = expression::Expression<State>;

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ANFLocal {
    ANF(usize),
    Normal(InternId),
}

impl<'interner> Display for WithInterner<'interner, &ANFLocal> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            ANFLocal::ANF(id) => write!(f, "<ANF{id}>"),
            ANFLocal::Normal(identifier) => write!(f, "{}", self.interner().lookup(identifier)),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum ANFPath {
    ANFLocal(usize),
    Normal(Vec<InternId>),
}

impl<'interner> Display for WithInterner<'interner, &ANFPath> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            ANFPath::ANFLocal(id) => write!(f, "<ANF{id}>"),
            ANFPath::Normal(parts) => match parts.as_slice() {
                [] => unreachable!(),
                [identifier] => {
                    write!(f, "{}", self.interner().lookup(identifier))
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

pub struct ANFTransformer {
    counter: RefCell<usize>,
}

impl ANFTransformer {
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

    pub fn program(&self, program: Program<Renamed>) -> anf_definition::Program<Unresolved> {
        program
            .into_iter()
            .map(|module| self.module(module))
            .collect()
    }

    pub fn module(&self, module: Module<Renamed>) -> anf_definition::Module<Unresolved> {
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

    fn name_definition(
        &self,
        name_definition: ast_definition::Name<Renamed>,
    ) -> Definition<Unresolved> {
        let ast_definition::name::RenamedObservation {
            identifier,
            expression,
            path,
        } = name_definition.observe();

        let expression = self.transform(expression.destruct().0);

        let name = anf_definition::name::Observation {
            identifier: identifier.into_data(),
            expression,
            path,
        }
        .into();

        Definition::Name(name)
    }

    fn structure_definition(
        &self,
        structure_definition: ast_definition::Structure<Renamed>,
    ) -> Definition<Unresolved> {
        let ast_definition::structure::RenamedObservation { constructors, .. } =
            structure_definition.observe();

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

        let structure = anf_definition::Structure::new(constructors);
        Definition::Structure(structure)
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
            path: ANFPath::Normal(parts.into_data()),
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

        self.expression(
            argument.into_data(),
            Box::new(|argument| {
                self.expression(
                    function.into_data(),
                    Box::new(|function| {
                        let id = self.new_local_id();
                        let path = Atom::Path(
                            atom::path::UnresolvedObservation {
                                path: ANFPath::ANFLocal(id),
                                bound: None,
                            }
                            .into(),
                        );
                        let application = anf_expression::application::Observation {
                            variable: ANFLocal::ANF(id),
                            function,
                            argument,
                            expression: k(path),
                        }
                        .into();

                        Expression::Application(application)
                    }),
                )
            }),
        )
    }

    fn lambda(
        &self,
        lambda: ast_expression::Lambda<Renamed>,
        k: Continuation,
    ) -> Expression<Unresolved> {
        let ast_expression::lambda::RenamedObservation {
            variable,
            expression,
            captures: _,
        } = lambda.observe();

        k(Atom::Lambda(
            atom::lambda::UnresolvedObservation {
                variable: variable.into_data(),
                expression: self.transform(expression.into_data()),
            }
            .into(),
        ))
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

        self.expression(
            variable_expression.into_data(),
            Box::new(|variable_expression| {
                Expression::Let(
                    anf_expression::letin::Observation {
                        variable: variable.into_data(),
                        variable_expression,
                        return_expression: self.expression(return_expression.into_data(), k),
                    }
                    .into(),
                )
            }),
        )
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

        self.expression(
            expression.into_data(),
            Box::new(|expression| {
                let join_label_id = self.new_local_id();

                let join_var_id = self.new_local_id();
                let join_var_path = Atom::Path(
                    atom::path::UnresolvedObservation {
                        path: ANFPath::ANFLocal(join_var_id),
                        bound: None,
                    }
                    .into(),
                );

                let id = self.new_local_id();

                let mut branch_anfs = Vec::new();
                for branch in branches {
                    let ast_expression::matchas::branch::Observation {
                        pattern,
                        expression: branch_expression,
                    } = branch.into_data().observe();

                    let path = Atom::Path(
                        atom::path::UnresolvedObservation {
                            path: ANFPath::ANFLocal(id),
                            bound: None,
                        }
                        .into(),
                    );

                    let k = |expression| {
                        Expression::Jump(
                            anf_expression::jump::Observation {
                                to: join_label_id,
                                expression,
                            }
                            .into(),
                        )
                    };
                    let branch_anf = self.expression(branch_expression.into_data(), Box::new(k));
                    let branch = anf_expression::matchas::branch::Observation {
                        pattern: pattern.into_data(),
                        matched: path,
                        expression: branch_anf,
                    }
                    .into();
                    branch_anfs.push(branch);
                }

                Expression::Join(
                    anf_expression::join::Observation {
                        label: join_label_id,
                        variable: ANFLocal::ANF(join_var_id),
                        join: Expression::Match(
                            anf_expression::matchas::Observation {
                                variable: ANFLocal::ANF(id),
                                variable_expression: expression,
                                branches: branch_anfs,
                            }
                            .into(),
                        ),
                        expression: k(join_var_path),
                    }
                    .into(),
                )
            }),
        )
    }

    pub fn transform(&self, expression: ASTExpression<Renamed>) -> Expression<Unresolved> {
        self.expression(expression, Box::new(Expression::Atom))
    }
}
