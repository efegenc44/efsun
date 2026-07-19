pub mod atom;
pub mod definition;
pub mod expression;

use std::{
    cell::{Cell, RefCell},
    fmt::Display,
    rc::Rc,
};

use definition::{self as anf_definition};
use expression::{self as anf_expression};

use crate::{
    interner::{InternId, WithInterner},
    metadata::{Generator, Indicies, Metadata},
    parse::{
        definition::{
            self as ast_definition, Definition as ASTDefinition, Module as ASTModule,
            Program as ASTProgram,
        },
        expression::{self as ast_expression, Expression as ASTExpression},
    },
    resolution::{
        bound::Bound,
        renamer::{Renamed, UniqueName},
    },
};

pub type Definition = definition::Definition;
pub type Atom = atom::Atom;
pub type Expression = expression::Expression;

pub type Module = definition::Module;
pub type Program = definition::Program;

// TODO: Maybe don't seperate ANF and standard as in enum Local
// and enum Path, but keep generating unique InternId's (or something)
// it can simplify somethings but will also hide others

/// Possible local variables in an ANF expression
/// Either an ANF-intorduced local or a standard
/// lexical local name from the AST
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Local {
    ANFLocal(usize),
    Standard(UniqueName),
}

impl<'interner> Display for WithInterner<'interner, &Local> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
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
    Absolute(Vec<InternId>),
    Local(UniqueName),
}

impl<'interner> Display for WithInterner<'interner, &Path> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;

        match &self.data {
            Path::ANFLocal(id) => write!(f, "<ANF{id}>"),
            Path::Local(unique_name) => {
                write!(f, "{unique_name}")
            }
            Path::Absolute(parts) => match parts.as_slice() {
                [] => unreachable!(),
                [x, xs @ ..] => {
                    write!(f, "{}", interner.lookup(x))?;
                    for x in xs {
                        write!(f, ".{}", interner.lookup(x))?;
                    }

                    Ok(())
                }
            },
        }
    }
}

pub type Continuation<'a> = Box<dyn FnOnce(Atom) -> Expression + 'a>;

/// AST to ANF Transformer
pub struct Transformer<'metadata> {
    /// State for ANF-introduced local variables and jump labels
    counter: Cell<usize>,
    /// Metadata
    metadata: &'metadata Metadata<Renamed>,
    /// Indicies for metadata
    indicies: RefCell<Indicies>,
}

impl<'metadata> Transformer<'metadata> {
    pub fn new(metadata: &'metadata Metadata<Renamed>, indicies: Indicies) -> Self {
        Self {
            counter: Cell::new(0),
            metadata,
            indicies: RefCell::new(indicies),
        }
    }

    fn new_local_id(&self) -> usize {
        let id = self.counter.get();
        self.counter.update(|x| x + 1);
        id
    }

    fn new_anf_local(&self) -> (Local, atom::Path) {
        let id = self.new_local_id();

        let local = Local::ANFLocal(id);

        let path = atom::Path {
            path: Path::ANFLocal(id),
            bound: None,
            anf_bound_id: self.indicies.borrow_mut().get(),
        };

        (local, path)
    }

    pub fn program(&self, program: ASTProgram) -> Program {
        let ASTProgram { modules } = program;

        let modules = modules
            .into_iter()
            .map(|module| self.module(module))
            .collect::<Vec<_>>();

        Program::new(modules)
    }

    pub fn module(&self, module: ASTModule) -> Module {
        let ASTModule { definitions, .. } = module;

        let mut anf_module = Vec::new();

        for definition in definitions {
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

        Module::new(anf_module)
    }

    fn name_definition(&self, name: ast_definition::Name) -> Definition {
        let ast_definition::Name {
            identifier,
            expression,
            path_id,
        } = name;

        let name = anf_definition::Name {
            identifier: identifier.data,
            expression: self.transform(expression.data),
            path_id,
        };

        Definition::Name(name)
    }

    fn structure_definition(&self, structure: ast_definition::Structure) -> Definition {
        let ast_definition::Structure { constructors, .. } = structure;

        let constructors = constructors
            .into_iter()
            .map(|constructor| {
                let ast_definition::Constructor {
                    name,
                    arguments,
                    path_id,
                    ..
                } = constructor.data;

                anf_definition::Constructor {
                    name: name.data,
                    arity: arguments.len(),
                    path_id,
                }
            })
            .collect();

        Definition::Structure(anf_definition::Structure { constructors })
    }

    fn expression(&self, expression: ASTExpression, k: Continuation) -> Expression {
        match expression {
            ASTExpression::String(id) => k(Atom::String(id)),
            ASTExpression::Path(path) => self.path(path, k),
            ASTExpression::Application(application) => self.application(application, k),
            ASTExpression::Lambda(lambda) => self.lambda(lambda, k),
            ASTExpression::LetIn(letin) => self.letin(letin, k),
            ASTExpression::MatchAs(matchas) => self.matchas(matchas, k),
        }
    }

    fn path(&self, path: ast_expression::Path, k: Continuation) -> Expression {
        let ast_expression::Path {
            parts,
            bound_id,
            unique_name_id,
        } = path;

        let bound = &self.metadata[bound_id];
        let bound = match bound {
            Bound::Absolute(_) => Some(bound.clone()),
            Bound::Local(_) | Bound::Capture(_) => None,
        };

        let unique_name = self.metadata[unique_name_id];
        let path = match unique_name {
            Some(unique_name) => Path::Local(unique_name),
            None => Path::Absolute(parts.data),
        };

        let path = atom::Path {
            path,
            bound,
            anf_bound_id: self.indicies.borrow_mut().get(),
        };

        k(Atom::Path(path))
    }

    fn application(&self, application: ast_expression::Application, k: Continuation) -> Expression {
        let ast_expression::Application {
            mut function,
            argument,
        } = application;

        let mut arguments = vec![argument.data];
        while let ASTExpression::Application(successive) = function.data {
            arguments.push(successive.argument.data);
            function = successive.function;
        }

        #[rustfmt::skip]
        let result = self.expression(function.data, Box::new(|function| {
            let (variable, path) = self.new_anf_local();

            type Accumulation<'a> = Box<dyn FnOnce(Rc<RefCell<Vec<Atom>>>) -> Expression + 'a>;

            let mut application: Accumulation = Box::new(|arguments| {
                let arguments = Rc::into_inner(arguments).unwrap().into_inner();

                Expression::Application(anf_expression::Application {
                    variable,
                    function,
                    arguments,
                    expression: Box::new(k(Atom::Path(path))),
                })
            });

            let atom_arguments = Rc::new(RefCell::new(vec![]));
            for argument in arguments {
                let bar = atom_arguments.clone();
                application = Box::new(move |arguments| self.expression(argument, Box::new(|argument| {
                    bar.borrow_mut().push(argument);
                    // NOTE: This drop is needed otherwise Rc::into_inner runs before dropping references
                    //  at the end of the scope and consequently Rc::into_inner fails as reference count != 1.
                    //  So it is safely droped before Rust does.
                    drop(bar);
                    application(arguments)
                })));
            }

            application(atom_arguments)
        }));

        result
    }

    fn lambda(&self, lambda: ast_expression::Lambda, k: Continuation) -> Expression {
        let ast_expression::Lambda {
            mut expression,
            unique_name_id,
            ..
        } = lambda;

        let mut variables = vec![self.metadata[unique_name_id].unwrap()];
        while let ASTExpression::Lambda(successive) = expression.data {
            variables.push(self.metadata[successive.unique_name_id].unwrap());
            expression = successive.expression;
        }

        let lambda = atom::Lambda {
            variables,
            expression: Box::new(self.transform(expression.data)),
            anf_capture_id: self.indicies.borrow_mut().get(),
        };

        k(Atom::Lambda(lambda))
    }

    fn letin(&self, letin: ast_expression::LetIn, k: Continuation) -> Expression {
        let ast_expression::LetIn {
            variable_expression,
            return_expression,
            unique_name_id,
            ..
        } = letin;

        #[rustfmt::skip]
        let result = self.expression(variable_expression.data, Box::new(|variable_expression| {
            let letin = anf_expression::LetIn {
                variable: self.metadata[unique_name_id].unwrap(),
                variable_expression,
                return_expression: Box::new(self.expression(return_expression.data, k)),
            };

            Expression::LetIn(letin)
        }));

        result
    }

    fn matchas(&self, matchas: ast_expression::MatchAs, k: Continuation) -> Expression {
        let ast_expression::MatchAs {
            expression,
            branches,
        } = matchas;

        #[rustfmt::skip]
        let result = self.expression(expression.data, Box::new(|expression| {
            let label = self.new_local_id();

            let branch_k = |expression| {
                let jump = anf_expression::Jump {
                    expression,
                    to: label,
                };
                Expression::Jump(jump)
            };

            let branches = branches
                .into_iter()
                .map(|branch| self.match_branch(branch.data, Box::new(branch_k)))
                .collect();

            let matchas = anf_expression::MatchAs { expression, branches };

            let (variable, path) = self.new_anf_local();

            let join = anf_expression::Join {
                label,
                variable,
                join: Box::new(Expression::MatchAs(matchas)),
                expression: Box::new(k(Atom::Path(path))),
            };

            Expression::Join(join)
        }));

        result
    }

    fn match_branch(
        &self,
        branch: ast_expression::Branch,
        k: Continuation,
    ) -> anf_expression::Branch {
        let ast_expression::Branch {
            pattern,
            expression,
        } = branch;

        anf_expression::Branch {
            pattern: pattern.data,
            expression: self.expression(expression.data, k),
        }
    }

    pub fn transform(&self, expression: ASTExpression) -> Expression {
        self.expression(expression, Box::new(Expression::Atom))
    }
}
