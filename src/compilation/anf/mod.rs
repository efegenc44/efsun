pub mod atom;
pub mod definition;
pub mod expression;

use std::{cell::RefCell, fmt::Display};

use definition::{self as anf_definition};
use expression::{self as anf_expression};

use crate::{
    interner::{InternId, WithInterner},
    metadata::Metadata,
    parse::{
        definition::{
            self as ast_definition, Definition as ASTDefinition, Module as ASTModule,
            Program as ASTProgram,
        },
        expression::{self as ast_expression, Expression as ASTExpression},
    },
    resolution::{bound::Bound, renamer::UniqueName},
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
    Absolute(Vec<InternId>),
    Local(UniqueName),
}

impl<'interner> Display for WithInterner<'interner, &Path> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            Path::ANFLocal(id) => write!(f, "<ANF{id}>"),
            Path::Local(unique_name) => {
                write!(f, "{unique_name}")
            }
            Path::Absolute(parts) => match parts.as_slice() {
                [] => unreachable!(),
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

type Continuation<'a> = Box<dyn FnOnce(Atom) -> Expression + 'a>;

/// AST to ANF Transformer
pub struct Transformer<'metadata> {
    /// State for ANF-introduced local variables and jump labels
    counter: RefCell<usize>,
    /// Metadata
    metadata: &'metadata Metadata,

    anf_bound_counter: RefCell<usize>,
    anf_capture_counter: RefCell<usize>,
}

impl<'metadata> Transformer<'metadata> {
    pub fn new(metadata: &'metadata Metadata) -> Self {
        Self {
            counter: RefCell::new(0),
            metadata,
            anf_bound_counter: RefCell::new(0),
            anf_capture_counter: RefCell::new(0),
        }
    }

    fn new_local_id(&self) -> usize {
        let id = *self.counter.borrow();
        *self.counter.borrow_mut() += 1;
        id
    }

    fn new_anf_bound_id(&self) -> usize {
        let id = *self.anf_bound_counter.borrow();
        *self.anf_bound_counter.borrow_mut() += 1;
        id
    }

    fn new_anf_capture_id(&self) -> usize {
        let id = *self.anf_capture_counter.borrow();
        *self.anf_capture_counter.borrow_mut() += 1;
        id
    }

    fn new_anf_local(&self) -> (Local, atom::Path) {
        let id = self.new_local_id();

        let local = Local::ANFLocal(id);
        let path = atom::Path::new(Path::ANFLocal(id), None, self.new_anf_bound_id());

        (local, path)
    }

    pub fn program(&self, program: ASTProgram) -> Program {
        let ast_definition::Program { modules } = program;

        let modules = modules
            .into_iter()
            .map(|module| self.module(module))
            .collect::<Vec<_>>();

        anf_definition::Program::new(modules)
    }

    pub fn module(&self, module: ASTModule) -> Module {
        let ast_definition::Module { definitions, .. } = module;

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

        anf_definition::Module::new(anf_module)
    }

    fn name_definition(&self, name: ast_definition::Name) -> Definition {
        let ast_definition::Name {
            identifier,
            expression,
            path_id,
        } = name;

        let name = anf_definition::Name::new(
            identifier.into_data(),
            self.transform(expression.into_data()),
            path_id,
        );

        Definition::Name(name)
    }

    fn structure_definition(&self, structure: ast_definition::Structure) -> Definition {
        let ast_definition::Structure { constructors, .. } = structure;

        let constructors = constructors
            .into_iter()
            .map(|constructor| {
                let ast_definition::structure::Constructor {
                    name,
                    arguments,
                    path_id,
                    ..
                } = constructor.into_data();

                anf_definition::structure::Constructor::new(
                    name.into_data(),
                    arguments.len(),
                    path_id,
                )
            })
            .collect();

        Definition::Structure(anf_definition::Structure::new(constructors))
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

        let bound = self.metadata.get_bound(bound_id);
        let bound = match bound {
            Bound::Absolute(_) => Some(bound.clone()),
            Bound::Local(_) | Bound::Capture(_) => None,
        };

        let unique_name = self.metadata.get_unique_name(unique_name_id);
        let path = match unique_name {
            Some(unique_name) => Path::Local(*unique_name),
            None => Path::Absolute(parts.into_data()),
        };

        let path = atom::Path::new(path, bound, self.new_anf_bound_id());
        k(Atom::Path(path))
    }

    fn application(&self, application: ast_expression::Application, k: Continuation) -> Expression {
        let ast_expression::Application { function, argument } = application;

        #[rustfmt::skip]
        let result = self.expression(argument.into_data(), Box::new(|argument| {
            self.expression(function.into_data(), Box::new(|function| {
                let (variable, path) = self.new_anf_local();

                let application = anf_expression::Application::new(
                    variable,
                    function,
                    argument,
                    k(Atom::Path(path))
                );

                Expression::Application(application)
            }))
        }));

        result
    }

    fn lambda(&self, lambda: ast_expression::Lambda, k: Continuation) -> Expression {
        let ast_expression::Lambda {
            expression,
            unique_name_id,
            ..
        } = lambda;

        let variable = self.metadata.get_unique_name(unique_name_id).unwrap();
        let lambda = atom::Lambda::new(
            variable,
            self.transform(expression.into_data()),
            self.new_anf_capture_id(),
        );

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
        let result = self.expression(variable_expression.into_data(), Box::new(|variable_expression| {
            let letin = anf_expression::LetIn::new(
                self.metadata.get_unique_name(unique_name_id).unwrap(),
                variable_expression,
                self.expression(return_expression.into_data(), k),
            );

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
        let result = self.expression(expression.into_data(), Box::new(|expression| {
            let label = self.new_local_id();

            let branch_k = |expression| {
                let jump = anf_expression::Jump::new(label, expression);
                Expression::Jump(jump)
            };

            let branches = branches
                .into_iter()
                .map(|branch| self.match_branch(branch.into_data(), Box::new(branch_k)))
                .collect();

            let matchas = anf_expression::MatchAs::new(expression, branches);

            let (variable, path) = self.new_anf_local();

            let join = anf_expression::Join::new(
                label,
                variable,
                Expression::Match(matchas),
                k(Atom::Path(path)),
            );

            Expression::Join(join)
        }));

        result
    }

    fn match_branch(
        &self,
        branch: ast_expression::matchas::Branch,
        k: Continuation,
    ) -> anf_expression::matchas::Branch {
        let ast_expression::matchas::Branch {
            pattern,
            expression,
        } = branch;

        anf_expression::matchas::Branch::new(
            pattern.into_data(),
            self.expression(expression.into_data(), k),
        )
    }

    pub fn transform(&self, expression: ASTExpression) -> Expression {
        self.expression(expression, Box::new(Expression::Atom))
    }
}
