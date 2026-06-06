pub mod atom;
pub mod definition;
pub mod expression;

use std::{cell::RefCell, fmt::Display};

use crate::{
    interner::{InternId, WithInterner},
    metadata::Metadata,
    parse::expression::Expression as ASTExpression,
    resolution::renamer::UniqueName,
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

pub type Continuation<'a> = Box<dyn FnOnce(Atom) -> Expression + 'a>;

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

    pub fn metadata(&self) -> &'metadata Metadata {
        self.metadata
    }

    pub fn new_local_id(&self) -> usize {
        let id = *self.counter.borrow();
        *self.counter.borrow_mut() += 1;
        id
    }

    pub fn new_anf_bound_id(&self) -> usize {
        let id = *self.anf_bound_counter.borrow();
        *self.anf_bound_counter.borrow_mut() += 1;
        id
    }

    pub fn new_anf_capture_id(&self) -> usize {
        let id = *self.anf_capture_counter.borrow();
        *self.anf_capture_counter.borrow_mut() += 1;
        id
    }

    pub fn new_anf_local(&self) -> (Local, atom::Path) {
        let id = self.new_local_id();

        let local = Local::ANFLocal(id);
        let path = atom::Path::new(Path::ANFLocal(id), None, self.new_anf_bound_id());

        (local, path)
    }

    pub fn transform(&self, expression: ASTExpression) -> Expression {
        expression.into_anf(self, Box::new(Expression::Atom))
    }
}
