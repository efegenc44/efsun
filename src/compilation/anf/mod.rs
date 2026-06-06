pub mod atom;
pub mod definition;
pub mod expression;

use std::{
    cell::{Cell, RefCell, RefMut},
    fmt::Display,
};

use crate::{
    interner::{InternId, WithInterner},
    metadata::{Generator, Indicies, Metadata},
    parse::expression::Expression as ASTExpression,
    resolution::renamer::{Renamed, UniqueName},
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

    pub fn metadata(&self) -> &Metadata<Renamed> {
        self.metadata
    }

    pub fn indicies_mut(&self) -> RefMut<'_, Indicies> {
        self.indicies.borrow_mut()
    }

    pub fn new_local_id(&self) -> usize {
        let id = self.counter.get();
        self.counter.update(|x| x + 1);
        id
    }

    pub fn new_anf_local(&self) -> (Local, atom::Path) {
        let id = self.new_local_id();

        let local = Local::ANFLocal(id);
        let path = atom::Path::new(Path::ANFLocal(id), None, self.indicies.borrow_mut().get());

        (local, path)
    }

    pub fn transform(&self, expression: ASTExpression) -> Expression {
        expression.into_anf(self, Box::new(Expression::Atom))
    }
}
