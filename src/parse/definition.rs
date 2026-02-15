use std::marker::PhantomData;

use crate::{
    interner::InternId,
    location::Located,
    parse::expression::Expression
};

pub enum Definition<State> {
    Module(ModuleDefinition<State>),
    Name(NameDefinition<State>)
}

pub struct ModuleDefinition<State> {
    parts: Located<Vec<InternId>>,
    state: PhantomData<State>
}

impl<State> ModuleDefinition<State> {
    pub fn new(parts: Located<Vec<InternId>>) -> Self {
        Self { parts, state: PhantomData }
    }
}

pub struct NameDefinition<T> {
    identifier: Located<InternId>,
    expression: Located<Expression<T>>,
}

impl<T> NameDefinition<T> {
    pub fn new(identifier: Located<InternId>, expression: Located<Expression<T>>) -> Self {
        Self { identifier, expression }
    }
}

