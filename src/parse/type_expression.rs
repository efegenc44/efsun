use std::marker::PhantomData;

use crate::{
    interner::InternId,
    location::Located,
    resolution::{Renamed, Resolved, Unresolved, bound::Bound}
};

#[derive(Clone)]
pub enum TypeExpression<State> {
    Path(PathTypeExpression<State>),
    Application(ApplicationTypeExpression<State>),
}

impl Located<TypeExpression<Resolved>> {
    pub fn renamed(self) -> Located<TypeExpression<Renamed>> {
        let (data, span) = self.destruct();

        let expression = match data {
            TypeExpression::Path(path) => TypeExpression::Path(path.renamed()),
            TypeExpression::Application(application) => {
                let (function, arguments) = application.destruct();

                let arguments = arguments.into_iter().map(|argument| argument.renamed()).collect();

                TypeExpression::Application(ApplicationTypeExpression::new(
                    function.renamed(),
                    arguments,
                ))
            },
        };

        Located::new(expression, span)
    }
}

#[derive(Clone)]
pub struct PathTypeExpression<State> {
    parts: Located<Vec<InternId>>,
    bound: Option<Bound>,
    state: PhantomData<State>
}

impl<T> PathTypeExpression<T> {
    pub fn parts(&self) -> &Located<Vec<InternId>> {
        &self.parts
    }
}

impl PathTypeExpression<Unresolved> {
    pub fn new(parts: Located<Vec<InternId>>) -> Self {
        Self { parts, bound: None, state: PhantomData }
    }

    pub fn resolve(self, bound: Bound) -> PathTypeExpression<Resolved> {
        PathTypeExpression {
            parts: self.parts,
            bound: Some(bound),
            state: PhantomData
        }
    }
}

impl PathTypeExpression<Resolved> {
    pub fn bound(&self) -> &Bound {
        self.bound.as_ref().unwrap()
    }

    pub fn renamed(self) -> PathTypeExpression<Renamed> {
        PathTypeExpression {
            parts: self.parts,
            bound: self.bound,
            state: PhantomData
        }
    }
}

#[derive(Clone)]
pub struct ApplicationTypeExpression<T> {
    function: Box<Located<TypeExpression<T>>>,
    arguments: Vec<Located<TypeExpression<T>>>,
}

impl<T> ApplicationTypeExpression<T> {
    pub fn new(function: Located<TypeExpression<T>>, arguments: Vec<Located<TypeExpression<T>>>) -> Self {
        Self {
            function: Box::new(function),
            arguments
        }
    }

    pub fn destruct(self) -> (Located<TypeExpression<T>>, Vec<Located<TypeExpression<T>>>) {
        (*self.function, self.arguments)
    }

    pub fn function(&self) -> &Located<TypeExpression<T>> {
        &self.function
    }

    pub fn arguments(&self) -> &[Located<TypeExpression<T>>] {
        &self.arguments
    }
}