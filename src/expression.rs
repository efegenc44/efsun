use std::marker::PhantomData;

use crate::{
    interner::{InternId, Interner},
    location::Located,
    resolver::Bound
};

pub struct Resolved;
pub struct Unresolved;

pub enum Expression<State> {
    Identifier(IdentifierExpression<State>),
    Application(ApplicationExpression<State>),
    Lambda(LambdaExpression<State>),
}

impl<T> Expression<T> {
    #[allow(unused)]
    pub fn print(&self, interner: &Interner, depth: usize) {
        let indent = depth*2;

        match self {
            Self::Identifier(identifier) => {
                println!(
                    "{:indent$}Identifier: {}{}", "",
                    interner.lookup(*identifier.identifier().data()),
                    if let Some(bound) = identifier.bound { format!("#{}", bound) } else { "".to_string() }
                )
            },
            Self::Lambda(lambda) => {
                println!("{:indent$}Lambda:", "");
                println!("{:indent$}{}", "", interner.lookup(*lambda.variable.data()), indent=indent + 2);
                lambda.expression.data().print(interner, depth + 1);
            }
            Self::Application(application) => {
                println!("{:indent$}Application:", "");
                application.function.data().print(interner, depth + 1);
                application.argument.data().print(interner, depth + 1);
            },
        }
    }
}

pub struct IdentifierExpression<State> {
    identifier: Located<InternId>,
    bound: Option<Bound>,
    state: PhantomData<State>
}

impl<T> IdentifierExpression<T> {
    pub fn identifier(&self) -> Located<InternId> {
        self.identifier
    }
}

impl IdentifierExpression<Unresolved> {
    pub fn new(identifier: Located<InternId>) -> Self {
        Self { identifier: identifier, bound: Option::None, state: PhantomData }
    }

    pub fn resolve(self, bound: Bound) -> IdentifierExpression<Resolved> {
        IdentifierExpression {
            identifier: self.identifier,
            bound: Some(bound),
            state: PhantomData
        }
    }
}

impl IdentifierExpression<Resolved> {
    pub fn bound(&self) -> Bound {
        self.bound.unwrap()
    }
}

pub struct ApplicationExpression<T> {
    function: Box<Located<Expression<T>>>,
    argument: Box<Located<Expression<T>>>,
}

impl<T> ApplicationExpression<T> {
    pub fn new(function: Located<Expression<T>>, argument: Located<Expression<T>>) -> Self {
        Self { function: Box::new(function), argument: Box::new(argument) }
    }

    pub fn desturct(self) -> (Box<Located<Expression<T>>>, Box<Located<Expression<T>>>) {
        (self.function, self.argument)
    }

    pub fn function(&self) -> &Located<Expression<T>> {
        &self.function
    }

    pub fn argument(&self) -> &Located<Expression<T>> {
        &self.argument
    }
}

pub struct LambdaExpression<T> {
    variable: Located<InternId>,
    expression: Box<Located<Expression<T>>>,
}

impl<T> LambdaExpression<T> {
    pub fn new(variable: Located<InternId>, expression: Located<Expression<T>>) -> Self {
        Self { variable, expression: Box::new(expression) }
    }

    pub fn desturct(self) -> (Located<InternId>, Box<Located<Expression<T>>>) {
        (self.variable, self.expression)
    }

    #[allow(unused)]
    pub fn variable(&self) -> Located<InternId> {
        self.variable
    }

    pub fn expression(&self) -> &Located<Expression<T>> {
        &self.expression
    }
}