use std::marker::PhantomData;

use crate::{
    interner::{InternId, Interner},
    location::Located,
    resolver::{Bound, Capture}
};

#[derive(Clone, Copy)]
pub struct Resolved;
#[derive(Clone, Copy)]
pub struct Unresolved;

#[derive(Clone)]
pub enum Expression<State> {
    String(InternId),
    Identifier(IdentifierExpression<State>),
    Application(ApplicationExpression<State>),
    Lambda(LambdaExpression<State>),
    Let(LetExpression<State>),
}

impl<T> Expression<T> {
    #[allow(unused)]
    pub fn print(&self, interner: &Interner, depth: usize) {
        let indent = depth*2;

        match self {
            Self::String(string) => {
                println!("{:indent$}{}", "", interner.lookup(*string));
            },
            Self::Identifier(identifier) => {
                println!(
                    "{:indent$}Identifier: {}{}", "",
                    interner.lookup(*identifier.identifier().data()),
                    if let Some(bound) = identifier.bound { format!("#{}", bound) } else { "".to_string() }
                )
            },
            Self::Lambda(lambda) => {
                println!("{:indent$}Lambda:", "");
                if let Some(captures) = &lambda.captures {
                    if !captures.is_empty() {
                        println!("{:indent$}Captures: {:?}", "", captures, indent=indent + 2);
                    }
                }
                println!("{:indent$}{}", "", interner.lookup(*lambda.variable.data()), indent=indent + 2);
                lambda.expression.data().print(interner, depth + 1);
            }
            Self::Application(application) => {
                println!("{:indent$}Application:", "");
                application.function.data().print(interner, depth + 1);
                application.argument.data().print(interner, depth + 1);
            },
            Self::Let(letin) => {
                println!("{:indent$}Let:", "");
                if let Some(captures) = &letin.captures {
                    if !captures.is_empty() {
                        println!("{:indent$}Captures: {:?}", "", captures, indent=indent + 2);
                    }
                }
                println!("{:indent$}{}", "", interner.lookup(*letin.variable.data()), indent=indent + 2);
                letin.variable_expression.data().print(interner, depth + 2);
                letin.return_expression.data().print(interner, depth + 1);
            }
        }
    }
}

#[derive(Clone)]
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
        Self { identifier, bound: Option::None, state: PhantomData }
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

#[derive(Clone)]
pub struct ApplicationExpression<T> {
    function: Box<Located<Expression<T>>>,
    argument: Box<Located<Expression<T>>>,
}

impl<T> ApplicationExpression<T> {
    pub fn new(function: Located<Expression<T>>, argument: Located<Expression<T>>) -> Self {
        Self { function: Box::new(function), argument: Box::new(argument) }
    }

    pub fn destruct(self) -> (Located<Expression<T>>, Located<Expression<T>>) {
        (*self.function, *self.argument)
    }

    pub fn function(&self) -> &Located<Expression<T>> {
        &self.function
    }

    pub fn argument(&self) -> &Located<Expression<T>> {
        &self.argument
    }
}

#[derive(Clone)]
pub struct LambdaExpression<T> {
    variable: Located<InternId>,
    expression: Box<Located<Expression<T>>>,
    captures: Option<Vec<Capture>>
}

impl<T> LambdaExpression<T> {
    #[allow(unused)]
    pub fn variable(&self) -> Located<InternId> {
        self.variable
    }

    pub fn expression(&self) -> &Located<Expression<T>> {
        &self.expression
    }
}

impl LambdaExpression<Unresolved> {
    pub fn new(variable: Located<InternId>, expression: Located<Expression<Unresolved>>) -> Self {
        Self { variable, expression: Box::new(expression), captures: None }
    }

    pub fn destruct(self) -> (Located<InternId>, Located<Expression<Unresolved>>) {
        (self.variable, *self.expression)
    }
}

impl LambdaExpression<Resolved> {
    pub fn new(variable: Located<InternId>, expression: Located<Expression<Resolved>>, captures: Vec<Capture>) -> Self {
        Self { variable, expression: Box::new(expression), captures: Some(captures) }
    }

    pub fn captures(&self) -> &[Capture] {
        self.captures.as_ref().unwrap()
    }
}



#[derive(Clone)]
pub struct LetExpression<T> {
    variable: Located<InternId>,
    variable_expression: Box<Located<Expression<T>>>,
    return_expression: Box<Located<Expression<T>>>,
    captures: Option<Vec<Capture>>
}

impl<T> LetExpression<T> {
    #[allow(unused)]
    pub fn variable(&self) -> Located<InternId> {
        self.variable
    }

    pub fn variable_expression(&self) -> &Located<Expression<T>> {
        &self.variable_expression
    }

    pub fn return_expression(&self) -> &Located<Expression<T>> {
        &self.return_expression
    }
}

impl LetExpression<Unresolved> {
    pub fn new(
        variable: Located<InternId>,
        variable_expression: Located<Expression<Unresolved>>,
        return_expression: Located<Expression<Unresolved>>
    ) -> Self {
        Self {
            variable,
            variable_expression: Box::new(variable_expression),
            return_expression: Box::new(return_expression),
            captures: None
        }
    }

    pub fn destruct(self) -> (Located<InternId>, Located<Expression<Unresolved>>, Located<Expression<Unresolved>>) {
        (self.variable, *self.variable_expression, *self.return_expression)
    }
}

impl LetExpression<Resolved> {
    pub fn new(
        variable: Located<InternId>,
        variable_expression: Located<Expression<Resolved>>,
        return_expression: Located<Expression<Resolved>>,
        captures: Vec<Capture>
    ) -> Self {
        Self {
            variable,
            variable_expression: Box::new(variable_expression),
            return_expression: Box::new(return_expression),
            captures: Some(captures)
        }
    }

    pub fn captures(&self) -> &[Capture] {
        self.captures.as_ref().unwrap()
    }
}