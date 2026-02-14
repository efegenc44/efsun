use std::{cell::RefCell, marker::PhantomData};

use crate::{
    parse::expression::{Expression, Resolved, Unresolved},
    interner::{InternId, Interner},
    location::{Located, SourceLocation},
    resolver::{Bound, Capture}
};

#[derive(Clone)]
pub enum ANF<State> {
    Let(LetExpression<State>),
    Application(ApplicationExpression<State>),
    Atom(Atom<State>)
}

impl<T> ANF<T> {
    pub fn print(&self, depth: usize, interner: &Interner) {
        let indent = 2*depth;

        match self {
            ANF::Let(letin) => {
                print!("{:indent$}let {} = ", "", interner.lookup(letin.variable));
                letin.variable_expression.print(0, interner);
                println!(" in");
                letin.return_expression.print(depth + 1, interner);
                println!();
            }
            ,
            ANF::Application(application) => {
                print!("{:indent$}let {} = ", "", application.variable.display(interner));
                application.function.print(0, interner);
                application.argument.print(0, interner);
                println!(" in");
                application.expression.print(depth + 1, interner);
                println!();
            },
            ANF::Atom(atom) => atom.print(depth, interner),
        }
    }
}

#[derive(Clone)]
pub struct LetExpression<T> {
    variable: InternId,
    variable_expression: Atom<T>,
    return_expression: Box<ANF<T>>
}

impl<T> LetExpression<T> {
    pub fn new(variable: InternId, variable_expression: Atom<T>, return_expression: ANF<T>) -> Self {
        Self { variable, variable_expression, return_expression: Box::new(return_expression) }
    }

    pub fn destruct(self) -> (InternId, Atom<T>, ANF<T>) {
        (self.variable, self.variable_expression, *self.return_expression)
    }
}

impl LetExpression<Resolved> {
    pub fn variable_expression(&self) -> &Atom<Resolved> {
        &self.variable_expression
    }

    pub fn return_expression(&self) -> &ANF<Resolved> {
        &self.return_expression
    }
}

#[derive(Clone)]
pub struct ApplicationExpression<T> {
    variable: Identifier,
    function: Atom<T>,
    argument: Atom<T>,
    expression: Box<ANF<T>>
}

impl<T> ApplicationExpression<T> {
    pub fn new(variable: Identifier, function: Atom<T>, argument: Atom<T>, expression: ANF<T>) -> Self {
        Self { variable, function, argument, expression: Box::new(expression) }
    }
}

impl ApplicationExpression<Unresolved> {
    pub fn destruct(self) -> (Identifier, Atom<Unresolved>, Atom<Unresolved>, ANF<Unresolved>) {
        (self.variable, self.function, self.argument, *self.expression)
    }
}

impl ApplicationExpression<Resolved> {
    pub fn function(&self) -> &Atom<Resolved> {
        &self.function
    }

    pub fn argument(&self) -> &Atom<Resolved> {
        &self.argument
    }

    pub fn expression(&self) -> &ANF<Resolved> {
        &self.expression
    }
}

#[derive(Clone)]
pub enum Atom<State> {
    String(InternId),
    Identifier(IdentifierExpression<State>),
    Lambda(LambdaExpression<State>)
}

impl<T> Atom<T> {
    fn print(&self, depth: usize, interner: &Interner) {
        let indent = 2*depth;

        match self {
            Atom::String(id) => print!("{:indent$}\"{}\"", "", interner.lookup(*id)),
            Atom::Identifier(identifier) => {
                print!(
                    "{:indent$}{}{}", "",
                    identifier.identifier.display(interner),
                    if let Some(bound) = identifier.bound { format!("#{}", bound) } else { "".to_string() }
                )
            },
            Atom::Lambda(lambda) => {
                print!("{:indent$}\\{} ", "", interner.lookup(lambda.variable));
                if let Some(captures) = &lambda.captures {
                    if !captures.is_empty() {
                        print!("{:?} ", captures);
                    }
                }
                lambda.expression.print(0, interner);
            },
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Identifier {
    ANF(usize),
    Normal(InternId),
}

impl Identifier {
    fn display(&self, interner: &Interner) -> String {
        match self {
            Identifier::ANF(id) => format!("{{ANF{id}}}"),
            Identifier::Normal(id) => interner.lookup(*id).to_string(),
        }
    }
}

#[derive(Clone)]
pub struct IdentifierExpression<T> {
    identifier: Identifier,
    bound: Option<Bound>,
    state: PhantomData<T>,
}

impl IdentifierExpression<Unresolved> {
    fn new(identifier: Identifier) -> Self {
        Self { identifier, bound: None, state: PhantomData }
    }

    pub fn resolve(self, bound: Bound) -> IdentifierExpression<Resolved> {
        IdentifierExpression {
            identifier: self.identifier,
            bound: Some(bound),
            state: PhantomData
        }
    }

    pub fn identifier(&self) -> Identifier {
        self.identifier
    }
}

impl IdentifierExpression<Resolved> {
    pub fn bound(&self) -> Bound {
        self.bound.unwrap()
    }
}

#[derive(Clone)]
pub struct LambdaExpression<T> {
    variable: InternId,
    expression: Box<ANF<T>>,
    captures: Option<Vec<Capture>>
}

impl LambdaExpression<Unresolved> {
    fn new(variable: InternId, expression: ANF<Unresolved>) -> Self {
        Self { variable, expression: Box::new(expression), captures: None }
    }

    pub fn destruct(self) -> (InternId, ANF<Unresolved>) {
        (self.variable, *self.expression)
    }
}

impl LambdaExpression<Resolved> {
    pub fn new(variable: InternId, expression: ANF<Resolved>, captures: Vec<Capture>) -> Self {
        Self { variable, expression: Box::new(expression), captures: Some(captures) }
    }

    pub fn expression(&self) -> &ANF<Resolved> {
        &self.expression
    }

    pub fn captures(&self) -> &[Capture] {
        self.captures.as_ref().unwrap()
    }
}

pub struct ANFConverter {
    counter: RefCell<usize>,
}

impl ANFConverter {
    pub fn new() -> Self {
        Self { counter: RefCell::new(0) }
    }

    fn anf(&self, e: Expression<Unresolved>, k: Box<dyn FnOnce(Atom<Unresolved>) -> ANF<Unresolved> + '_>) -> ANF<Unresolved> {
        match e {
            Expression::String(id) => k(Atom::String(id)),
            Expression::Identifier(identifier) => {
                let id = *identifier.identifier().data();
                k(Atom::Identifier(IdentifierExpression::new(Identifier::Normal(id))))
            },
            Expression::Application(application) => {
                let (function, argument) = application.destruct();

                self.anf(argument.destruct().0, Box::new(|argument| {
                    self.anf(function.data().clone(), Box::new(|function| {
                        let variable = Identifier::ANF(*self.counter.borrow());
                        *self.counter.borrow_mut() += 1;
                        ANF::Application(ApplicationExpression::new(
                            variable,
                            function,
                            argument.clone(),
                            k(Atom::Identifier(IdentifierExpression::new(variable))))
                        )
                    }))
                }))
            },
            Expression::Lambda(lambda) => {
                let (variable, expression) = lambda.destruct();

                let expression = self.convert(expression.destruct().0);
                k(Atom::Lambda(LambdaExpression::<Unresolved>::new(*variable.data(), expression)))
            },
            Expression::Let(letin) => {
                let (variable, variable_expression, return_expression) = letin.destruct();

                self.anf(variable_expression.destruct().0, Box::new(|vexpr| {
                    ANF::Let(LetExpression::new(*variable.data(), vexpr, self.anf(return_expression.data().clone(), k)))
                }))
            },
        }
    }

    pub fn convert(&self, e: Expression<Unresolved>) -> ANF<Unresolved> {
        self.anf(e, Box::new(ANF::Atom))
    }
}

