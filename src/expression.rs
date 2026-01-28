use std::cell::OnceCell;

use crate::{interner::{InternId, Interner}, location::Located, resolver::Bound};

pub enum Expression {
    Application(ApplicationExpression),
    Lambda(LambdaExpression),
    Identifier(IdentifierExpression),
}

impl Expression {
    pub fn identifier(identifier: Located<InternId>) -> Self {
        Self::Identifier(IdentifierExpression { identifier, bound: OnceCell::new() })
    }

    pub fn application(function: Box<Located<Self>>, argument: Box<Located<Self>>) -> Self {
        Self::Application(ApplicationExpression { function, argument })
    }

    pub fn lambda(variable: Located<InternId>, expression: Box<Located<Self>>) -> Self {
        Self::Lambda(LambdaExpression { variable, expression })
    }

    pub fn print(&self, interner: &Interner, depth: usize) {
        let indent = depth*2;

        match self {
            Self::Identifier(identifier) => {
                println!(
                    "{:indent$}Identifier: {}#{}", "",
                    interner.lookup(*identifier.identifier.data()),
                    identifier.bound()
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

pub struct IdentifierExpression {
    identifier: Located<InternId>,
    bound: OnceCell<Bound>,
}

impl IdentifierExpression {
    pub fn identifier(&self) -> Located<InternId> {
        self.identifier
    }

    pub fn set_bound(&mut self, bound: Bound) {
        self.bound.set(bound).unwrap();
    }

    pub fn bound(&self) -> Bound {
        *self.bound.get().unwrap()
    }
}

pub struct ApplicationExpression {
    function: Box<Located<Expression>>,
    argument: Box<Located<Expression>>,
}

impl ApplicationExpression {
    pub fn function_mut(&mut self) -> &mut Located<Expression> {
        &mut self.function
    }

    pub fn argument_mut(&mut self) -> &mut Located<Expression> {
        &mut self.argument
    }
}

pub struct LambdaExpression {
    variable: Located<InternId>,
    expression: Box<Located<Expression>>,
}

impl LambdaExpression {
    pub fn variable(&self) -> Located<InternId> {
        self.variable
    }

    pub fn expression_mut(&mut self) -> &mut Located<Expression> {
        &mut self.expression
    }
}