use crate::{interner::{InternId, Interner}, location::Located};

pub enum Expression {
    Identifier(InternId),
    Lambda(LambdaExpression),
    Application(ApplicationExpression),
}

impl Expression {
    pub fn application(function: Box<Located<Expression>>, argument: Box<Located<Expression>>) -> Self {
        Self::Application(ApplicationExpression { function, argument })
    }

    pub fn lambda(variable: Located<InternId>, expression: Box<Located<Expression>>) -> Self {
        Self::Lambda(LambdaExpression { variable, expression })
    }

    pub fn print(&self, interner: &Interner, depth: usize) {
        let indent = depth*2;

        match self {
            Self::Identifier(id) => {
                println!("{:indent$}Identifier: {}", "", interner.lookup(*id))
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

pub struct ApplicationExpression {
    function: Box<Located<Expression>>,
    argument: Box<Located<Expression>>
}

pub struct LambdaExpression {
    variable: Located<InternId>,
    expression: Box<Located<Expression>>
}