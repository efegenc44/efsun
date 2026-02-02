use crate::{
    expression::{
        Expression, Resolved, IdentifierExpression, ApplicationExpression,
        LambdaExpression, LetExpression
    },
    value::{Value, LambdaValue},
    resolver::Bound
};

pub struct Evaluator {
    locals: Vec<Value>
}

impl Evaluator {
    pub fn new() -> Self {
        Self { locals: Vec::new() }
    }

    pub fn expression(&mut self, expression: &Expression<Resolved>) -> Value {
        match expression {
            Expression::Identifier(identifier) => self.identifier(identifier),
            Expression::Application(application) => self.application(application),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Let(letin) => self.letin(letin),
        }
    }

    fn identifier(&mut self, identifier: &IdentifierExpression<Resolved>) -> Value {
        match identifier.bound() {
            Bound::Local(id) => self.locals[id.value()].clone(),
            Bound::Capture(_capture) => todo!(),
        }
    }

    fn application(&mut self, application: &ApplicationExpression<Resolved>) -> Value {
        let function = self.expression(application.function().data());
        let argument = self.expression(application.argument().data());

        let n = self.locals.len();
        let return_value = match function {
            Value::Lambda(lambda) => {
                self.locals.extend(lambda.captures().iter().cloned());
                self.locals.push(argument);
                self.expression(lambda.expression())
            }
        };
        self.locals.truncate(n);

        return_value
    }

    fn lambda(&mut self, lambda: &LambdaExpression<Resolved>) -> Value {
        let lambda = lambda.expression().data().clone();
        let captures = self.locals.clone();

        Value::Lambda(LambdaValue::new(lambda, captures))
    }

    fn letin(&mut self, letin: &LetExpression<Resolved>) -> Value {
        let variable = self.expression(letin.variable_expression().data());
        self.locals.push(variable);
        let return_value = self.expression(letin.return_expression().data());
        self.locals.pop();

        return_value
    }
}