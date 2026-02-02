use std::rc::Rc;

use crate::{
    expression::{
        Expression, Resolved, IdentifierExpression, ApplicationExpression,
        LambdaExpression, LetExpression
    },
    value::{Value, LambdaValue},
    resolver::{Bound, Capture}
};

pub struct Evaluator {
    locals: Vec<Value>,
    closures: Vec<Rc<Vec<Value>>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            closures: Vec::new(),
        }
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
            Bound::Capture(id) => self.closures.last().unwrap()[id.value()].clone(),
        }
    }

    fn application(&mut self, application: &ApplicationExpression<Resolved>) -> Value {
        let function = self.expression(application.function().data());
        let argument = self.expression(application.argument().data());

        match function {
            Value::Lambda(lambda) => {
                self.closures.push(lambda.captures());
                self.locals.push(argument);
                let return_value = self.expression(lambda.expression());
                self.locals.pop();
                self.closures.pop();

                return_value
            }
        }
    }

    fn lambda(&mut self, lambda: &LambdaExpression<Resolved>) -> Value {
        let expression = lambda.expression().data().clone();
        let captures = self.capture(lambda.captures());

        Value::Lambda(LambdaValue::new(expression, captures))
    }

    fn capture(&self, captures: &[Capture]) -> Vec<Value> {
        let mut values = vec![];
        for capture in captures {
            match capture {
                Capture::Local(id) => {
                    let index = self.locals.len() - 1 - id.value();
                    values.push(self.locals[index].clone());
                },
                Capture::Outer(id) => {
                    let value = self.closures.last().unwrap()[id.value()].clone();
                    values.push(value);
                },
            }
        }

        values
    }

    fn letin(&mut self, letin: &LetExpression<Resolved>) -> Value {
        let variable = self.expression(letin.variable_expression().data());
        self.locals.push(variable);
        let return_value = self.expression(letin.return_expression().data());
        self.locals.pop();

        return_value
    }
}