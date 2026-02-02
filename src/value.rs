use std::{fmt::Display, rc::Rc};

use crate::expression::{Expression, Resolved};

#[derive(Clone)]
pub enum Value {
    Lambda(LambdaValue)
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lambda(_lambda) => write!(f, "<lambda>"),
        }
    }
}

#[derive(Clone)]
pub struct LambdaValue {
    expression: Rc<Expression<Resolved>>,
    captures: Rc<Vec<Value>>
}

impl LambdaValue {
    pub fn new(expression: Expression<Resolved>, captures: Vec<Value>) -> Self {
        Self {
            expression: Rc::new(expression),
            captures: Rc::new(captures)
        }
    }

    pub fn expression(&self) -> &Expression<Resolved> {
        &self.expression
    }

    pub fn captures(&self) -> Rc<Vec<Value>> {
        self.captures.clone()
    }
}