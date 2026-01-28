use std::fmt::Display;

use crate::{
    expression::{
        ApplicationExpression, Expression, IdentifierExpression,
        LambdaExpression
    },
    interner::InternId,
    location::Located
};

pub struct Resolver {
    locals: Vec<InternId>
}

impl Resolver {
    pub fn new() -> Self {
        Resolver { locals: Vec::default() }
    }

    pub fn expression(&mut self, expression: &mut Located<Expression>) -> ResolutionResult {
        match expression.data_mut() {
            Expression::Identifier(identifier) => self.identifier(identifier),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Application(application) => self.application(application),
        }
    }

    fn identifier(&mut self, identifier: &mut IdentifierExpression) -> ResolutionResult {
        for (index, intern_id) in self.locals.iter().rev().enumerate() {
            if intern_id == identifier.identifier().data() {
                identifier.set_bound(Bound::Local(BoundId(index)));
                return Ok(())
            }
        }

        let error = Located::new(
            ResolutionError::UnboundIdentifier(*identifier.identifier().data()),
            identifier.identifier().location()
        );

        Err(error)
    }

    fn lambda(&mut self, lambda: &mut LambdaExpression) -> ResolutionResult {
        self.locals.push(*lambda.variable().data());
        self.expression(lambda.expression_mut())?;
        self.locals.pop();
        Ok(())
    }

    fn application(&mut self, application: &mut ApplicationExpression) -> ResolutionResult {
        self.expression(application.function_mut())?;
        self.expression(application.argument_mut())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Bound {
    Local(BoundId),
}

#[derive(Clone, Copy, Debug)]
pub struct BoundId(usize);

impl BoundId {
    pub fn value(&self) -> usize {
        self.0
    }
}

impl Display for Bound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(id) => write!(f, "{}", id.0),
        }
    }
}

#[allow(unused)]
#[derive(Debug)]
pub enum ResolutionError {
    UnboundIdentifier(InternId)
}

type ResolutionResult = Result<(), Located<ResolutionError>>;