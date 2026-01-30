use std::fmt::Display;

use crate::{
    expression::{
        ApplicationExpression, Expression, IdentifierExpression,
        LambdaExpression, Unresolved, Resolved
    },
    interner::{InternId},
    location::{Located, SourceLocation},
    error::{Error, Result}
};

pub struct Resolver {
    locals: Vec<InternId>
}

impl Resolver {
    pub fn new() -> Self {
        Resolver { locals: Vec::default() }
    }

    pub fn expression(&mut self, expression: Located<Expression<Unresolved>>) -> Result<Located<Expression<Resolved>>> {
        let (expression, start, end) = expression.destruct();

        let expression = match expression {
            Expression::Identifier(identifier) => Expression::Identifier(self.identifier(identifier, start, end)?),
            Expression::Lambda(lambda) => Expression::Lambda(self.lambda(lambda)?),
            Expression::Application(application) => Expression::Application(self.application(application)?),
        };

        Ok(Located::new(expression, start, end))
    }

    fn identifier(
        &mut self,
        identifier: IdentifierExpression<Unresolved>,
        start: SourceLocation,
        end: SourceLocation
    ) -> Result<IdentifierExpression<Resolved>> {
        for (index, intern_id) in self.locals.iter().rev().enumerate() {
            if intern_id == identifier.identifier().data() {
                return Ok(identifier.resolve(Bound::Local(BoundId(index))))
            }
        }

        let error: Error = ResolutionError::UnboundIdentifier(*identifier.identifier().data()).into();
        let error = Located::new(error, start, end);

        Err(error)
    }

    fn lambda(&mut self, lambda: LambdaExpression<Unresolved>) -> Result<LambdaExpression<Resolved>> {
        let (variable, expression) = lambda.desturct();

        self.locals.push(*variable.data());
        let expression = self.expression(*expression)?;
        self.locals.pop();

        Ok(LambdaExpression::new(variable, expression))
    }

    fn application(&mut self, application: ApplicationExpression<Unresolved>) -> Result<ApplicationExpression<Resolved>> {
        let (function, argument) = application.desturct();

        let function = self.expression(function)?;
        let argument = self.expression(argument)?;

        Ok(ApplicationExpression::new(function, argument))
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

#[derive(Debug, Clone, Copy)]
pub enum ResolutionError {
    UnboundIdentifier(InternId)
}
