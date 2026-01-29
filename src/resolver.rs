use std::fmt::Display;

use crate::{
    expression::{
        ApplicationExpression, Expression, IdentifierExpression,
        LambdaExpression, Unresolved, Resolved
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

    pub fn expression(&mut self, expression: Located<Expression<Unresolved>>) -> ResolutionResult<Located<Expression<Resolved>>> {
        let (expression, location) = expression.destruct();

        let expression = match expression {
            Expression::Identifier(identifier) => Expression::Identifier(self.identifier(identifier)?),
            Expression::Lambda(lambda) => Expression::Lambda(self.lambda(lambda)?),
            Expression::Application(application) => Expression::Application(self.application(application)?),
        };

        Ok(Located::new(expression, location))
    }

    fn identifier(&mut self, identifier: IdentifierExpression<Unresolved>) -> ResolutionResult<IdentifierExpression<Resolved>> {
        for (index, intern_id) in self.locals.iter().rev().enumerate() {
            if intern_id == identifier.identifier().data() {
                return Ok(identifier.resolve(Bound::Local(BoundId(index))))
            }
        }

        let error = Located::new(
            ResolutionError::UnboundIdentifier(*identifier.identifier().data()),
            identifier.identifier().location()
        );

        Err(error)
    }

    fn lambda(&mut self, lambda: LambdaExpression<Unresolved>) -> ResolutionResult<LambdaExpression<Resolved>> {
        let (variable, expression) = lambda.desturct();

        self.locals.push(*variable.data());
        let expression = self.expression(*expression)?;
        self.locals.pop();

        Ok(LambdaExpression::new(variable, expression))
    }

    fn application(&mut self, application: ApplicationExpression<Unresolved>) -> ResolutionResult<ApplicationExpression<Resolved>> {
        let (function, argument) = application.desturct();

        let function = self.expression(*function)?;
        let argument = self.expression(*argument)?;

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

#[allow(unused)]
#[derive(Debug)]
pub enum ResolutionError {
    UnboundIdentifier(InternId)
}

type ResolutionResult<T> = Result<T, Located<ResolutionError>>;