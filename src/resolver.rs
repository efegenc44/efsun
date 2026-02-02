use std::fmt::{Debug, Display};

use crate::{
    expression::{
        ApplicationExpression, Expression, IdentifierExpression,
        LambdaExpression, Unresolved, Resolved, LetExpression
    },
    interner::{InternId},
    location::{Located, SourceLocation},
    error::{Result, located_error}
};

pub struct Resolver {
    scopes: Vec<Scope>
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: vec![Scope::new()]
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope::new())
    }

    fn exit_scope(&mut self) -> Vec<Capture> {
        self.scopes.pop().unwrap().captures
    }

    fn push_local(&mut self, local: InternId) {
        self.scopes.last_mut().unwrap().locals.push(local);
    }

    fn pop_local(&mut self) {
        self.scopes.last_mut().unwrap().locals.pop();
    }

    pub fn expression(&mut self, expression: Located<Expression<Unresolved>>) -> Result<Located<Expression<Resolved>>> {
        let (expression, start, end) = expression.destruct();

        let expression = match expression {
            Expression::Identifier(identifier) => Expression::Identifier(self.identifier(identifier, start, end)?),
            Expression::Lambda(lambda) => Expression::Lambda(self.lambda(lambda)?),
            Expression::Application(application) => Expression::Application(self.application(application)?),
            Expression::Let(letin) => Expression::Let(self.letin(letin)?),
        };

        Ok(Located::new(expression, start, end))
    }

    fn identifier(
        &mut self,
        identifier: IdentifierExpression<Unresolved>,
        start: SourceLocation,
        end: SourceLocation
    ) -> Result<IdentifierExpression<Resolved>> {
        // Local
        let result = self.scopes[self.scopes.len() - 1].resolve(*identifier.identifier().data());
        if let Some(bound) = result {
            return Ok(identifier.resolve(Bound::Local(bound)))
        }

        if self.scopes.len() - 1 == 0 {
            let error = ResolutionError::UnboundIdentifier(*identifier.identifier().data());
            return Err(located_error(error, start, end));
        }

        // Capture
        let result = self.capture(*identifier.identifier().data(), self.scopes.len() - 2);
        if let Some(bound) = result {
            let scope_index = self.scopes.len() - 1;

            if let Some(position) = self.scopes[scope_index].captures.iter().position(|c| *c == bound) {
                return Ok(identifier.resolve(Bound::Capture(BoundId(position))));
            } else {
                self.scopes[scope_index].captures.push(bound);
                let id = self.scopes[scope_index].captures.len() - 1;

                return Ok(identifier.resolve(Bound::Capture(BoundId(id))));
            }
        }

        let error = ResolutionError::UnboundIdentifier(*identifier.identifier().data());
        Err(located_error(error, start, end))
    }

    fn capture(&mut self, identifier: InternId, scope_index: usize) -> Option<Capture> {
        let scope = &self.scopes[scope_index];

        if let Some(bound) = scope.resolve(identifier) {
            Some(Capture::Local(bound))
        } else {
            if scope_index == 0 {
                None
            } else {
                let capture = self.capture(identifier, scope_index - 1)?;

                if let Some(position) = self.scopes[scope_index].captures.iter().position(|c| *c == capture) {
                    Some(Capture::Outer(BoundId(position)))
                } else {
                    self.scopes[scope_index].captures.push(capture);
                    let id = self.scopes[scope_index].captures.len() - 1;
                    Some(Capture::Outer(BoundId(id)))
                }
            }
        }
    }

    fn lambda(&mut self, lambda: LambdaExpression<Unresolved>) -> Result<LambdaExpression<Resolved>> {
        let (variable, expression) = lambda.destruct();

        self.enter_scope();
        self.push_local(*variable.data());
        let expression = self.expression(expression)?;
        self.pop_local();
        let captures = self.exit_scope();

        Ok(LambdaExpression::<Resolved>::new(variable, expression, captures))
    }

    fn application(&mut self, application: ApplicationExpression<Unresolved>) -> Result<ApplicationExpression<Resolved>> {
        let (function, argument) = application.destruct();

        let function = self.expression(function)?;
        let argument = self.expression(argument)?;

        Ok(ApplicationExpression::new(function, argument))
    }

    fn letin(&mut self, letin: LetExpression<Unresolved>) -> Result<LetExpression<Resolved>> {
        let (variable, variable_expression, return_expression) = letin.destruct();

        let variable_expression = self.expression(variable_expression)?;
        self.push_local(*variable.data());
        let return_expression = self.expression(return_expression)?;
        self.pop_local();

        Ok(LetExpression::new(variable, variable_expression, return_expression))
    }
}

struct Scope {
    locals: Vec<InternId>,
    captures: Vec<Capture>,
}

impl Scope {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            captures: Vec::new(),
        }
    }

    fn resolve(&self, identifier: InternId) -> Option<BoundId> {
        for (index, intern_id) in self.locals.iter().rev().enumerate() {
            if identifier == *intern_id {
                return Some(BoundId(index));
            }
        }

        None
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Capture {
    Local(BoundId),
    Outer(BoundId)
}

impl Display for Capture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(id) => write!(f, "local({})", id.0),
            Self::Outer(id) => write!(f, "outer({})", id.0),
        }
    }
}

impl Debug for Capture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Bound {
    Local(BoundId),
    Capture(BoundId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
            Self::Capture(id) => write!(f, "captured({})", id.0),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ResolutionError {
    UnboundIdentifier(InternId)
}
