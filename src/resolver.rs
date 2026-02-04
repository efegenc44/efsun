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
    stack: Vec<Frame>,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            stack: vec![Frame::new()]
        }
    }

    fn current_frame(&self) -> &Frame {
        self.stack.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        self.stack.last_mut().unwrap()
    }

    fn push_frame(&mut self) {
        self.stack.push(Frame::new())
    }

    fn pop_frame(&mut self) -> Vec<Capture> {
        self.stack.pop().unwrap().captures
    }

    fn push_local(&mut self, local: InternId) {
        self.stack.last_mut().unwrap().locals.push(local);
    }

    fn pop_local(&mut self) {
        self.stack.last_mut().unwrap().locals.pop();
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
        let intern_id = *identifier.identifier().data();

        let bound = match self.current_frame().resolve(intern_id) {
            Some(id) => Ok(Bound::Local(id)),
            None => {
                match self.capture(intern_id) {
                    Some(capture) => {
                        let id = match self.current_frame().captures.iter().position(|c| *c == capture) {
                            Some(id) => id,
                            None => {
                                self.current_frame_mut().captures.push(capture);
                                self.current_frame().captures.len() - 1
                            }
                        };

                        Ok(Bound::Capture(BoundId(id)))
                    },
                    None => {
                        let error = ResolutionError::UnboundIdentifier(intern_id);
                        Err(located_error(error, start, end))
                    }
                }
            },
        };

        Ok(identifier.resolve(bound?))

    }

    fn capture(&mut self, identifier: InternId) -> Option<Capture> {
        self.capture_in_frame(identifier, 1)
    }

    fn capture_in_frame(&mut self, identifier: InternId, frame_depth: usize) -> Option<Capture> {
        if frame_depth == self.stack.len() {
            return None;
        }

        let index = self.stack.len() - 1 - frame_depth;

        let capture = match self.stack[index].resolve(identifier) {
            Some(id) => Capture::Local(id),
            None => {
                let capture = self.capture_in_frame(identifier, frame_depth + 1)?;

                let id = match self.stack[index].captures.iter().position(|c| *c == capture) {
                    Some(id) => id,
                    None => {
                        self.stack[index].captures.push(capture);
                        self.stack[index].captures.len() - 1
                    },
                };

                Capture::Outer(BoundId(id))
            },
        };

        Some(capture)
    }

    fn lambda(&mut self, lambda: LambdaExpression<Unresolved>) -> Result<LambdaExpression<Resolved>> {
        let (variable, expression) = lambda.destruct();

        self.push_frame();
        self.push_local(*variable.data());
        let expression = self.expression(expression)?;
        self.pop_local();
        let captures = self.pop_frame();

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
        self.push_frame();
        self.push_local(*variable.data());
        let return_expression = self.expression(return_expression)?;
        self.pop_local();
        let captures = self.pop_frame();

        Ok(LetExpression::<Resolved>::new(variable, variable_expression, return_expression, captures))
    }
}

pub struct Frame {
    locals: Vec<InternId>,
    captures: Vec<Capture>,
}

impl Frame {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            captures: Vec::new(),
        }
    }

    fn resolve(&self, identifier: InternId) -> Option<BoundId> {
        for (index, intern_id) in self.locals.iter().rev().enumerate() {
            if identifier == *intern_id {
                return Some(BoundId(self.locals.len() - 1 - index));
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
