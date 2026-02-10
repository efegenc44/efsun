use std::fmt::{Debug, Display};

use crate::{
    error::{Result, located_error},
    expression::{
        ApplicationExpression, Expression, IdentifierExpression, LambdaExpression,
        LetExpression, Resolved, Unresolved
    },
    interner::InternId,
    location::{Located, SourceLocation},
    anf::{self, Atom, ANF, Identifier},
};

pub struct ExpressionResolver {
    stack: Stack<InternId>,
}

impl ExpressionResolver {
    pub fn new() -> Self {
        let mut stack = Stack::new();
        stack.push_frame();

        ExpressionResolver {
            stack
        }
    }

    pub fn expression(&mut self, expression: Located<Expression<Unresolved>>) -> Result<Located<Expression<Resolved>>> {
        let (expression, start, end) = expression.destruct();

        let expression = match expression {
            Expression::String(string) => Expression::String(string),
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

        let bound = match self.stack.current_frame().resolve(intern_id) {
            Some(id) => Ok(Bound::Local(id)),
            None => {
                match self.stack.capture(intern_id) {
                    Some(capture) => {
                        let id = match self.stack.current_frame().captures.iter().position(|c| *c == capture) {
                            Some(id) => id,
                            None => {
                                self.stack.current_frame_mut().captures.push(capture);
                                self.stack.current_frame().captures.len() - 1
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

    fn lambda(&mut self, lambda: LambdaExpression<Unresolved>) -> Result<LambdaExpression<Resolved>> {
        let (variable, expression) = lambda.destruct();

        self.stack.push_frame();
        self.stack.push_local(*variable.data());
        let expression = self.expression(expression)?;
        self.stack.pop_local();
        let captures = self.stack.pop_frame();

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
        self.stack.push_frame();
        self.stack.push_local(*variable.data());
        let return_expression = self.expression(return_expression)?;
        self.stack.pop_local();
        let captures = self.stack.pop_frame();

        Ok(LetExpression::<Resolved>::new(variable, variable_expression, return_expression, captures))
    }
}

pub struct ANFResolver {
    stack: Stack<anf::Identifier>,
}

impl ANFResolver {
    pub fn new() -> Self {
        let mut stack = Stack::new();
        stack.push_frame();

        ANFResolver {
            stack
        }
    }

    pub fn expression(&mut self, anf: ANF<Unresolved>) -> ANF<Resolved> {
        match anf {
            ANF::Let(letin) => ANF::Let(self.letin(letin)),
            ANF::Application(application) => ANF::Application(self.application(application)),
            ANF::Atom(atom) => ANF::Atom(self.atom(atom)),
        }
    }

    fn atom(&mut self, atom: Atom<Unresolved>) -> Atom<Resolved> {
        match atom {
            Atom::String(id) => Atom::String(id),
            Atom::Identifier(identifier) => Atom::Identifier(self.identifier(identifier)),
            Atom::Lambda(lambda) => Atom::Lambda(self.lambda(lambda)),
        }
    }

    fn identifier(&mut self, identifier: anf::IdentifierExpression<Unresolved>) -> anf::IdentifierExpression<Resolved> {
        let id = identifier.identifier();

        let bound = match self.stack.current_frame().resolve(id) {
            Some(id) => Bound::Local(id),
            None => {
                let capture = self.stack.capture(id).unwrap();
                let id = match self.stack.current_frame().captures.iter().position(|c| *c == capture) {
                    Some(id) => id,
                    None => {
                        self.stack.current_frame_mut().captures.push(capture);
                        self.stack.current_frame().captures.len() - 1
                    }
                };

                Bound::Capture(BoundId(id))
            },
        };

        identifier.resolve(bound)
    }

    fn lambda(&mut self, lambda: anf::LambdaExpression<Unresolved>) -> anf::LambdaExpression<Resolved> {
        let (variable, expression) = lambda.destruct();

        self.stack.push_frame();
        self.stack.push_local(Identifier::Normal(variable));
        let expression = self.expression(expression);
        self.stack.pop_local();
        let captures = self.stack.pop_frame();

        anf::LambdaExpression::new(variable, expression, captures)
    }

    fn letin(&mut self, letin: anf::LetExpression<Unresolved>) -> anf::LetExpression<Resolved> {
        let (variable, variable_expression, return_expression) = letin.destruct();

        let variable_expression = self.atom(variable_expression);
        self.stack.push_local(Identifier::Normal(variable));
        let return_expression = self.expression(return_expression);
        self.stack.pop_local();

        anf::LetExpression::new(variable, variable_expression, return_expression)
    }

    fn application(&mut self, application: anf::ApplicationExpression<Unresolved>) -> anf::ApplicationExpression<Resolved> {
        let (variable, function, argument, expression) = application.destruct();

        let function = self.atom(function);
        let argument = self.atom(argument);
        self.stack.push_local(variable);
        let expression = self.expression(expression);
        self.stack.pop_local();

        anf::ApplicationExpression::new(variable, function, argument, expression)
    }
}

pub struct Stack<T>(Vec<Frame<T>>);

impl<T: Eq + Copy + Clone> Stack<T> {
    fn new() -> Self {
        Self(vec![])
    }

    fn current_frame(&self) -> &Frame<T> {
        self.0.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut Frame<T> {
        self.0.last_mut().unwrap()
    }

    fn push_frame(&mut self) {
        self.0.push(Frame::new())
    }

    fn pop_frame(&mut self) -> Vec<Capture> {
        self.0.pop().unwrap().captures
    }

    fn push_local(&mut self, local: T) {
        self.0.last_mut().unwrap().locals.push(local);
    }

    fn pop_local(&mut self) {
        self.0.last_mut().unwrap().locals.pop();
    }

    fn capture(&mut self, identifier: T) -> Option<Capture> {
        self.capture_in_frame(identifier, 1)
    }

    fn capture_in_frame(&mut self, identifier: T, frame_depth: usize) -> Option<Capture> {
        if frame_depth == self.0.len() {
            return None;
        }

        let index = self.0.len() - 1 - frame_depth;

        let capture = match self.0[index].resolve(identifier) {
            Some(id) => Capture::Local(id),
            None => {
                let capture = self.capture_in_frame(identifier, frame_depth + 1)?;

                let id = match self.0[index].captures.iter().position(|c| *c == capture) {
                    Some(id) => id,
                    None => {
                        self.0[index].captures.push(capture);
                        self.0[index].captures.len() - 1
                    },
                };

                Capture::Outer(BoundId(id))
            },
        };

        Some(capture)
    }

}

pub struct Frame<T> {
    locals: Vec<T>,
    captures: Vec<Capture>,
}

impl<T: Eq> Frame<T> {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            captures: Vec::new(),
        }
    }

    fn resolve(&self, identifier: T) -> Option<BoundId> {
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
