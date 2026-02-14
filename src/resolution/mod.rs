pub mod frame;
pub mod bound;

use std::fmt::Debug;

use crate::{
    error::{Result, located_error},
    parse::expression::{
        ApplicationExpression, Expression, IdentifierExpression, LambdaExpression,
        LetExpression
    },
    interner::InternId,
    location::{Located, SourceLocation},
    compile::anf::{self, Atom, ANF, Identifier},
};

use bound::{Bound, BoundId};
use frame::Stack;

#[derive(Clone, Copy)]
pub struct Resolved;
#[derive(Clone, Copy)]
pub struct Unresolved;

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
                        let id = match self.stack.current_frame().captures().iter().position(|c| *c == capture) {
                            Some(id) => id,
                            None => {
                                self.stack.current_frame_mut().captures_mut().push(capture);
                                self.stack.current_frame().captures().len() - 1
                            }
                        };

                        Ok(Bound::Capture(BoundId::new(id)))
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
                let id = match self.stack.current_frame().captures().iter().position(|c| *c == capture) {
                    Some(id) => id,
                    None => {
                        self.stack.current_frame_mut().captures_mut().push(capture);
                        self.stack.current_frame().captures().len() - 1
                    }
                };

                Bound::Capture(BoundId::new(id))
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

#[derive(Debug, Clone, Copy)]
pub enum ResolutionError {
    UnboundIdentifier(InternId)
}
