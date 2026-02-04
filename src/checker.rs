use std::{collections::HashMap, result};

use crate::{
    expression::{
        ApplicationExpression, Expression, IdentifierExpression,
        LambdaExpression, Resolved, LetExpression
    },
    location::{Located, SourceLocation},
    resolver::{Bound, Capture},
    typ::{Type, MonoType, ArrowType},
    error::{Result, located_error}
};

pub struct TypeChecker {
    frames: Vec<Frame>,
    newvar_counter: usize,
    unification_table: HashMap<usize, MonoType>
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            frames: vec![Frame::with_captures(vec![])],
            newvar_counter: 0,
            unification_table: HashMap::default()
        }
    }

    fn current_frame(&self) -> &Frame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().unwrap()
    }

    fn push_frame(&mut self, captures: Vec<Capture>) {
        self.frames.push(Frame::with_captures(captures));
    }

    fn pop_frame(&mut self) {
        self.frames.pop().unwrap();
    }

    fn push_local(&mut self, local: Type) {
        self.frames.last_mut().unwrap().locals.push(local);
    }

    fn pop_local(&mut self) {
        self.frames.last_mut().unwrap().locals.pop();
    }

    fn newvar(&mut self) -> MonoType {
        let variable = MonoType::Variable(self.newvar_counter);
        self.newvar_counter += 1;
        variable
    }

    fn instantiate(&mut self, t: Type) -> MonoType {
        match t {
            Type::Mono(mono) => mono,
            Type::Poly(variables, mono) => {
                let mut table = HashMap::new();
                for variable in variables {
                    table.insert(variable, self.newvar());
                }

                mono.substitute(&table)
            },
        }
    }

    pub fn infer(&mut self, expression: &Located<Expression<Resolved>>) -> Result<MonoType> {
        match expression.data() {
            Expression::String(_) => Ok(MonoType::String),
            Expression::Identifier(identifier) => self.identifier(identifier),
            Expression::Application(application) => self.application(application, expression.start(), expression.end()),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Let(letin) => self.letin(letin),
        }
    }

    fn substitute(&self, t: MonoType) -> MonoType {
        t.substitute(&self.unification_table)
    }

    fn unify(&mut self, t1: &MonoType, t2: &MonoType) -> result::Result<(), (MonoType, MonoType)> {

        match (t1, t2) {
            (MonoType::Variable(id1), MonoType::Variable(id2)) => {
                match (self.unification_table.get(id1), self.unification_table.get(id2)) {
                    (None, None) => {
                        let newvar = self.newvar();
                        self.unification_table.insert(*id1, newvar.clone());
                        self.unification_table.insert(*id2, newvar);

                        Ok(())
                    },
                    (None, Some(t)) => self.unify(t1, &t.clone()),
                    (Some(t), None) => self.unify(&t.clone(), t2),
                    (Some(t1), Some(t2)) => self.unify(&t1.clone(), &t2.clone()),
                }
            },
            (MonoType::Variable(id), t) | (t, MonoType::Variable(id)) => {
                if self.substitute(t.clone()).includes(*id) {
                    return Err((t1.clone(), t2.clone()));
                }

                match self.unification_table.get(id) {
                    Some(k) => self.unify(t, &k.clone()),
                    None => {
                        self.unification_table.insert(*id, t.clone());
                        Ok(())
                    },
                }
            },
            (MonoType::Arrow(arrow1), MonoType::Arrow(arrow2)) => {
                self.unify(arrow1.from(), arrow2.from())?;
                self.unify(arrow1.to(), arrow2.to())
            },
            (MonoType::String, MonoType::String) => Ok(()),
            _ => Err((t1.clone(), t2.clone()))
        }
    }

    fn identifier(&mut self, identifier: &IdentifierExpression<Resolved>) -> Result<MonoType> {
        match identifier.bound() {
            Bound::Local(id) => {
                let index = self.current_frame().locals.len() - 1 - id.value();

                if let Type::Mono(mono) = &self.current_frame().locals[index] {
                    self.current_frame_mut().locals[index] = Type::Mono(self.substitute(mono.clone()));
                }

                let t = self.current_frame().locals[index].clone();
                Ok(self.instantiate(t))
            },
            Bound::Capture(capture) => {
                let capture = self.current_frame().captures[capture.value()];
                Ok(self.get_capture(capture))
            }
        }
    }

    fn get_capture(&mut self, capture: Capture) -> MonoType {
        self.get_capture_in_frame(capture, 1)
    }

    fn get_capture_in_frame(&mut self, capture: Capture, frame_depth: usize) -> MonoType {
        let index = self.frames.len() - 1 - frame_depth;

        match capture {
            Capture::Local(id) => {
                let local_index = self.frames[index].locals.len() - 1 - id.value();
                let t = self.frames[index].locals[local_index].clone();
                self.instantiate(t)
            },
            Capture::Outer(id) => {
                let capture = self.frames[index].captures[id.value()];
                self.get_capture_in_frame(capture, frame_depth + 1)
            },
        }
    }

    fn application(
        &mut self,
        application: &ApplicationExpression<Resolved>,
        start: SourceLocation,
        end: SourceLocation
    ) -> Result<MonoType> {
        let return_type = self.newvar();
        let function = self.infer(application.function())?;
        let argument = self.infer(application.argument())?;
        let arrow = ArrowType::new(argument.clone(), return_type.clone());
        let arrow = MonoType::Arrow(arrow);

        if let Err((first, second)) = self.unify(&function, &arrow) {
            let error = TypeCheckError::TypeMismatch {
                first: self.substitute(first),
                second: self.substitute(second)
            };
            Err(located_error(error, start, end))
        } else {
            Ok(self.substitute(return_type))
        }
    }

    fn lambda(&mut self, lambda: &LambdaExpression<Resolved>) -> Result<MonoType> {
        let argument = self.newvar();

        self.push_frame(lambda.captures().to_vec());
        self.push_local(Type::Mono(argument.clone()));
        let return_type = self.infer(lambda.expression())?;
        self.pop_local();
        self.pop_frame();

        let arrow = ArrowType::new(
            self.substitute(argument),
            self.substitute(return_type)
        );

        Ok(MonoType::Arrow(arrow))
    }

    fn letin(&mut self, letin: &LetExpression<Resolved>) -> Result<MonoType> {
        let variable_type = self.infer(letin.variable_expression())?;
        let variable_type = variable_type.generalize();

        self.push_frame(letin.captures().to_vec());
        self.push_local(variable_type);
        let return_type = self.infer(letin.return_expression())?;
        self.pop_local();
        self.pop_frame();

        Ok(self.substitute(return_type))
    }
}

struct Frame {
    locals: Vec<Type>,
    captures: Vec<Capture>,
}

impl Frame {
    pub fn with_captures(captures: Vec<Capture>) -> Self {
        Self {
            locals: Vec::new(),
            captures,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    TypeMismatch {
        first: MonoType,
        second: MonoType,
    }
}
