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
    scopes: Vec<Scope>,
    newvar_counter: usize,
    unification_table: HashMap<usize, MonoType>
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
            newvar_counter: 0,
            unification_table: HashMap::default()
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn push_local(&mut self, local: Type) {
        self.scopes.last_mut().unwrap().locals.push(local);
    }

    fn pop_local(&mut self) {
        self.scopes.last_mut().unwrap().locals.pop();
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
        }
    }

    fn identifier(&mut self, identifier: &IdentifierExpression<Resolved>) -> Result<MonoType> {
        match identifier.bound() {
            Bound::Local(id) => {
                let scope_index = self.scopes.len() - 1;

                let index = self.scopes[scope_index].locals.len() - 1 - id.value();

                if let Type::Mono(mono) = &self.scopes[scope_index].locals[index] {
                    self.scopes[scope_index].locals[index] = Type::Mono(self.substitute(mono.clone()));
                }

                let t = self.scopes[scope_index].locals[index].clone();
                Ok(self.instantiate(t))
            },
            Bound::Capture(capture) => {
                let capture = self.scopes[self.scopes.len() - 1].captures[capture.value()];
                Ok(self.get_capture(capture, self.scopes.len() - 2))
            }
        }
    }

    fn get_capture(&mut self, capture: Capture, scope_index: usize) -> MonoType {
        match capture {
            Capture::Local(id) => {
                let index = self.scopes[scope_index].locals.len() - 1 - id.value();
                let t = self.scopes[scope_index].locals[index].clone();
                self.instantiate(t)
            },
            Capture::Outer(id) => {
                let capture = self.scopes[scope_index].captures[id.value()];
                self.get_capture(capture, scope_index - 1)
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

        self.enter_scope();

        let scope_index = self.scopes.len() - 1;
        self.scopes[scope_index].captures = lambda.captures().to_vec();

        self.push_local(Type::Mono(argument.clone()));
        let return_type = self.infer(lambda.expression())?;
        self.pop_local();
        self.exit_scope();

        let arrow = ArrowType::new(
            self.substitute(argument),
            self.substitute(return_type)
        );

        Ok(MonoType::Arrow(arrow))
    }

    fn letin(&mut self, letin: &LetExpression<Resolved>) -> Result<MonoType> {
        let variable_type = self.infer(letin.variable_expression())?;
        let variable_type = variable_type.generalize();

        self.push_local(variable_type);
        let return_type = self.infer(letin.return_expression())?;
        self.pop_local();

        Ok(self.substitute(return_type))
    }
}

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    TypeMismatch {
        first: MonoType,
        second: MonoType,
    }
}

struct Scope {
    locals: Vec<Type>,
    captures: Vec<Capture>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            captures: Vec::new(),
        }
    }
}
