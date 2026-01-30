use std::{collections::HashMap, result};

use crate::{
    expression::{
        ApplicationExpression, Expression, IdentifierExpression,
        LambdaExpression, Resolved
    },
    location::{Located, SourceLocation},
    resolver::Bound,
    typ::{MonoType, ArrowType},
    error::{Result, located_error}
};

pub struct TypeChecker {
    locals: Vec<MonoType>,
    newvar_counter: usize,
    unification_table: HashMap<usize, MonoType>
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            locals: Vec::default(),
            newvar_counter: 0,
            unification_table: HashMap::default()
        }
    }

    fn newvar(&mut self) -> MonoType {
        let variable = MonoType::Variable(self.newvar_counter);
        self.newvar_counter += 1;
        variable
    }

    pub fn infer(&mut self, expression: &Located<Expression<Resolved>>) -> Result<MonoType> {
        match expression.data() {
            Expression::Identifier(identifier) => self.identifier(identifier),
            Expression::Application(application) => self.application(application, expression.start(), expression.end()),
            Expression::Lambda(lambda) => self.lambda(lambda),
        }
    }

    fn substitute(&self, t: MonoType) -> MonoType {
        t.substitute(&self.unification_table)
    }

    fn unify(&mut self, t1: &MonoType, t2: &MonoType) -> result::Result<(), (MonoType, MonoType)> {
        match (t1, t2) {
            (MonoType::Variable(id1), MonoType::Variable(id2)) => {
                self.unification_table.insert(*id1, MonoType::Variable(*id2));
                self.unification_table.insert(*id2, MonoType::Variable(*id1));
                Ok(())
            },
            (MonoType::Variable(id), t) |
            (t, MonoType::Variable(id)) => {
                if t.includes(*id) {
                    return Err((t1.clone(), t2.clone()));
                }

                self.unification_table.insert(*id, t.clone());
                Ok(())
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
                let index = self.locals.len() - 1 - id.value();
                self.locals[index] = self.substitute(self.locals[index].clone());
                Ok(self.locals[index].clone())
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
            let error = TypeCheckError::TypeMismatch { first, second };
            Err(located_error(error, start, end))
        } else {
            Ok(self.substitute(return_type))
        }
    }

    fn lambda(&mut self, lambda: &LambdaExpression<Resolved>) -> Result<MonoType> {
        let argument = self.newvar();
        self.locals.push(argument.clone());
        let return_type = self.infer(lambda.expression())?;
        self.locals.pop();

        let arrow = ArrowType::new(
            self.substitute(argument),
            self.substitute(return_type)
        );

        Ok(MonoType::Arrow(arrow))
    }
}

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    TypeMismatch {
        first: MonoType,
        second: MonoType,
    }
}
