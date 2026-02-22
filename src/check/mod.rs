pub mod typ;

use std::{collections::HashMap, result};

use crate::{
    parse::{
        expression::{
            ApplicationExpression, Expression, PathExpression,
            LambdaExpression, LetExpression
        },
        definition::{Definition, NameDefinition}
    },
    location::{Located, SourceLocation},
    resolution::{Resolved, frame::CheckStack, bound::{Bound, Path}},
    error::{Result, located_error}
};

use typ::{Type, MonoType, ArrowType};

pub struct TypeChecker {
    stack: CheckStack<Type>,
    name_exprs: HashMap<Path, (Located<Expression<Resolved>>, bool)>,
    names: HashMap<Path, Type>,
    newvar_counter: usize,
    unification_table: HashMap<usize, MonoType>
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut stack = CheckStack::new();
        stack.push_frame(vec![]);

        Self {
            stack,
            name_exprs: HashMap::new(),
            names: HashMap::new(),
            newvar_counter: 0,
            unification_table: HashMap::default()
        }
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
            Expression::Path(path) => self.path(path),
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

    fn path(&mut self, path: &PathExpression<Resolved>) -> Result<MonoType> {
        match path.bound() {
            Bound::Local(id) => {
                let t = self.stack.get_local(*id);
                Ok(self.instantiate(t))
            },
            Bound::Capture(id) => {
                let t = self.stack.get_capture(*id);
                Ok(self.instantiate(t))
            }
            Bound::Absolute(path) => {
                if let Some(t) = self.names.get(path) {
                    Ok(self.instantiate(t.clone()))
                } else {
                    // TODO: Ideally don't clone here
                    let (expr, status) = self.name_exprs[path].clone();

                    if status {
                        panic!("Cyclic definition");
                    }

                    self.name_exprs.get_mut(path).unwrap().1 = true;

                    let m = self.infer(&expr)?;

                    self.name_exprs.get_mut(path).unwrap().1 = false;

                    let t = m.generalize();
                    self.names.insert(path.clone(), t.clone());
                    Ok(self.instantiate(t))
                }
            }
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

        self.stack.push_frame(lambda.captures().to_vec());
        self.stack.push_local(Type::Mono(argument.clone()));
        let return_type = self.infer(lambda.expression())?;
        self.stack.pop_local();
        self.stack.pop_frame();

        let arrow = ArrowType::new(
            self.substitute(argument),
            self.substitute(return_type)
        );

        Ok(MonoType::Arrow(arrow))
    }

    fn letin(&mut self, letin: &LetExpression<Resolved>) -> Result<MonoType> {
        let variable_type = self.infer(letin.variable_expression())?;
        let variable_type = variable_type.generalize();

        self.stack.push_local(variable_type);
        let return_type = self.infer(letin.return_expression())?;
        self.stack.pop_local();

        Ok(self.substitute(return_type))
    }

    pub fn program(&mut self, modules: &[Vec<Definition<Resolved>>]) -> Result<()> {
        for module in modules {
            self.collect_names(module)?;
        }

        for module in modules {
            self.module(module)?;
        }

        Ok(())
    }

    pub fn module(&mut self, definitions: &[Definition<Resolved>]) -> Result<()> {
        for definition in definitions {
            match definition {
                Definition::Module(_) => (),
                Definition::Name(name) => self.let_definition(name)?,
                Definition::Import(_) => (),
            }
        }

        Ok(())
    }

    fn collect_names(&mut self, definitions: &[Definition<Resolved>]) -> Result<()> {
        for definition in definitions {
            if let Definition::Name(name) = definition {
                self.name_exprs.insert(name.path().clone(), (name.expression().clone(), false));
            }
        }

        Ok(())
    }

    fn let_definition(&mut self, let_definition: &NameDefinition<Resolved>) -> Result<()> {
        self.name_exprs.get_mut(let_definition.path()).unwrap().1 = true;

        let m = self.infer(let_definition.expression())?;
        let t = m.generalize();
        self.names.insert(let_definition.path().clone(), t);

        self.name_exprs.get_mut(let_definition.path()).unwrap().1 = false;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    TypeMismatch {
        first: MonoType,
        second: MonoType,
    }
}
