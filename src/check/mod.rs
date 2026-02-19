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
    resolution::{Resolved, bound::{Bound, Capture, Path}},
    error::{Result, located_error}
};

use typ::{Type, MonoType, ArrowType};

pub struct TypeChecker {
    frames: Vec<Frame>,
    name_exprs: HashMap<Path, (Located<Expression<Resolved>>, bool)>,
    names: HashMap<Path, Type>,
    newvar_counter: usize,
    unification_table: HashMap<usize, MonoType>
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            frames: vec![Frame::with_captures(vec![])],
            name_exprs: HashMap::new(),
            names: HashMap::new(),
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

        self.push_local(variable_type);
        let return_type = self.infer(letin.return_expression())?;
        self.pop_local();

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
        // self.collect_names(definitions)?;

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
