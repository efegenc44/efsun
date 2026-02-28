pub mod typ;

use std::{collections::HashMap, result};

use crate::{
    parse::{
        expression::{
            ApplicationExpression, Expression, PathExpression,
            LambdaExpression, LetExpression, MatchExpression, MatchBranch, Pattern
        },
        type_expression::{
            TypeExpression, PathTypeExpression, ApplicationTypeExpression
        },
        definition::{
            Definition, NameDefinition
        }
    },
    location::{Located, Span},
    resolution::{Resolved, frame::CheckStack, bound::{Bound, Path}},
    error::{Result, located_error}
};

use typ::{Type, MonoType, ArrowType, StructureType};

pub struct TypeChecker<'ast> {
    stack: CheckStack<Type>,
    type_locals: Vec<MonoType>,
    name_expressions: ExpressionMap<'ast>,
    names: HashMap<Path, Type>,
    types: HashMap<&'ast Path, usize>,
    newvar_counter: usize,
    unification_table: HashMap<usize, MonoType>,
    current_source_name: String,
}

impl<'ast> TypeChecker<'ast> {
    pub fn new() -> Self {
        let mut stack = CheckStack::new();
        stack.push_frame(vec![]);

        Self {
            stack,
            type_locals: Vec::new(),
            name_expressions: ExpressionMap::new(),
            names: HashMap::new(),
            types: HashMap::new(),
            newvar_counter: 0,
            unification_table: HashMap::default(),
            current_source_name: String::new(),
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

    fn eval_type_expression(&mut self, expression: &Located<TypeExpression<Resolved>>) -> MonoType {
        match expression.data() {
            TypeExpression::Path(path) => self.eval_type_path(path),
            TypeExpression::Application(application) => self.eval_type_application(application),
        }
    }

    fn eval_type_path(&mut self, path: &PathTypeExpression<Resolved>) -> MonoType {
        match path.bound() {
            Bound::Capture(_) => unreachable!(),
            Bound::Local(id) => self.type_locals[id.value()].clone(),
            Bound::Absolute(path) => {
                let mut arguments = Vec::new();
                for _ in 0..self.types[path] {
                    arguments.push(self.newvar());
                }

                let structure = StructureType::new(path.clone(), arguments);
                MonoType::Structure(structure)
            },
        }
    }

    fn eval_type_application(&mut self, application: &ApplicationTypeExpression<Resolved>) -> MonoType {
        let MonoType::Structure(structure) = self.eval_type_expression(application.function()) else {
            todo!("Expected a structure");
        };

        let true = application.arguments().len() == structure.arguments().len() else {
            todo!("Type artiy mismatch");
        };

        let mut application_arguments = Vec::new();
        for argument in application.arguments() {
            application_arguments.push(self.eval_type_expression(argument));
        }

        let structure = StructureType::new(structure.path().clone(), application_arguments);
        MonoType::Structure(structure)
    }

    pub fn infer(&mut self, expression: &Located<Expression<Resolved>>) -> Result<MonoType> {
        match expression.data() {
            Expression::String(_) => Ok(MonoType::String),
            Expression::Path(path) => self.path(path, expression.span()),
            Expression::Application(application) => self.application(application, expression.span()),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Let(letin) => self.letin(letin),
            Expression::Match(matchlet) => self.matchlet(matchlet),
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
            (MonoType::Structure(structure1), MonoType::Structure(structure2)) => {
                if structure1.path() != structure2.path() {
                    Err((t1.clone(), t2.clone()))
                } else {
                    let arguments = structure1.arguments().iter().zip(structure2.arguments());
                    for (argument1, argument2) in arguments {
                        self.unify(argument1, argument2)?;
                    }

                    Ok(())
                }
            },
            (MonoType::String, MonoType::String) => Ok(()),
            _ => Err((t1.clone(), t2.clone()))
        }
    }

    fn path(&mut self, path: &PathExpression<Resolved>, span: Span) -> Result<MonoType> {
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
                    if self.name_expressions.is_currently_visiting(path) {
                        let error = TypeCheckError::CyclicDefinition(path.clone());
                        return Err(located_error(error, span, self.current_source_name.clone()));
                    }

                    self.name_expressions.visiting(path);
                    let m = self.infer(self.name_expressions.get(path))?;
                    self.name_expressions.leaving(path);

                    let t = m.generalize();
                    self.names.insert(path.clone(), t.clone());
                    Ok(self.instantiate(t))
                }
            }
        }
    }

    fn application(&mut self, application: &ApplicationExpression<Resolved>, span: Span) -> Result<MonoType> {
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
            Err(located_error(error, span, self.current_source_name.clone()))
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

    fn matchlet(&mut self, matchlet: &MatchExpression<Resolved>) -> Result<MonoType> {
        let t = self.infer(matchlet.expression())?;

        let return_type = self.newvar();

        for branch in matchlet.branches() {
            let t = self.match_branch(&t, branch.data())?;

            if let Err((first, second)) = self.unify(&return_type, &t) {
                let error = TypeCheckError::TypeMismatch {
                    first: self.substitute(first),
                    second: self.substitute(second)
                };
                return Err(located_error(error, branch.span(), self.current_source_name.clone()));
            }
        }

        Ok(self.substitute(return_type))
    }

    fn match_branch(&mut self, t: &MonoType, branch: &MatchBranch<Resolved>) -> Result<MonoType> {
        let len = self.stack.len();
        self.pattern(t, branch.pattern().data())?;
        let m = self.infer(branch.expression())?;
        self.stack.truncate(len);

        Ok(m)
    }

    fn pattern(&mut self, t: &MonoType, pattern: &Pattern<Resolved>) -> Result<()> {
        match pattern {
            Pattern::Any(_) => {
                self.stack.push_local(Type::Mono(t.clone()));
            },
            Pattern::String(_) => {
                let Ok(_) = self.unify(t, &MonoType::String) else {
                    todo!("Error");
                };
            },
            Pattern::Structure(structure) => {
                let type_path = structure.type_path();

                let mut arguments = Vec::new();
                for _ in 0..self.types[type_path] {
                    arguments.push(self.newvar());
                }

                let structure_t = StructureType::new(type_path.clone(), arguments);
                let structure_t = MonoType::Structure(structure_t);

                let Ok(_) = self.unify(t, &structure_t) else {
                    todo!("Error");
                };

                let constructor = structure.parts().data().last().unwrap();
                let constructor_path = type_path.append(*constructor);

                let m = self.instantiate(self.names[&constructor_path].clone());
                if let MonoType::Arrow(constructor) = m {
                    let mut t = &constructor;
                    for argument in structure.arguments() {
                        self.pattern(t.from(), argument.data())?;
                        if let MonoType::Arrow(arrow) = t.to() {
                            t = arrow;
                        }
                    }
                } else {
                    assert!(matches!(m, MonoType::Structure(_)));
                }
            },
        }

        Ok(())
    }

    pub fn program(&mut self, modules: &'ast [(Vec<Definition<Resolved>>, String)]) -> Result<()> {
        for (module, _) in modules {
            self.collect_names(module)?;
        }


        for (module, source_name) in modules {
            self.current_source_name = source_name.clone();
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
                Definition::Structure(_) => (),
            }
        }

        Ok(())
    }

    fn collect_names(&mut self, definitions: &'ast [Definition<Resolved>]) -> Result<()> {
        for definition in definitions {
            if let Definition::Name(name) = definition {
                self.name_expressions.add(name.path(), name.expression());
            }

            if let Definition::Structure(structure) = definition {
                self.types.insert(structure.path(), structure.variables().len());
            }
        }

        for definition in definitions {
            if let Definition::Structure(structure) = definition {
                let mut arguments = Vec::new();
                for _ in 0..self.types[structure.path()] {
                    let newvar = self.newvar();
                    arguments.push(newvar.clone());
                    self.type_locals.push(newvar);
                }

                let structure_t = StructureType::new(structure.path().clone(), arguments);
                for constructor in structure.constructors() {
                    let mut t = MonoType::Structure(structure_t.clone());
                    for argument in constructor.arguments().iter().rev() {
                        let argument = self.eval_type_expression(argument);
                        t = MonoType::Arrow(ArrowType::new(argument, t));
                    }

                    self.names.insert(constructor.path().clone(), t.generalize());
                }

                self.type_locals.clear();
            }
        }

        Ok(())
    }

    fn let_definition(&mut self, let_definition: &NameDefinition<Resolved>) -> Result<()> {
        self.name_expressions.visiting(let_definition.path());
        let m = self.infer(let_definition.expression())?;
        let t = m.generalize();
        self.names.insert(let_definition.path().clone(), t);
        self.name_expressions.leaving(let_definition.path());

        Ok(())
    }
}

pub struct ExpressionMap<'ast> {
    map: HashMap<&'ast Path, (&'ast Located<Expression<Resolved>>, bool)>,
}

impl<'ast> ExpressionMap<'ast> {
    fn new() -> Self {
        Self { map: HashMap::new() }
    }

    fn add(&mut self, path: &'ast Path, expression: &'ast Located<Expression<Resolved>>) {
        self.map.insert(path, (expression, false));
    }

    fn get(&self, path: &Path) -> &'ast Located<Expression<Resolved>> {
        self.map.get(path).unwrap().0
    }

    fn is_currently_visiting(&self, path: &Path) -> bool {
        self.map.get(path).unwrap().1
    }

    fn visiting(&mut self, path: &Path) {
        self.map.get_mut(path).unwrap().1 = true;
    }

    fn leaving(&mut self, path: &Path) {
        self.map.get_mut(path).unwrap().1 = false;
    }
}

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    TypeMismatch {
        first: MonoType,
        second: MonoType,
    },
    CyclicDefinition(Path),
}
