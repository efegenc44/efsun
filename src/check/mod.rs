pub mod typ;

use std::{collections::HashMap, result};

use crate::{
    error::{ReportableError, Result},
    interner::Interner,
    location::{Located, Span},
    parse::{
        definition::{self, Definition, Module, Program},
        expression::{self, Expression},
        pattern::Pattern,
        type_expression::{self, TypeExpression},
    },
    resolution::{
        bound::{Bound, Path},
        frame::CheckStack,
    },
    state::Resolved,
};

use typ::{ArrowType, MonoType, StructureType, Type};

pub struct TypeChecker<'ast> {
    /// Stack for local variables
    stack: CheckStack<Type>,
    /// Stack for type variables in structure definitions
    type_variables: Vec<MonoType>,
    /// True if currently type checking a lambda expression.
    ///     Its purpose is to check for cyclic definitions
    in_lambda: bool,
    /// Map for accesing expressions of let definitions and keeping track of cylic definitions
    name_expressions: ExpressionMap<'ast>,
    /// Map from path of the let definition to its type
    names: HashMap<&'ast Path, Type>,
    /// Map from path of the structure definition to its corresponding type
    types: HashMap<&'ast Path, Type>,
    /// Counter for generating fresh type variables
    newvar_counter: usize,
    /// Map from type variable id (usize) to its substitution (MonoType)
    unification_table: HashMap<usize, MonoType>,
    /// Source file name of the current module for error reporting
    current_source_name: Option<&'ast str>,
}

impl<'ast> TypeChecker<'ast> {
    pub fn new() -> Self {
        Self {
            stack: CheckStack::new(),
            type_variables: Vec::new(),
            in_lambda: false,
            name_expressions: ExpressionMap::new(),
            names: HashMap::new(),
            types: HashMap::new(),
            newvar_counter: 0,
            unification_table: HashMap::default(),
            current_source_name: None,
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
                let table = variables
                    .into_iter()
                    .map(|variable| (variable, self.newvar()))
                    .collect();

                mono.substitute(&table)
            }
        }
    }

    fn replace_in_lambda(&mut self, value: bool) -> bool {
        let before = self.in_lambda;
        self.in_lambda = value;
        before
    }

    fn initialize_structure_type(&mut self, path: Path, argument_count: usize) -> Type {
        if argument_count == 0 {
            Type::Mono(MonoType::Structure(StructureType::new(path, vec![])))
        } else {
            let arguments = (0..argument_count).map(|_| self.newvar()).collect();
            MonoType::Structure(StructureType::new(path, arguments)).generalize()
        }
    }

    fn evaluate_type_expression(
        &mut self,
        expression: &Located<TypeExpression<Resolved>>,
    ) -> Result<MonoType> {
        match expression.data() {
            TypeExpression::Path(path) => self.evaluate_type_path(path),
            TypeExpression::Application(application) => {
                self.evaluate_type_application(application, expression.span())
            }
        }
    }

    fn evaluate_type_path(&mut self, path: &type_expression::Path<Resolved>) -> Result<MonoType> {
        let t = match path.bound() {
            Bound::Capture(_) => unreachable!(),
            Bound::Local(id) => self.type_variables[id.value()].clone(),
            Bound::Absolute(path) => {
                let t = self.types[path].clone();
                self.instantiate(t)
            }
        };

        Ok(t)
    }

    fn evaluate_type_application(
        &mut self,
        application: &type_expression::Application<Resolved>,
        span: Span,
    ) -> Result<MonoType> {
        let m = self.evaluate_type_expression(application.function())?;
        let MonoType::Structure(structure) = m else {
            return self.error(TypeCheckError::ExpectedStructure(m), span);
        };

        let expected = structure.arguments().len();
        let found = application.arguments().len();
        let true = found == expected else {
            return self.error(TypeCheckError::TypeArityMismatch { expected, found }, span);
        };

        let mut application_arguments = Vec::new();
        for argument in application.arguments() {
            application_arguments.push(self.evaluate_type_expression(argument)?);
        }

        let structure = StructureType::new(structure.path().clone(), application_arguments);
        Ok(MonoType::Structure(structure))
    }

    pub fn infer(&mut self, expression: &'ast Located<Expression<Resolved>>) -> Result<MonoType> {
        match expression.data() {
            Expression::String(_) => Ok(MonoType::String),
            Expression::Path(path) => self.path(path, expression.span()),
            Expression::Application(application) => {
                self.application(application, expression.span())
            }
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::LetIn(letin) => self.letin(letin),
            Expression::MatchAs(matchlet) => self.matchlet(matchlet),
        }
    }

    fn substitute(&self, t: MonoType) -> MonoType {
        t.substitute(&self.unification_table)
    }

    fn unify(&mut self, t1: &MonoType, t2: &MonoType) -> result::Result<(), (MonoType, MonoType)> {
        match (t1, t2) {
            (MonoType::Variable(id1), MonoType::Variable(id2)) => {
                let m1 = self.unification_table.get(id1);
                let m2 = self.unification_table.get(id2);
                match (m1, m2) {
                    (None, None) => {
                        let newvar = self.newvar();
                        self.unification_table.insert(*id1, newvar.clone());
                        self.unification_table.insert(*id2, newvar);

                        Ok(())
                    }
                    (None, Some(t)) => self.unify(t1, &t.clone()),
                    (Some(t), None) => self.unify(&t.clone(), t2),
                    (Some(t1), Some(t2)) => self.unify(&t1.clone(), &t2.clone()),
                }
            }
            (MonoType::Variable(id), t) | (t, MonoType::Variable(id)) => {
                let t = self.substitute(t.clone());
                if t.includes(*id) {
                    return Err((t1.clone(), t2.clone()));
                }

                match self.unification_table.get(id) {
                    Some(k) => self.unify(&t, &k.clone()),
                    None => {
                        self.unification_table.insert(*id, t.clone());
                        Ok(())
                    }
                }
            }
            (MonoType::Arrow(arrow1), MonoType::Arrow(arrow2)) => {
                self.unify(arrow1.from(), arrow2.from())?;
                self.unify(arrow1.to(), arrow2.to())
            }
            (MonoType::Structure(structure1), MonoType::Structure(structure2)) => {
                if structure1.path() != structure2.path() {
                    Err((self.substitute(t1.clone()), self.substitute(t2.clone())))
                } else {
                    let arguments = structure1.arguments().iter().zip(structure2.arguments());
                    for (argument1, argument2) in arguments {
                        self.unify(argument1, argument2)?;
                    }

                    Ok(())
                }
            }
            (MonoType::String, MonoType::String) => Ok(()),
            _ => Err((self.substitute(t1.clone()), self.substitute(t2.clone()))),
        }
    }

    fn path(&mut self, path: &'ast expression::Path<Resolved>, span: Span) -> Result<MonoType> {
        let t = match path.bound() {
            Bound::Local(id) => self.stack.get_local(*id),
            Bound::Capture(id) => self.stack.get_capture(*id),
            Bound::Absolute(path) => {
                if let Type::Mono(MonoType::Variable(variable)) = self.names[path].clone() {
                    if self.name_expressions.is_currently_visiting(path) {
                        if !self.in_lambda {
                            return self
                                .error(TypeCheckError::CyclicDefinition(path.clone()), span);
                        }

                        return Ok(MonoType::Variable(variable));
                    }

                    self.name_expressions.visiting(path);
                    let before = self.replace_in_lambda(false);
                    let m = self.infer(self.name_expressions.get(path))?;
                    self.replace_in_lambda(before);
                    self.name_expressions.leaving(path);

                    let t = MonoType::Variable(variable);
                    if let Err((t1, t2)) = self.unify(&m, &t) {
                        return self.error(
                            TypeCheckError::TypeMismatch { t1, t2 },
                            self.name_expressions.get(path).span(),
                        );
                    };

                    let t = m.generalize();
                    self.names.insert(path, t.clone());
                    t
                } else {
                    self.names[path].clone()
                }
            }
        };

        Ok(self.instantiate(t))
    }

    fn application(
        &mut self,
        application: &'ast expression::Application<Resolved>,
        span: Span,
    ) -> Result<MonoType> {
        let return_type = self.newvar();
        let function = self.infer(application.function())?;
        let argument = self.infer(application.argument())?;
        let arrow = ArrowType::new(argument.clone(), return_type.clone());
        let arrow = MonoType::Arrow(arrow);

        if let Err((t1, t2)) = self.unify(&function, &arrow) {
            self.error(TypeCheckError::TypeMismatch { t1, t2 }, span)
        } else {
            Ok(self.substitute(return_type))
        }
    }

    fn lambda(&mut self, lambda: &'ast expression::Lambda<Resolved>) -> Result<MonoType> {
        let argument = self.newvar();

        self.stack.push_frame(lambda.captures().to_vec());
        self.stack.push_local(Type::Mono(argument.clone()));
        let before = self.replace_in_lambda(true);
        let return_type = self.infer(lambda.expression())?;
        self.replace_in_lambda(before);
        self.stack.pop_local();
        self.stack.pop_frame();

        let arrow = ArrowType::new(self.substitute(argument), self.substitute(return_type));

        Ok(MonoType::Arrow(arrow))
    }

    fn letin(&mut self, letin: &'ast expression::LetIn<Resolved>) -> Result<MonoType> {
        let variable_type = self.infer(letin.variable_expression())?;
        let variable_type = variable_type.generalize();

        self.stack.push_local(variable_type);
        let return_type = self.infer(letin.return_expression())?;
        self.stack.pop_local();

        Ok(self.substitute(return_type))
    }

    fn matchlet(&mut self, matchlet: &'ast expression::MatchAs<Resolved>) -> Result<MonoType> {
        let t = self.infer(matchlet.expression())?;
        let return_type = self.newvar();

        for branch in matchlet.branches() {
            let t = self.match_branch(&t, branch.data())?;

            if let Err((t1, t2)) = self.unify(&return_type, &t) {
                return self.error(TypeCheckError::TypeMismatch { t1, t2 }, branch.span());
            }
        }

        Ok(self.substitute(return_type))
    }

    fn match_branch(
        &mut self,
        t: &MonoType,
        branch: &'ast expression::matchas::Branch<Resolved>,
    ) -> Result<MonoType> {
        let len = self.stack.len();
        self.match_pattern_and_define_locals(t, branch.pattern())?;
        let m = self.infer(branch.expression())?;
        self.stack.truncate(len);

        Ok(m)
    }

    fn match_pattern_and_define_locals(
        &mut self,
        t: &MonoType,
        pattern: &Located<Pattern<Resolved>>,
    ) -> Result<()> {
        match pattern.data() {
            Pattern::Any(_) => {
                self.stack.push_local(Type::Mono(t.clone()));
            }
            Pattern::String(_) => {
                if let Err((t1, t2)) = self.unify(t, &MonoType::String) {
                    return self.error(TypeCheckError::TypeMismatch { t1, t2 }, pattern.span());
                };
            }
            Pattern::Structure(structure) => {
                let structure_type = self.instantiate(self.types[structure.type_path()].clone());

                if let Err((t1, t2)) = self.unify(t, &structure_type) {
                    return self.error(TypeCheckError::TypeMismatch { t1, t2 }, pattern.span());
                };

                let constructor_path = structure.type_path().append([structure.constructor_name()]);
                let constructor_type = self.names[&constructor_path].clone();

                let variables = structure_type.variables();
                let arguments = variables.iter().copied().map(MonoType::Variable);

                let m = match constructor_type {
                    Type::Mono(m) => m,
                    Type::Poly(variables, m) => {
                        let table = variables.into_iter().zip(arguments).collect();
                        m.substitute(&table)
                    }
                };

                if let MonoType::Arrow(constructor) = m {
                    let mut t = &constructor;
                    for argument in structure.arguments() {
                        self.match_pattern_and_define_locals(t.from(), argument)?;
                        if let MonoType::Arrow(arrow) = t.to() {
                            t = arrow;
                        }
                    }
                } else {
                    // NOTE: Type constructors are either functions or constants
                    assert!(matches!(m, MonoType::Structure(_)));
                }
            }
        }

        Ok(())
    }

    pub fn program(
        &mut self,
        program: &'ast Program<Resolved>,
        interner: &Interner,
    ) -> Result<Type> {
        for module in program.modules() {
            self.collect_definitions(module)?;
        }

        for module in program.modules() {
            self.module(module)?;
        }

        let parts = ["Main", "main"]
            .iter()
            .map(|s| interner.intern_id(s))
            .collect::<Vec<_>>();

        Ok(self.names[&Path::from_parts(parts)].clone())
    }

    pub fn module(&mut self, module: &'ast Module<Resolved>) -> Result<()> {
        self.current_source_name = Some(module.source_name());

        for definition in module.definitions() {
            if let Definition::Name(name) = definition {
                self.let_definition(name)?
            }
        }

        Ok(())
    }

    fn collect_definitions(&mut self, module: &'ast Module<Resolved>) -> Result<()> {
        for definition in module.definitions() {
            match definition {
                Definition::Name(name) => {
                    let newvar = self.newvar();
                    self.names.insert(name.path(), Type::Mono(newvar));
                    self.name_expressions.add(name.path(), name.expression());
                }
                Definition::Structure(structure) => {
                    let t = self.initialize_structure_type(
                        structure.path().clone(),
                        structure.variables().len(),
                    );

                    self.types.insert(structure.path(), t);
                }
                _ => (),
            }
        }

        // Type constructors
        for definition in module.definitions() {
            if let Definition::Structure(structure) = definition {
                let structure_type = self.instantiate(self.types[structure.path()].clone());
                let variables = structure_type.variables();

                self.type_variables
                    .extend(variables.iter().copied().map(MonoType::Variable));

                for constructor in structure.constructors() {
                    let mut t = structure_type.clone();
                    for argument in constructor.data().arguments().iter().rev() {
                        let argument = self.evaluate_type_expression(argument)?;
                        t = MonoType::Arrow(ArrowType::new(argument, t));
                    }

                    let t = Type::Poly(variables.clone(), t);
                    self.names.insert(constructor.data().path(), t);
                }

                self.type_variables.clear();
            }
        }

        Ok(())
    }

    fn let_definition(&mut self, name_definition: &'ast definition::Name<Resolved>) -> Result<()> {
        self.name_expressions.visiting(name_definition.path());
        let m = self.infer(name_definition.expression())?;

        let t = self.instantiate(self.names[name_definition.path()].clone());
        if let Err((t1, t2)) = self.unify(&m, &t) {
            return self.error(
                TypeCheckError::TypeMismatch { t1, t2 },
                name_definition.identifier().span(),
            );
        };

        let t = m.generalize();
        self.names.insert(name_definition.path(), t);
        self.name_expressions.leaving(name_definition.path());

        Ok(())
    }

    fn error<T>(&self, error: TypeCheckError, span: Span) -> Result<T> {
        Err(ReportableError::new(
            error,
            span,
            self.current_source_name.unwrap().to_string(),
        ))
    }
}

pub struct ExpressionMap<'ast> {
    map: HashMap<&'ast Path, (&'ast Located<Expression<Resolved>>, bool)>,
}

impl<'ast> ExpressionMap<'ast> {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
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
    TypeMismatch { t1: MonoType, t2: MonoType },
    CyclicDefinition(Path),
    ExpectedStructure(MonoType),
    TypeArityMismatch { expected: usize, found: usize },
}
