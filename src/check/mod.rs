pub mod typ;

use std::{collections::HashMap, result};

use crate::{
    error::{ReportableError, Result},
    interner::Interner,
    location::{Located, Span},
    metadata::Metadata,
    parse::{
        definition::{self, Definition, Module, Program},
        expression::{self, Expression},
        pattern::Pattern,
        type_expression::{self, TypeExpression},
    },
    resolution::{
        Resolved,
        bound::{Bound, Path},
        frame::CheckStack,
    },
};

use typ::{ArrowType, MonoType, StructureType, Type};

pub struct TypeChecker<'ast, 'metadata>
where
    'ast: 'metadata,
{
    /// Stack for local variables
    stack: CheckStack<Type>,
    /// Stack for type variables in structure definitions
    type_variables: Vec<MonoType>,
    /// True if currently type checking a lambda expression.
    ///     Its purpose is to check for cyclic definitions
    in_lambda: bool,
    /// Map for accesing expressions of let definitions and keeping track of cylic definitions
    name_expressions: ExpressionMap<'metadata, 'ast>,
    /// Map from path of the let definition to its type
    names: HashMap<&'metadata Path, Type>,
    /// Map from path of the structure definition to its corresponding type
    types: HashMap<&'metadata Path, Type>,
    /// Map from structure types to their constructors paths' and
    ///     constructurs argument counts'
    constructors: HashMap<&'metadata Path, Vec<(&'metadata Path, usize)>>,
    /// Counter for generating fresh type variables
    newvar_counter: usize,
    /// Map from type variable id (usize) to its substitution (MonoType)
    unification_table: HashMap<usize, MonoType>,
    /// Source file name of the current module for error reporting
    current_source_name: Option<&'metadata str>,
    /// Metadata
    metadata: &'metadata Metadata<Resolved>,
}

impl<'ast, 'metadata> TypeChecker<'metadata, 'ast>
where
    'ast: 'metadata,
{
    pub fn new(metadata: &'metadata Metadata<Resolved>) -> Self {
        Self {
            stack: CheckStack::new(),
            type_variables: Vec::new(),
            in_lambda: false,
            name_expressions: ExpressionMap::new(),
            names: HashMap::new(),
            types: HashMap::new(),
            constructors: HashMap::new(),
            newvar_counter: 0,
            unification_table: HashMap::default(),
            current_source_name: None,
            metadata,
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
            Type::Mono(MonoType::Structure(StructureType {
                path,
                arguments: vec![],
            }))
        } else {
            let arguments = (0..argument_count).map(|_| self.newvar()).collect();
            MonoType::Structure(StructureType { path, arguments }).generalize()
        }
    }

    fn evaluate_type_expression(
        &mut self,
        expression: &Located<TypeExpression>,
    ) -> Result<MonoType> {
        match &expression.data {
            TypeExpression::Path(path) => self.evaluate_type_path(path),
            TypeExpression::Application(application) => {
                self.evaluate_type_application(application, expression.span)
            }
        }
    }

    fn evaluate_type_path(&mut self, path: &type_expression::Path) -> Result<MonoType> {
        let bound = &self.metadata[path.bound_id];

        let t = match bound {
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
        application: &type_expression::Application,
        span: Span,
    ) -> Result<MonoType> {
        let m = self.evaluate_type_expression(&application.function)?;
        let MonoType::Structure(structure) = m else {
            return self.error(TypeCheckError::ExpectedStructure(m), span);
        };

        let expected = structure.arguments.len();
        let found = application.arguments.len();
        let true = found == expected else {
            return self.error(TypeCheckError::TypeArityMismatch { expected, found }, span);
        };

        let mut application_arguments = Vec::new();
        for argument in &application.arguments {
            application_arguments.push(self.evaluate_type_expression(argument)?);
        }

        let structure = StructureType {
            path: structure.path.clone(),
            arguments: application_arguments,
        };

        Ok(MonoType::Structure(structure))
    }

    pub fn infer(&mut self, expression: &Located<Expression>) -> Result<MonoType> {
        match &expression.data {
            Expression::String(_) => Ok(MonoType::String),
            Expression::Path(path) => self.path(path, expression.span),
            Expression::Application(application) => self.application(application, expression.span),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::LetIn(letin) => self.letin(letin),
            Expression::MatchAs(matchlet) => self.matchas(matchlet),
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
                self.unify(&arrow1.from, &arrow2.from)?;
                self.unify(&arrow1.to, &arrow2.to)
            }
            (MonoType::Structure(structure1), MonoType::Structure(structure2)) => {
                if structure1.path != structure2.path {
                    Err((self.substitute(t1.clone()), self.substitute(t2.clone())))
                } else {
                    let arguments = structure1.arguments.iter().zip(&structure2.arguments);
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

    fn path(&mut self, path: &expression::Path, span: Span) -> Result<MonoType> {
        let t = match &self.metadata[path.bound_id] {
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
                            self.name_expressions.get(path).span,
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
        application: &expression::Application,
        span: Span,
    ) -> Result<MonoType> {
        let return_type = self.newvar();
        let function = self.infer(&application.function)?;
        let argument = self.infer(&application.argument)?;
        let arrow = ArrowType {
            from: Box::new(argument.clone()),
            to: Box::new(return_type.clone()),
        };
        let arrow = MonoType::Arrow(arrow);

        if let Err((t1, t2)) = self.unify(&function, &arrow) {
            self.error(TypeCheckError::TypeMismatch { t1, t2 }, span)
        } else {
            Ok(self.substitute(return_type))
        }
    }

    fn lambda(&mut self, lambda: &expression::Lambda) -> Result<MonoType> {
        let argument = self.newvar();

        let captures = &self.metadata[lambda.capture_id];

        self.stack.push_frame(captures.to_vec());
        self.stack.push_local(Type::Mono(argument.clone()));
        let before = self.replace_in_lambda(true);
        let return_type = self.infer(&lambda.expression)?;
        self.replace_in_lambda(before);
        self.stack.pop_local();
        self.stack.pop_frame();

        let arrow = ArrowType {
            from: Box::new(self.substitute(argument)),
            to: Box::new(self.substitute(return_type)),
        };

        Ok(MonoType::Arrow(arrow))
    }

    fn letin(&mut self, letin: &expression::LetIn) -> Result<MonoType> {
        let variable_type = self.infer(&letin.variable_expression)?;
        let variable_type = variable_type.generalize();

        self.stack.push_local(variable_type);
        let return_type = self.infer(&letin.return_expression)?;
        self.stack.pop_local();

        Ok(self.substitute(return_type))
    }

    fn matchas(&mut self, matchas: &expression::MatchAs) -> Result<MonoType> {
        let t = self.infer(&matchas.expression)?;
        let return_type = self.newvar();

        for branch in &matchas.branches {
            let t = self.match_branch(&t, &branch.data)?;

            if let Err((t1, t2)) = self.unify(&return_type, &t) {
                return self.error(TypeCheckError::TypeMismatch { t1, t2 }, branch.span);
            }
        }

        let branches = matchas
            .branches
            .iter()
            .map(|branch| &branch.data.pattern)
            .collect();

        self.check_branch_exhaustiveness(&self.substitute(t), branches, matchas.expression.span)?;

        Ok(self.substitute(return_type))
    }

    fn check_branch_exhaustiveness(
        &self,
        t: &MonoType,
        patterns: Vec<&Located<Pattern>>,
        span: Span,
    ) -> Result<()> {
        if patterns
            .iter()
            .any(|pattern| matches!(pattern.data, Pattern::Any(_)))
        {
            return Ok(());
        }

        // Other than a structure type needs an `Any` pattern to be exhaustive
        let MonoType::Structure(structure) = &t else {
            return self.error(TypeCheckError::UnexhaustivePatternMatching, span);
        };

        let constructor_count = self.constructors[&structure.path].len();
        let mut constructor_branches = vec![vec![]; constructor_count];

        for pattern in patterns {
            let Pattern::Structure(pattern) = &pattern.data else {
                unreachable!("Type checking guarantees only-all structure patterns a this point");
            };

            let structure_pattern = &self.metadata[pattern.structure_pattern_id];
            constructor_branches[structure_pattern.tag].push(&pattern.arguments);
        }

        if constructor_branches
            .iter()
            .any(|constructor| constructor.is_empty())
        {
            return self.error(TypeCheckError::UnexhaustivePatternMatching, span);
        }

        for (tag, constructors) in constructor_branches.into_iter().enumerate() {
            let (path, _) = self.constructors[&structure.path][tag];
            let m = self.instantiate_structure_pattern(path, structure.arguments.iter().cloned());

            self.structure_argument_exhaustiveness(&m, HashMap::from([(tag, constructors)]), span)?;
        }

        Ok(())
    }

    fn structure_argument_exhaustiveness<A>(
        &self,
        m: &MonoType,
        branches: HashMap<usize, Vec<A>>,
        span: Span,
    ) -> Result<()>
    where
        A: AsRef<[Located<Pattern>]>,
    {
        let MonoType::Arrow(arrow) = m else {
            return Ok(());
        };

        for arguments in branches.values() {
            let first_arguments = arguments
                .iter()
                .map(|arguments| &arguments.as_ref()[0])
                .collect();

            self.check_branch_exhaustiveness(&arrow.from, first_arguments, span)?;

            let branches = arguments.iter().fold(HashMap::new(), |mut acc, arguments| {
                let branch = if let Pattern::Structure(structure) = &arguments.as_ref()[0].data {
                    // 0 is reserved for `Any` paths
                    self.metadata[structure.structure_pattern_id].tag + 1
                } else {
                    // NOTE: Other patterns cannot nest so this value is not
                    //  important for them except for `Any` pattern. An `Any` and
                    //  structure pattern can share the same spot unlike any other
                    //  two patterns and therefore needs to distinguished. So 0 is
                    //  reserved for `Any` paths
                    0
                };

                acc.entry(branch)
                    .or_insert(vec![])
                    .push(&arguments.as_ref()[1..]);
                acc
            });

            self.structure_argument_exhaustiveness(&arrow.to, branches, span)?;
        }

        Ok(())
    }

    fn match_branch(&mut self, t: &MonoType, branch: &expression::Branch) -> Result<MonoType> {
        let len = self.stack.len();
        self.match_pattern_and_define_locals(t, &branch.pattern)?;
        let m = self.infer(&branch.expression)?;
        self.stack.truncate(len);

        Ok(m)
    }

    fn match_pattern_and_define_locals(
        &mut self,
        t: &MonoType,
        pattern: &Located<Pattern>,
    ) -> Result<()> {
        match &pattern.data {
            Pattern::Any(_) => {
                self.stack.push_local(Type::Mono(t.clone()));
            }
            Pattern::String(_) => {
                if let Err((t1, t2)) = self.unify(t, &MonoType::String) {
                    return self.error(TypeCheckError::TypeMismatch { t1, t2 }, pattern.span);
                };
            }
            Pattern::Structure(structure) => {
                let structure_pattern = &self.metadata[structure.structure_pattern_id];

                let structure_type =
                    self.instantiate(self.types[&structure_pattern.type_path].clone());

                if let Err((t1, t2)) = self.unify(t, &structure_type) {
                    return self.error(TypeCheckError::TypeMismatch { t1, t2 }, pattern.span);
                };

                let (path, argument_count) =
                    self.constructors[&structure_pattern.type_path][structure_pattern.tag];

                let m = self.instantiate_structure_pattern(
                    path,
                    structure_type
                        .variables()
                        .iter()
                        .copied()
                        .map(MonoType::Variable),
                );

                if argument_count != structure.arguments.len() {
                    return self.error(
                        TypeCheckError::StructurePatternArityMismatch {
                            expected: argument_count,
                            found: structure.arguments.len(),
                        },
                        pattern.span,
                    );
                }

                if let MonoType::Arrow(constructor) = m {
                    let mut t = &constructor;
                    for argument in &structure.arguments {
                        self.match_pattern_and_define_locals(&t.from, argument)?;
                        if let MonoType::Arrow(arrow) = t.to.as_ref() {
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

    fn instantiate_structure_pattern<I>(&self, constructor_path: &Path, arguments: I) -> MonoType
    where
        I: Iterator<Item = MonoType>,
    {
        let constructor_type = self.names[constructor_path].clone();

        match constructor_type {
            Type::Mono(m) => m,
            Type::Poly(variables, m) => {
                let table = variables.into_iter().zip(arguments).collect();
                m.substitute(&table)
            }
        }
    }

    pub fn program(&mut self, program: &'ast Program, interner: &Interner) -> Result<Type> {
        for module in &program.modules {
            self.collect_definitions(module)?;
        }

        for module in &program.modules {
            self.module(module)?;
        }

        let parts = ["Main", "main"]
            .iter()
            .map(|s| interner.intern_id(s))
            .collect::<Vec<_>>();

        Ok(self.names[&Path::from_parts(parts)].clone())
    }

    pub fn module(&mut self, module: &'ast Module) -> Result<()> {
        self.current_source_name = Some(&module.source_name);

        for definition in &module.definitions {
            if let Definition::Name(name) = definition {
                self.name_definition(name)?
            }
        }

        Ok(())
    }

    fn collect_definitions(&mut self, module: &'ast Module) -> Result<()> {
        for definition in &module.definitions {
            match definition {
                Definition::Name(name) => {
                    let path = &self.metadata[name.path_id];

                    let newvar = self.newvar();
                    self.names.insert(path, Type::Mono(newvar));
                    self.name_expressions.add(path, &name.expression);
                }
                Definition::Structure(structure) => {
                    let path = &self.metadata[structure.path_id];

                    let t = self.initialize_structure_type(path.clone(), structure.variables.len());

                    self.types.insert(path, t);

                    let constructors = structure
                        .constructors
                        .iter()
                        .map(|constructor| {
                            (
                                &self.metadata[constructor.data.path_id],
                                constructor.data.arguments.len(),
                            )
                        })
                        .collect();

                    self.constructors.insert(path, constructors);
                }
                _ => (),
            }
        }

        // Type constructors
        for definition in &module.definitions {
            if let Definition::Structure(structure) = definition {
                let path = &self.metadata[structure.path_id];

                let structure_type = self.instantiate(self.types[path].clone());
                let variables = structure_type.variables();

                self.type_variables
                    .extend(variables.iter().copied().map(MonoType::Variable));

                for constructor in &structure.constructors {
                    let mut t = structure_type.clone();
                    for argument in constructor.data.arguments.iter().rev() {
                        let argument = self.evaluate_type_expression(argument)?;
                        let arrow = ArrowType {
                            from: Box::new(argument),
                            to: Box::new(t),
                        };

                        t = MonoType::Arrow(arrow);
                    }

                    let path = &self.metadata[constructor.data.path_id];

                    let t = Type::Poly(variables.clone(), t);
                    self.names.insert(path, t);
                }

                self.type_variables.clear();
            }
        }

        Ok(())
    }

    fn name_definition(&mut self, name_definition: &definition::Name) -> Result<()> {
        let path = &self.metadata[name_definition.path_id];

        self.name_expressions.visiting(path);
        let m = self.infer(&name_definition.expression)?;

        let t = self.instantiate(self.names[path].clone());
        if let Err((t1, t2)) = self.unify(&m, &t) {
            return self.error(
                TypeCheckError::TypeMismatch { t1, t2 },
                name_definition.identifier.span,
            );
        };

        let t = m.generalize();
        self.names.insert(path, t);
        self.name_expressions.leaving(path);

        Ok(())
    }

    fn error<T>(&self, error: TypeCheckError, span: Span) -> Result<T> {
        Err(Box::new(ReportableError {
            error: error.into(),
            source_name: self.current_source_name.unwrap().to_string(),
            span: Some(span),
        }))
    }
}

pub struct ExpressionMap<'path, 'ast>
where
    'ast: 'path,
{
    map: HashMap<&'path Path, (&'ast Located<Expression>, bool)>,
}

impl<'path, 'ast> ExpressionMap<'path, 'ast> {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn add(&mut self, path: &'ast Path, expression: &'ast Located<Expression>) {
        self.map.insert(path, (expression, false));
    }

    fn get(&self, path: &Path) -> &'ast Located<Expression> {
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
    UnexhaustivePatternMatching,
    StructurePatternArityMismatch { expected: usize, found: usize },
}
