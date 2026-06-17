pub mod bound;
pub mod frame;
pub mod renamer;

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::{
    compilation::anf::{self, Local},
    error::{ReportableError, Result},
    interner::{InternId, Interner},
    location::{Located, Span},
    metadata::{self, Metadata, Setter, Unresolved},
    parse::{
        definition::{self, Definition, Module, Program},
        expression::{self, Expression},
        pattern::{self, Pattern},
        type_expression::{self, TypeExpression},
    },
    resolution::renamer::Renamed,
};

use bound::{Bound, BoundId, Module as ModuleBound, Path};
use frame::ResolutionStack;

/// Proof of resolution step
///   Can only be constructed from here
pub struct Resolved(());

/// AST Name Resolver
pub struct Resolver {
    /// Stack for local variables
    stack: ResolutionStack<InternId>,
    /// Stack for type parameters in structure definitions
    type_variables: Vec<InternId>,
    /// All of the names defined across the program
    names: HashSet<Path>,
    /// All of the types defined across the program
    types: HashSet<Path>,
    /// Modules of the program and their bound information
    modules: HashMap<Path, ModuleBound>,
    /// Path of the current module
    current_module_path: Option<Path>,
    /// Metadata
    metadata: Metadata<Unresolved>,
}

impl Resolver {
    pub fn new(metadata: Metadata<Unresolved>) -> Self {
        Resolver {
            stack: ResolutionStack::new(),
            type_variables: Vec::new(),
            names: HashSet::new(),
            types: HashSet::new(),
            modules: HashMap::new(),
            current_module_path: None,
            metadata,
        }
    }

    pub fn interactive_environment(mut self, interner: &mut Interner) -> Self {
        let interactive_id = interner.intern(String::from("interactive"));
        let path = Path::from_parts([interactive_id]);
        let module = ModuleBound::empty("<interactive>".to_string());

        self.modules.insert(path.clone(), module);
        self.current_module_path = Some(path);

        self
    }

    fn current_module(&self) -> &ModuleBound {
        &self.modules[self.current_module_path.as_ref().unwrap()]
    }

    fn current_module_mut(&mut self) -> &mut ModuleBound {
        self.modules
            .get_mut(self.current_module_path.as_ref().unwrap())
            .unwrap()
    }

    fn append_current_path(&self, identifier: InternId) -> Path {
        self.current_module_path
            .as_ref()
            .unwrap()
            .append([identifier])
    }

    pub fn expression(&mut self, expression: &Located<Expression>) -> Result<()> {
        let span = expression.span;

        match &expression.data {
            Expression::String(_) => (),
            Expression::Path(path) => self.path(path, span)?,
            Expression::Lambda(lambda) => self.lambda(lambda)?,
            Expression::Application(application) => self.application(application)?,
            Expression::LetIn(letin) => self.letin(letin)?,
            Expression::MatchAs(matchas) => self.matchas(matchas)?,
        }

        Ok(())
    }

    fn absolute_path(&self, base: &InternId) -> Path {
        if let Some(import_path) = self.current_module().imports().get(base) {
            import_path.data.clone()
        } else {
            self.append_current_path(*base)
        }
    }

    fn identifier(&mut self, identifier: InternId, span: Span) -> Result<Bound> {
        match self.stack.locally_resolve(identifier) {
            Some(bound) => Ok(bound),
            None => {
                let path = self.absolute_path(&identifier);
                if !self.names.contains(&path) {
                    self.error(
                        ResolutionError::UnboundPath(Path::from_parts(vec![identifier])),
                        Some(span),
                    )
                } else {
                    Ok(Bound::Absolute(path))
                }
            }
        }
    }

    fn path(&mut self, path: &expression::Path, span: Span) -> Result<()> {
        let bound = match path.parts.data.as_slice() {
            [] => unreachable!(),
            [identifier] => self.identifier(*identifier, span)?,
            [base, rest @ ..] => {
                let mut path = self.absolute_path(base);
                path.push(rest);

                let true = self.names.contains(&path) else {
                    return self.error(ResolutionError::UnboundPath(path), Some(span));
                };

                Bound::Absolute(path)
            }
        };

        self.metadata.set(path.bound_id, bound);

        Ok(())
    }

    fn lambda(&mut self, lambda: &expression::Lambda) -> Result<()> {
        self.stack.push_frame();
        self.stack.push_local(lambda.variable.data);
        self.expression(&lambda.expression)?;
        self.stack.pop_local();
        let capture = self.stack.pop_frame();

        self.metadata.set(lambda.capture_id, capture);

        Ok(())
    }

    fn application(&mut self, application: &expression::Application) -> Result<()> {
        self.expression(&application.function)?;
        self.expression(&application.argument)?;

        Ok(())
    }

    fn letin(&mut self, letin: &expression::LetIn) -> Result<()> {
        self.expression(&letin.variable_expression)?;
        self.stack.push_local(letin.variable.data);
        self.expression(&letin.return_expression)?;
        self.stack.pop_local();

        Ok(())
    }

    fn matchas(&mut self, matchas: &expression::MatchAs) -> Result<()> {
        self.expression(&matchas.expression)?;

        for branch in &matchas.branches {
            self.match_branch(&branch.data)?;
        }

        Ok(())
    }

    fn match_branch(&mut self, branch: &expression::Branch) -> Result<()> {
        let span = branch.pattern.span;

        let len = self.stack.len();
        self.pattern_define_locals(&branch.pattern.data, span)?;
        self.expression(&branch.expression)?;
        self.stack.truncate(len);

        Ok(())
    }

    fn pattern_define_locals(&mut self, pattern: &Pattern, span: Span) -> Result<()> {
        match pattern {
            Pattern::Any(any) => self.any_pattern(any)?,
            Pattern::String(_) => (),
            Pattern::Structure(structure) => self.structure_pattern(structure, span)?,
        };

        Ok(())
    }

    fn any_pattern(&mut self, any: &pattern::Any) -> Result<()> {
        self.stack.push_local(any.identifier);

        Ok(())
    }

    fn structure_pattern(&mut self, structure: &pattern::Structure, span: Span) -> Result<()> {
        for argument in &structure.arguments {
            self.pattern_define_locals(&argument.data, span)?;
        }

        let (type_path, constructor_name, tag) = match &structure.parts.data.as_slice() {
            [] => unreachable!(),
            [base, rest @ ..] => {
                let mut path = self.absolute_path(base);
                path.push(rest);

                let true = self.names.contains(&path) else {
                    return self.error(ResolutionError::UnboundPath(path), Some(span));
                };

                let constructor_name = path.pop();
                let type_name = path.pop();

                let constructors = &self.modules[&path].types()[&type_name];
                let tag = constructors
                    .iter()
                    .position(|cs| cs == &constructor_name)
                    .unwrap();

                path.push([type_name]);
                (path, constructor_name, tag)
            }
        };

        let structure_pattern = metadata::StructurePattern::new(type_path, constructor_name, tag);
        self.metadata
            .set(structure.structure_pattern_id, structure_pattern);

        Ok(())
    }

    fn type_expression(&mut self, expression: &Located<TypeExpression>) -> Result<()> {
        let span = expression.span;

        match &expression.data {
            TypeExpression::Path(path) => self.type_path(path, span)?,
            TypeExpression::Application(application) => self.type_application(application)?,
        };

        Ok(())
    }

    fn type_identifier(&self, identifier: &InternId, span: Span) -> Result<Bound> {
        let mut id = None;
        for (index, intern_id) in self.type_variables.iter().rev().enumerate() {
            if identifier == intern_id {
                id = Some(BoundId::new(self.type_variables.len() - 1 - index));
                break;
            }
        }

        match id {
            Some(id) => Ok(Bound::Local(id)),
            None => {
                let path = self.absolute_path(identifier);
                if !self.types.contains(&path) {
                    self.error(
                        ResolutionError::UnboundPath(Path::from_parts(vec![*identifier])),
                        Some(span),
                    )
                } else {
                    Ok(Bound::Absolute(path))
                }
            }
        }
    }

    fn type_path(&mut self, path: &type_expression::Path, span: Span) -> Result<()> {
        let bound = match &path.parts.data.as_slice() {
            [] => unreachable!(),
            [identifier] => self.type_identifier(identifier, span)?,
            [base, rest @ ..] => {
                let mut path = self.absolute_path(base);
                path.push(rest);

                let true = self.types.contains(&path) else {
                    return self.error(ResolutionError::UnboundPath(path), Some(span));
                };

                Bound::Absolute(path)
            }
        };

        self.metadata.set(path.bound_id, bound);

        Ok(())
    }

    fn type_application(&mut self, application: &type_expression::Application) -> Result<()> {
        self.type_expression(&application.function)?;

        for argument in &application.arguments {
            self.type_expression(argument)?;
        }

        Ok(())
    }

    pub fn program(&mut self, program: &Program) -> Result<()> {
        let module_paths = program
            .modules
            .iter()
            .map(|module| {
                let path = self.find_module_name(module)?;
                self.current_module_path = Some(path);
                self.collect_names(module)?;
                Ok(self.current_module_path.take().unwrap())
            })
            .collect::<Result<Vec<_>>>()?;

        self.check_if_imports_exist()?;

        for (module, module_path) in program.modules.iter().zip(module_paths) {
            self.current_module_path = Some(module_path);
            self.module(module)?;
        }

        Ok(())
    }

    pub fn module(&mut self, module: &Module) -> Result<()> {
        for definition in &module.definitions {
            self.definition(definition)?;
        }

        Ok(())
    }

    fn find_module_name(&mut self, module: &Module) -> Result<Path> {
        for definition in &module.definitions {
            if let Definition::ModulePath(module_path) = definition {
                let module_path = Path::from_parts(module_path.parts.data.to_vec());
                self.modules.insert(
                    module_path.clone(),
                    ModuleBound::empty(module.source_name.to_string()),
                );

                return Ok(module_path);
            }
        }

        // TODO: Check for duplicate module definition
        let error = ReportableError {
            error: ResolutionError::MissingModuleDefinition.into(),
            source_name: module.source_name.to_string(),
            span: None,
        };

        Err(Box::new(error))
    }

    fn collect_names(&mut self, module: &Module) -> Result<()> {
        for definition in &module.definitions {
            if let Definition::Name(name) = definition {
                // TODO: Check for duplicate definitions
                self.current_module_mut()
                    .names_mut()
                    .insert(name.identifier.data);

                self.names
                    .insert(self.append_current_path(name.identifier.data));
            }

            if let Definition::Structure(structure) = definition {
                let structure_path = self.append_current_path(structure.name.data);

                for constructor in &structure.constructors {
                    let name = constructor.data.name.data;
                    self.names.insert(structure_path.append([name]));
                }

                self.types.insert(structure_path);

                // TODO: Check for duplicate definitions
                let constructors = structure
                    .constructors
                    .iter()
                    .map(|constructor| constructor.data.name.data)
                    .collect();

                self.current_module_mut()
                    .types_mut()
                    .insert(structure.name.data, constructors);
            }

            if let Definition::Import(import) = definition {
                let base = Path::from_parts(import.module_path.data.to_vec());

                match &import.subimport {
                    Some(import_name) => {
                        self.collect_subimport(import_name, &base)?;
                    }
                    None => {
                        self.current_module_mut().imports_mut().insert(
                            *import.module_path.data.last().unwrap(),
                            Located {
                                data: base,
                                span: import.module_path.span,
                            },
                        );
                    }
                }
            }
        }

        Ok(())
    }

    fn collect_subimport(
        &mut self,
        import_name: &definition::Subimport,
        base: &Path,
    ) -> Result<()> {
        match import_name {
            definition::Subimport::As(as_name) => {
                self.current_module_mut().imports_mut().insert(
                    as_name.data,
                    Located {
                        data: base.clone(),
                        span: as_name.span,
                    },
                );
            }
            definition::Subimport::Import(imports) => {
                for import in imports {
                    let base = base.append(&import.module_path.data);

                    match &import.subimport {
                        Some(subimport) => {
                            self.collect_subimport(subimport, &base)?;
                        }
                        None => {
                            self.current_module_mut().imports_mut().insert(
                                *import.module_path.data.last().unwrap(),
                                Located {
                                    data: base,
                                    span: import.module_path.span,
                                },
                            );
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn check_if_imports_exist(&self) -> Result<()> {
        for module in self.modules.values() {
            for import in module.imports().values() {
                if !(self.names.contains(&import.data)
                    || self.types.contains(&import.data)
                    || self.modules.contains_key(&import.data))
                {
                    let error = ReportableError {
                        error: ResolutionError::UnresolvedImport(import.data.clone()).into(),
                        source_name: module.source_name().to_string(),
                        span: Some(import.span),
                    };

                    return Err(Box::new(error));
                }
            }
        }

        Ok(())
    }

    fn definition(&mut self, definiton: &Definition) -> Result<()> {
        match definiton {
            Definition::ModulePath(_) => (),
            Definition::Name(name) => self.name_definition(name)?,
            Definition::Import(_) => (),
            Definition::Structure(structure) => self.structure_definition(structure)?,
        };

        Ok(())
    }

    fn name_definition(&mut self, name_definition: &definition::Name) -> Result<()> {
        self.expression(&name_definition.expression)?;
        let path = self.append_current_path(name_definition.identifier.data);

        self.metadata.set(name_definition.path_id, path);

        Ok(())
    }

    fn structure_definition(&mut self, structure_definition: &definition::Structure) -> Result<()> {
        let path = self.append_current_path(structure_definition.name.data);

        self.type_variables
            .extend(structure_definition.variables.iter().map(|v| v.data));

        for constructor in &structure_definition.constructors {
            self.constructor(&constructor.data, &path)?;
        }

        self.type_variables.clear();

        self.metadata.set(structure_definition.path_id, path);

        Ok(())
    }

    fn constructor(
        &mut self,
        constructor: &definition::Constructor,
        type_path: &Path,
    ) -> Result<()> {
        for argument in &constructor.arguments {
            self.type_expression(argument)?;
        }

        let path = type_path.append([constructor.name.data]);

        self.metadata.set(constructor.path_id, path);

        Ok(())
    }

    fn error<T>(&self, error: ResolutionError, span: Option<Span>) -> Result<T> {
        Err(Box::new(ReportableError {
            error: error.into(),
            source_name: self.current_module().source_name().to_string(),
            span,
        }))
    }

    pub fn finish(self) -> Metadata<Resolved> {
        self.metadata.transition(Resolved(()))
    }
}

/// Proof of ANF resolution
///   Can only be constructed from here
pub struct ANFResolved(());

/// ANF Name Resolver
/// ANF only affects local variables so ANFResolver only
/// re-resolves local variables
pub struct ANFResolver {
    /// Stack for local variables
    stack: ResolutionStack<anf::Local>,
    /// Metadata
    metadata: Metadata<Renamed>,
}

impl ANFResolver {
    pub fn new(metadata: Metadata<Renamed>) -> Self {
        ANFResolver {
            stack: ResolutionStack::new(),
            metadata,
        }
    }

    pub fn expression(&mut self, anf: &anf::Expression) {
        match anf {
            anf::Expression::LetIn(letin) => self.letin(letin),
            anf::Expression::Application(application) => self.application(application),
            anf::Expression::MatchAs(matchlet) => self.matchas(matchlet),
            anf::Expression::Join(join) => self.join(join),
            anf::Expression::Jump(jump) => self.jump(jump),
            anf::Expression::Atom(atom) => self.atom(atom),
        }
    }

    fn atom(&mut self, atom: &anf::Atom) {
        match atom {
            anf::Atom::String(_) => (),
            anf::Atom::Path(path) => self.path(path),
            anf::Atom::Lambda(lambda) => self.lambda(lambda),
        }
    }

    fn path(&mut self, path: &anf::atom::Path) {
        let bound = if let Some(bound) = &path.bound {
            bound.clone()
        } else {
            match &path.path {
                anf::Path::ANFLocal(id) => self.identifier(Local::ANFLocal(*id)),
                anf::Path::Local(local) => self.identifier(Local::Standard(*local)),
                anf::Path::Absolute(_) => unreachable!(),
            }
        };

        self.metadata.set(path.anf_bound_id, bound);
    }

    fn identifier(&mut self, identifier: Local) -> Bound {
        self.stack.locally_resolve(identifier).unwrap()
    }

    fn lambda(&mut self, lambda: &anf::atom::Lambda) {
        self.stack.push_frame();
        self.stack.push_local(Local::Standard(lambda.variable));
        self.expression(&lambda.expression);
        self.stack.pop_local();
        let capture = self.stack.pop_frame();

        self.metadata.set(lambda.anf_capture_id, capture);
    }

    fn letin(&mut self, letin: &anf::expression::LetIn) {
        self.atom(&letin.variable_expression);
        self.stack.push_local(Local::Standard(letin.variable));
        self.expression(&letin.return_expression);
        self.stack.pop_local();
    }

    fn application(&mut self, application: &anf::expression::Application) {
        self.atom(&application.function);
        self.atom(&application.argument);
        self.stack.push_local(application.variable);
        self.expression(&application.expression);
        self.stack.pop_local();
    }

    fn matchas(&mut self, matchas: &anf::expression::MatchAs) {
        self.atom(&matchas.expression);

        for branch in &matchas.branches {
            self.branch(branch);
        }
    }

    fn branch(&mut self, branch: &anf::expression::Branch) {
        let len = self.stack.len();
        self.define_pattern_locals(&branch.pattern);
        self.expression(&branch.expression);
        self.stack.truncate(len);
    }

    fn define_pattern_locals(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Any(any) => {
                let unique_name = &self.metadata[any.unique_name_id];
                self.stack.push_local(Local::Standard(unique_name.unwrap()));
            }
            pattern::Pattern::String(_) => (),
            pattern::Pattern::Structure(structure) => {
                for argument in &structure.arguments {
                    self.define_pattern_locals(&argument.data);
                }
            }
        }
    }

    fn join(&mut self, join: &anf::expression::Join) {
        self.expression(&join.join);
        self.stack.push_local(join.variable);
        self.expression(&join.expression);
        self.stack.pop_local();
    }

    fn jump(&mut self, jump: &anf::expression::Jump) {
        self.atom(&jump.expression);
    }

    pub fn program(&mut self, program: &anf::Program) {
        for module in program.modules() {
            self.module(module);
        }
    }

    pub fn module(&mut self, module: &anf::Module) {
        for definition in module.definitions() {
            self.definition(definition);
        }
    }

    fn definition(&mut self, definition: &anf::Definition) {
        match definition {
            anf::Definition::Name(name) => self.name_definition(name),
            anf::Definition::Structure(_) => (),
        }
    }

    fn name_definition(&mut self, name_definition: &anf::definition::Name) {
        self.expression(&name_definition.expression);
    }

    pub fn finish(self) -> Metadata<ANFResolved> {
        self.metadata.transition(ANFResolved(()))
    }
}

#[derive(Debug, Clone)]
pub enum ResolutionError {
    UnboundPath(Path),
    MissingModuleDefinition,
    UnresolvedImport(Path),
}
