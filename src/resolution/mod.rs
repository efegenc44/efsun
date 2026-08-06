pub mod bound;
pub mod frame;
pub mod renamer;

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::{
    compilation::anf::{self, Local},
    data_table::{
        ANFBoundDataId, ANFCaptureDataId, BoundDataId, CaptureDataId, DataTable,
        OptionalDataTable, PathDataId, SelfCaptureDataId, StructurePatternDataId,
        TailCallDataId,
    },
    error::{ReportableError, Result},
    interner::{InternId, Interner},
    location::{Located, Span},
    parse::{
        definition::{self, Definition, Module, Program},
        expression::{self, Expression},
        pattern::{self, Pattern},
        type_expression::{self, TypeExpression},
    },
    resolution::{bound::Capture, renamer::RenameData},
};

use bound::{Bound, BoundId, Module as ModuleBound, Path};
use frame::ResolutionStack;

/// Indicates which namespace to lookup for a name
///   `Scope` is first look up `Type` then `Module` and
///   the name is meant to indicate an opening of a new
///   _namespace scope_.
enum Namespace {
    Name,
    Type,
    Module,
    Scope,
}

/// State for whether an expression can reference the let in
///   expression's variable. It is `Allowed` when the right side
///   of the let in is a lambda and `Rejected` if it's different.
///   When not resolving a variable expression of a let in,
///   it is `Irrelevant`.
#[derive(Copy, Clone, Debug)]
enum LetInVariableReference {
    Allowed,
    Rejected,
    Irrelevant,
}

pub type Graph<N> = HashMap<N, Vec<(N, bool)>>;
pub type Edge<N> = (N, N);

/// AST Name Resolver
pub struct Resolver {
    /// Stack for local variables
    stack: ResolutionStack<InternId>,
    /// Stack for type parameters in structure definitions
    type_variables: Vec<InternId>,
    /// All of the names defined across the program
    names: HashSet<Path>,
    /// All of the types defined across the program
    types: HashMap<Path, HashSet<InternId>>,
    /// Modules of the program and their bound information
    modules: HashMap<Path, ModuleBound>,
    /// Path of the current module
    current_module_path: Option<Path>,
    /// Current node in the dependency graph
    current_name_definition: Option<Path>,
    /// True if currently resolving a lambda expression.
    ///     Its purpose is to check for cyclic definitions
    in_lambda: bool,
    /// State of allowance for referencing let in expression's variable
    ///   in variable expression of let in
    letin_reference: LetInVariableReference,
    /// Resoluiton produced data
    data: ResolutionData,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            stack: ResolutionStack::new(),
            type_variables: Vec::new(),
            names: HashSet::new(),
            types: HashMap::new(),
            modules: HashMap::new(),
            current_module_path: None,
            current_name_definition: None,
            in_lambda: false,
            letin_reference: LetInVariableReference::Irrelevant,
            data: ResolutionData::default(),
        }
    }

    pub fn set_interactive_module(mut self, interner: &mut Interner) -> Self {
        let interactive_id = interner.intern(String::from("interactive"));
        let path = Path::from_parts([interactive_id]);
        let module = ModuleBound::empty("<interactive>".to_string());

        self.modules.insert(path.clone(), module);
        self.current_module_path = Some(path);

        self
    }

    fn replace_in_lambda(&mut self, value: bool) -> bool {
        let before = self.in_lambda;
        self.in_lambda = value;
        before
    }

    fn replace_letin_reference(&mut self, value: LetInVariableReference) -> LetInVariableReference {
        let before = self.letin_reference;
        self.letin_reference = value;
        before
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

    fn add_dependency(&mut self, mut path: Path) {
        let (path, is_constructor) = {
            let posssible_constructor_name = path.pop();
            let is_constructor = self.types.contains_key(&path);
            path.push([posssible_constructor_name]);
            (path, is_constructor)
        };

        let current_name = self.current_name_definition.as_ref().unwrap();
        if &path != current_name && !is_constructor {
            self.data
                .dependencies
                .get_mut(current_name)
                .unwrap()
                .push((path.clone(), self.in_lambda));
        }
    }

    fn expression(&mut self, expression: &Located<Expression>) -> Result<()> {
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

    pub fn expression_repl(mut self, expression: &Located<Expression>) -> Result<ResolutionData> {
        self.expression(expression)?;

        Ok(self.data)
    }

    fn absolute_path(&self, base: &InternId, namespace: Namespace) -> Path {
        let module = self.current_module();

        let imports = match namespace {
            Namespace::Name => &module.name_imports,
            Namespace::Type => &module.type_imports,
            Namespace::Module => &module.module_imports,
            Namespace::Scope => {
                return if let Some(import_path) = module.type_imports.get(base) {
                    import_path.data.clone()
                } else if let Some(import_path) = module.module_imports.get(base) {
                    import_path.data.clone()
                } else {
                    self.append_current_path(*base)
                };
            }
        };

        if let Some(import_path) = imports.get(base) {
            import_path.data.clone()
        } else {
            self.append_current_path(*base)
        }
    }

    fn identifier(&mut self, identifier: InternId, span: Span) -> Result<Bound> {
        match self.stack.locally_resolve(identifier) {
            Some(bound) => {
                if let LetInVariableReference::Irrelevant = self.letin_reference {
                    return Ok(bound);
                }

                // NOTE: It is guaranteed that the let in variable sits on top of the frame.
                if let Bound::Local(id) = bound
                    && id.value() == self.stack.len() - 1
                    && let LetInVariableReference::Rejected = self.letin_reference
                {
                    return self.error(ResolutionError::RejectedLetInSelfReference, Some(span));
                }

                Ok(bound)
            }
            None => {
                let path = self.absolute_path(&identifier, Namespace::Name);
                if !self.names.contains(&path) {
                    self.error(
                        ResolutionError::UnboundPath(Path::from_parts(vec![identifier])),
                        Some(span),
                    )
                } else {
                    self.add_dependency(path.clone());
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
                let path = if rest.is_empty() {
                    self.absolute_path(base, Namespace::Name)
                } else {
                    let mut path = self.absolute_path(base, Namespace::Scope);
                    path.push(rest);
                    path
                };

                let true = self.names.contains(&path) else {
                    return self.error(ResolutionError::UnboundPath(path), Some(span));
                };

                self.add_dependency(path.clone());
                Bound::Absolute(path)
            }
        };

        self.data.bounds.set(path.bound_id, bound);

        Ok(())
    }

    fn lambda(&mut self, lambda: &expression::Lambda) -> Result<()> {
        self.resolve_tail_call(&lambda.expression.data);

        self.stack.push_frame();
        self.stack.push_local(lambda.variable.data);
        let before = self.replace_in_lambda(true);
        self.expression(&lambda.expression)?;
        self.replace_in_lambda(before);
        self.stack.pop_local();
        let capture = self.stack.pop_frame();

        if let LetInVariableReference::Allowed = self.letin_reference {
            for (index, capture) in capture.iter().enumerate() {
                // NOTE: It is guaranteed that the let in variable sits on top of the frame.
                if let Capture::Local(id) = capture
                    && id.value() == self.stack.len() - 1
                {
                    self.data.self_captures.set(lambda.self_capture_id, index);
                }
            }
        }

        self.data.captures.set(lambda.capture_id, capture);

        Ok(())
    }

    fn resolve_tail_call(&mut self, expression: &Expression) {
        match expression {
            Expression::Application(application) => {
                self.data.tail_calls.set(application.tail_call_id, ());
            }
            Expression::LetIn(letin) => self.resolve_tail_call(&letin.return_expression.data),
            Expression::MatchAs(matchas) => {
                for branch in &matchas.branches {
                    self.resolve_tail_call(&branch.data.expression.data);
                }
            }
            Expression::String(_) | Expression::Lambda(_) | Expression::Path(_) => (),
        }
    }

    fn application(&mut self, application: &expression::Application) -> Result<()> {
        self.expression(&application.function)?;
        self.expression(&application.argument)?;

        Ok(())
    }

    // TODO: Maybe make recursive let in expressions optional with a keyword like `rec`
    fn letin(&mut self, letin: &expression::LetIn) -> Result<()> {
        let allowance = if let Expression::Lambda(_) = &letin.variable_expression.data {
            LetInVariableReference::Allowed
        } else {
            LetInVariableReference::Rejected
        };

        let old = self.replace_letin_reference(allowance);
        self.stack.push_local(letin.variable.data);
        self.expression(&letin.variable_expression)?;
        self.replace_letin_reference(LetInVariableReference::Irrelevant);
        self.expression(&letin.return_expression)?;
        self.stack.pop_local();
        self.replace_letin_reference(old);

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

        let (type_path, tag) = match &structure.parts.data.as_slice() {
            [] => unreachable!(),
            [base, rest @ ..] => {
                let mut path = if rest.is_empty() {
                    self.absolute_path(base, Namespace::Name)
                } else {
                    let mut path = self.absolute_path(base, Namespace::Scope);
                    path.push(rest);
                    path
                };

                let true = self.names.contains(&path) else {
                    return self.error(ResolutionError::UnboundPath(path), Some(span));
                };

                let constructor_name = path.pop();
                let type_name = path.pop();

                // TODO: Can get empty module path when non-constructor name is used because
                //   it doesn't haveto have a type in its path, not nesesarilly it has
                //   at least 3 parts but 2 parts. This is problematic for error reporting
                let Some(module) = &self.modules.get(&path) else {
                    return self.error(ResolutionError::UnboundPath(path), Some(span));
                };

                let Some(constructors) = module.types.get(&type_name) else {
                    return self.error(ResolutionError::UnboundPath(path), Some(span));
                };

                let tag = constructors
                    .iter()
                    .position(|cs| cs == &constructor_name)
                    .unwrap();

                path.push([type_name]);
                (path, tag)
            }
        };

        let structure_pattern = StructurePattern { type_path, tag };
        self.data
            .structure_patterns
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
                let path = self.absolute_path(identifier, Namespace::Type);
                if !self.types.contains_key(&path) {
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
                let path = if rest.is_empty() {
                    self.absolute_path(base, Namespace::Type)
                } else {
                    let mut path = self.absolute_path(base, Namespace::Module);
                    path.push(rest);
                    path
                };

                let true = self.types.contains_key(&path) else {
                    return self.error(ResolutionError::UnboundPath(path), Some(span));
                };

                Bound::Absolute(path)
            }
        };

        self.data.bounds.set(path.bound_id, bound);

        Ok(())
    }

    fn type_application(&mut self, application: &type_expression::Application) -> Result<()> {
        self.type_expression(&application.function)?;

        for argument in &application.arguments {
            self.type_expression(argument)?;
        }

        Ok(())
    }

    pub fn program(mut self, program: &Program) -> Result<ResolutionData> {
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

        for (module, module_path) in program.modules.iter().zip(module_paths) {
            self.current_module_path = Some(module_path);
            self.register_imports(module)?;
            self.module(module)?;
        }

        Ok(self.data)
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
                self.current_module_mut().names.insert(name.identifier.data);

                let path = self.append_current_path(name.identifier.data);
                self.names.insert(path.clone());
                self.data.dependencies.insert(path, vec![]);
            }

            if let Definition::Structure(structure) = definition {
                let structure_path = self.append_current_path(structure.name.data);

                let mut constructor_names = HashSet::new();
                for constructor in &structure.constructors {
                    let name = constructor.data.name.data;
                    constructor_names.insert(name);
                    self.names.insert(structure_path.append([name]));
                }

                self.types.insert(structure_path, constructor_names);

                // TODO: Check for duplicate definitions
                let constructors = structure
                    .constructors
                    .iter()
                    .map(|constructor| constructor.data.name.data)
                    .collect();

                self.current_module_mut()
                    .types
                    .insert(structure.name.data, constructors);
            }
        }

        Ok(())
    }

    fn register_import(&mut self, name: InternId, path: Path, span: Span) -> Result<()> {
        // TODO: When importing allow relative paths from the current module
        let imports = if self.modules.contains_key(&path) {
            &mut self.current_module_mut().module_imports
        } else if self.types.contains_key(&path) {
            &mut self.current_module_mut().type_imports
        } else if self.names.contains(&path) {
            &mut self.current_module_mut().name_imports
        } else {
            return self.error(ResolutionError::UnresolvedImport(path.clone()), Some(span));
        };

        imports.insert(name, Located { data: path, span });

        Ok(())
    }

    // TODO: Import `Main` module automatically
    fn register_imports(&mut self, module: &Module) -> Result<()> {
        for definition in &module.definitions {
            if let Definition::Import(import) = definition {
                let base = Path::from_parts(import.module_path.data.to_vec());

                match &import.subimport {
                    Some(import_name) => {
                        self.register_subimport(import_name, base)?;
                    }
                    None => {
                        let identifier = *import.module_path.data.last().unwrap();
                        self.register_import(identifier, base, import.module_path.span)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn register_subimport(
        &mut self,
        import_name: &definition::Subimport,
        base: Path,
    ) -> Result<()> {
        match import_name {
            definition::Subimport::As(as_name) => {
                self.register_import(as_name.data, base, as_name.span)?;
            }
            definition::Subimport::Import(imports) => {
                for import in imports {
                    let base = base.append(&import.module_path.data);

                    match &import.subimport {
                        Some(subimport) => {
                            self.register_subimport(subimport, base)?;
                        }
                        None => {
                            let identifier = *import.module_path.data.last().unwrap();
                            self.register_import(identifier, base, import.module_path.span)?;
                        }
                    }
                }
            }
            definition::Subimport::All(span) => {
                self.register_import_all(base, *span)?;
            }
        }

        Ok(())
    }

    fn register_import_all(&mut self, path: Path, span: Span) -> Result<()> {
        let (names, types) = if let Some(from) = self.modules.get(&path) {
            (from.names.iter(), Some(from.types.keys()))
        } else if let Some(constructors) = self.types.get(&path) {
            (constructors.iter(), None)
        } else {
            return self.error(ResolutionError::UnresolvedImport(path), Some(span));
        };

        let names = names
            .map(|name| {
                let located = Located {
                    data: path.append([name]),
                    span,
                };

                (*name, located)
            })
            .collect::<Vec<_>>();

        let types = if let Some(types) = types {
            let types = types
                .map(|t| {
                    let located = Located {
                        data: path.append([t]),
                        span,
                    };

                    (*t, located)
                })
                .collect::<Vec<_>>();

            Some(types)
        } else {
            None
        };

        self.current_module_mut().name_imports.extend(names);
        if let Some(types) = types {
            self.current_module_mut().type_imports.extend(types);
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
        let path = self.append_current_path(name_definition.identifier.data);

        self.current_name_definition = Some(path);
        self.expression(&name_definition.expression)?;
        let path = self.current_name_definition.take().unwrap();

        self.data.paths.set(name_definition.path_id, path);

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

        self.data.paths.set(structure_definition.path_id, path);

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

        self.data.paths.set(constructor.path_id, path);

        Ok(())
    }

    fn error<T>(&self, error: ResolutionError, span: Option<Span>) -> Result<T> {
        Err(Box::new(ReportableError {
            error: error.into(),
            source_name: self.current_module().source_name.to_string(),
            span,
        }))
    }
}

/// ANF Name Resolver
/// ANF only affects local variables so ANFResolver only
/// re-resolves local variables
pub struct ANFResolver<'rename_data> {
    /// Stack for local variables
    stack: ResolutionStack<anf::Local>,
    /// Rename produced data
    rename_data: &'rename_data RenameData,
    /// ANF Resolution produced data
    data: ANFResolutionData,
}

impl<'rename_data> ANFResolver<'rename_data> {
    pub fn new(rename_data: &'rename_data RenameData) -> Self {
        ANFResolver {
            stack: ResolutionStack::new(),
            rename_data,
            data: ANFResolutionData::default(),
        }
    }

    fn expression(&mut self, anf: &anf::Expression) {
        match anf {
            anf::Expression::LetIn(letin) => self.letin(letin),
            anf::Expression::Application(application) => self.application(application),
            anf::Expression::MatchAs(matchlet) => self.matchas(matchlet),
            anf::Expression::Join(join) => self.join(join),
            anf::Expression::Jump(jump) => self.jump(jump),
            anf::Expression::Atom(atom) => self.atom(atom),
        }
    }

    pub fn expression_repl(mut self, anf: &anf::Expression) -> ANFResolutionData {
        self.expression(anf);

        self.data
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

        self.data.bounds.set(path.anf_bound_id, bound);
    }

    fn identifier(&mut self, identifier: Local) -> Bound {
        self.stack.locally_resolve(identifier).unwrap()
    }

    fn lambda(&mut self, lambda: &anf::atom::Lambda) {
        self.stack.push_frame();
        // NOTE: Reverse is to preserve left associative application
        //   semantics that is forced by previous phases
        for variable in lambda.variables.iter().rev() {
            self.stack.push_local(Local::Standard(*variable));
        }
        self.expression(&lambda.expression);
        self.stack.pop_local();
        let capture = self.stack.pop_frame();

        self.data.captures.set(lambda.anf_capture_id, capture);
    }

    fn letin(&mut self, letin: &anf::expression::LetIn) {
        self.stack.push_local(Local::Standard(letin.variable));
        self.atom(&letin.variable_expression);
        self.expression(&letin.return_expression);
        self.stack.pop_local();
    }

    fn application(&mut self, application: &anf::expression::Application) {
        // NOTE: Reverse is to preserve left associative application
        //   semantics that is forced by previous phases
        for argument in application.arguments.iter().rev() {
            self.atom(argument);
        }
        self.atom(&application.function);
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
                let unique_name = self.rename_data.unique_names.get(&any.unique_name_id);
                self.stack.push_local(Local::Standard(*unique_name));
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

    pub fn program(mut self, program: &anf::Program) -> ANFResolutionData {
        for module in program.modules() {
            self.module(module);
        }

        self.data
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
}

#[derive(Debug, Clone)]
pub enum ResolutionError {
    UnboundPath(Path),
    MissingModuleDefinition,
    UnresolvedImport(Path),
    RejectedLetInSelfReference,
}

#[derive(Default)]
pub struct ResolutionData {
    pub bounds: DataTable<BoundDataId, Bound>,
    pub captures: DataTable<CaptureDataId, Vec<Capture>>,
    pub structure_patterns: DataTable<StructurePatternDataId, StructurePattern>,
    pub paths: DataTable<PathDataId, Path>,
    pub tail_calls: OptionalDataTable<TailCallDataId, ()>,
    pub self_captures: OptionalDataTable<SelfCaptureDataId, usize>,
    /// Dependency graph for name definitions
    pub dependencies: Graph<Path>,
}

pub struct StructurePattern {
    pub type_path: Path,
    pub tag: usize,
}

#[derive(Default)]
pub struct ANFResolutionData {
    pub bounds: DataTable<ANFBoundDataId, Bound>,
    pub captures: DataTable<ANFCaptureDataId, Vec<Capture>>,
}
