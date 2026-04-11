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
    parse::{
        definition::{self, Definition, Module, Program},
        expression::{self, Expression},
        pattern::{self, Pattern},
        type_expression::{self, TypeExpression},
    },
};

use bound::{Bound, BoundId, Module as ModuleBound, Path};
use frame::ResolutionStack;

#[derive(Clone, Copy)]
pub struct Unresolved;
#[derive(Clone, Copy)]
pub struct Resolved;
#[derive(Clone, Copy)]
pub struct Renamed;

pub trait RenamedState {}
pub trait ResolvedState : RenamedState {}
#[allow(unused)]
pub trait UnresolvedState : ResolvedState {}

impl UnresolvedState for Unresolved {}
impl ResolvedState for Unresolved {}
impl RenamedState for Unresolved {}

impl ResolvedState for Resolved {}
impl RenamedState for Resolved {}

impl RenamedState for Renamed {}

// impl UnresolvedState for Unresolved {}
// impl ResolvedState for Unresolved {}
// impl RenamedState for Unresolved {}

// impl UnresolvedState for Resolved {}
// impl ResolvedState for Resolved {}

// impl UnresolvedState for Renamed {}
// impl ResolvedState for Renamed {}
// impl RenamedState for Renamed {}

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
    current_module_path: Path,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            stack: ResolutionStack::new(),
            type_variables: Vec::new(),
            names: HashSet::new(),
            types: HashSet::new(),
            modules: HashMap::new(),
            current_module_path: Path::empty(),
        }
    }

    pub fn interactive_environment(mut self, interner: &mut Interner) -> Self {
        let interactive_id = interner.intern(String::from("interactive"));
        let path = Path::from_parts([interactive_id]);
        let module = ModuleBound::empty("<interactive>".to_string());

        self.modules.insert(path.clone(), module);
        self.current_module_path = path;

        self.stack.push_frame();
        self
    }

    fn current_module(&self) -> &ModuleBound {
        &self.modules[&self.current_module_path]
    }

    fn current_module_mut(&mut self) -> &mut ModuleBound {
        self.modules.get_mut(&self.current_module_path).unwrap()
    }

    pub fn expression(
        &mut self,
        expression: Located<Expression<Unresolved>>,
    ) -> Result<Located<Expression<Resolved>>> {
        let span = expression.span();

        expression
            .map(|expression| {
                let expression = match expression {
                    Expression::String(string) => Expression::String(string),
                    Expression::Path(path) => Expression::Path(self.path(path, span)?),
                    Expression::Lambda(lambda) => Expression::Lambda(self.lambda(lambda)?),
                    Expression::Application(application) => {
                        Expression::Application(self.application(application)?)
                    }
                    Expression::LetIn(letin) => Expression::LetIn(self.letin(letin)?),
                    Expression::MatchAs(matchlet) => Expression::MatchAs(self.matchlet(matchlet)?),
                };

                Ok(expression)
            })
            .transpose()
    }

    fn absolute_path(&self, base: &InternId) -> Path {
        if let Some(import_path) = self.current_module().imports().get(base) {
            import_path.data().clone()
        } else {
            self.current_module_path.append([*base])
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
                        span,
                    )
                } else {
                    Ok(Bound::Absolute(path))
                }
            }
        }
    }

    fn path(
        &mut self,
        path: expression::Path<Unresolved>,
        span: Span,
    ) -> Result<expression::Path<Resolved>> {
        let expression::path::UnresolvedObservation { parts } = path.observe();

        let bound = match parts.data().as_slice() {
            [] => unreachable!(),
            [identifier] => self.identifier(*identifier, span)?,
            [base, rest @ ..] => {
                let mut path = self.absolute_path(base);
                path.push(rest);

                let true = self.names.contains(&path) else {
                    return self.error(ResolutionError::UnboundPath(path), span);
                };

                Bound::Absolute(path)
            }
        };

        let path = expression::path::ResolvedObservation { parts, bound };

        Ok(path.into())
    }

    fn lambda(
        &mut self,
        lambda: expression::Lambda<Unresolved>,
    ) -> Result<expression::Lambda<Resolved>> {
        let expression::lambda::UnresolvedObservation {
            variable,
            expression,
        } = lambda.observe();

        self.stack.push_frame();
        self.stack.push_local(variable.into_data());
        let expression = self.expression(expression)?;
        self.stack.pop_local();
        let captures = self.stack.pop_frame();

        let lambda = expression::lambda::ResolvedObservation {
            variable,
            expression,
            captures,
        };

        Ok(lambda.into())
    }

    fn application(
        &mut self,
        application: expression::Application<Unresolved>,
    ) -> Result<expression::Application<Resolved>> {
        let expression::application::Observation { function, argument } = application.observe();

        let function = self.expression(function)?;
        let argument = self.expression(argument)?;

        let application = expression::application::Observation { function, argument };

        Ok(application.into())
    }

    fn letin(
        &mut self,
        letin: expression::LetIn<Unresolved>,
    ) -> Result<expression::LetIn<Resolved>> {
        let expression::letin::Observation {
            variable,
            variable_expression,
            return_expression,
        } = letin.observe();

        let variable_expression = self.expression(variable_expression)?;
        self.stack.push_local(variable.into_data());
        let return_expression = self.expression(return_expression)?;
        self.stack.pop_local();

        let letin = expression::letin::Observation {
            variable,
            variable_expression,
            return_expression,
        };

        Ok(letin.into())
    }

    fn matchlet(
        &mut self,
        matchas: expression::MatchAs<Unresolved>,
    ) -> Result<expression::MatchAs<Resolved>> {
        let expression::matchas::Observation {
            expression,
            branches,
        } = matchas.observe();

        let expression = self.expression(expression)?;

        let branches = branches
            .into_iter()
            .map(|branch| branch.map(|branch| self.match_branch(branch)))
            .collect::<Result<_>>()?;

        let matchas = expression::matchas::Observation {
            expression,
            branches,
        };

        Ok(matchas.into())
    }

    fn match_branch(
        &mut self,
        branch: expression::matchas::Branch<Unresolved>,
    ) -> Result<expression::matchas::Branch<Resolved>> {
        let expression::matchas::branch::Observation {
            pattern,
            expression,
        } = branch.observe();

        let span = pattern.span();

        let len = self.stack.len();
        let pattern = pattern
            .map(|pattern| self.pattern_and_define_locals(pattern, span))
            .transpose()?;
        let expression = self.expression(expression)?;
        self.stack.truncate(len);

        let branch = expression::matchas::branch::Observation {
            pattern,
            expression,
        };

        Ok(branch.into())
    }

    fn pattern_and_define_locals(
        &mut self,
        pattern: Pattern<Unresolved>,
        span: Span,
    ) -> Result<Pattern<Resolved>> {
        let pattern = match pattern {
            Pattern::Any(any) => {
                Pattern::Any(self.any_pattern(any)?)
            }
            Pattern::String(id) => Pattern::String(id),
            Pattern::Structure(structure) => {
                Pattern::Structure(self.structure_pattern(structure, span)?)
            }
        };

        Ok(pattern)
    }

    fn any_pattern(&mut self, any: pattern::Any<Unresolved>) -> Result<pattern::Any<Resolved>> {
        let pattern::any::Observation { identifier } = any.observe();

        self.stack.push_local(identifier);

        Ok(pattern::any::Observation { identifier }.into())
    }

    fn structure_pattern(
        &mut self,
        structure: pattern::Structure<Unresolved>,
        span: Span,
    ) -> Result<pattern::Structure<Resolved>> {
        let pattern::structure::UnresolvedObservation { parts, arguments } = structure.observe();

        let arguments = arguments
            .into_iter()
            .map(|argument| argument.map(|argument| self.pattern_and_define_locals(argument, span)))
            .collect::<Result<_>>()?;

        let (type_path, constructor_name, order) = match parts.data().as_slice() {
            [] => unreachable!(),
            [base, rest @ ..] => {
                let mut path = self.absolute_path(base);
                path.push(rest);

                let true = self.names.contains(&path) else {
                    return self.error(ResolutionError::UnboundPath(path), span);
                };

                let constructor_name = path.pop();
                let type_name = path.pop();

                let constructors = &self.modules[&path].types()[&type_name];
                let order = constructors
                    .iter()
                    .position(|cs| cs == &constructor_name)
                    .unwrap();

                path.push([type_name]);
                (path, constructor_name, order)
            }
        };

        let structure = pattern::structure::ResolvedObservation {
            parts,
            arguments,
            type_path,
            constructor_name,
            order,
        };

        Ok(structure.into())
    }

    fn type_expression(
        &mut self,
        expression: Located<TypeExpression<Unresolved>>,
    ) -> Result<Located<TypeExpression<Resolved>>> {
        let span = expression.span();

        expression
            .map(|expression| {
                let expression = match expression {
                    TypeExpression::Path(path) => TypeExpression::Path(self.type_path(path, span)?),
                    TypeExpression::Application(application) => {
                        TypeExpression::Application(self.type_application(application)?)
                    }
                };

                Ok(expression)
            })
            .transpose()
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
                        span,
                    )
                } else {
                    Ok(Bound::Absolute(path))
                }
            }
        }
    }

    fn type_path(
        &mut self,
        path: type_expression::Path<Unresolved>,
        span: Span,
    ) -> Result<type_expression::Path<Resolved>> {
        let type_expression::path::UnresolvedObservation { parts } = path.observe();

        let bound = match parts.data().as_slice() {
            [] => unreachable!(),
            [identifier] => self.type_identifier(identifier, span)?,
            [base, rest @ ..] => {
                let mut path = self.absolute_path(base);
                path.push(rest);

                let true = self.types.contains(&path) else {
                    return self.error(ResolutionError::UnboundPath(path), span);
                };

                Bound::Absolute(path)
            }
        };

        Ok(type_expression::path::ResolvedObservation { parts, bound }.into())
    }

    fn type_application(
        &mut self,
        application: type_expression::Application<Unresolved>,
    ) -> Result<type_expression::Application<Resolved>> {
        let type_expression::application::Observation {
            function,
            arguments,
        } = application.observe();

        let function = self.type_expression(function)?;
        let arguments = arguments
            .into_iter()
            .map(|argument| self.type_expression(argument))
            .collect::<Result<_>>()?;

        let application = type_expression::application::Observation {
            function,
            arguments,
        };

        Ok(application.into())
    }

    pub fn program(&mut self, program: Program<Unresolved>) -> Result<Program<Resolved>> {
        let definition::program::Observation { modules } = program.observe();

        let mut module_paths = vec![];
        for module in &modules {
            module_paths.push(self.find_module_name(module)?);
            self.collect_names(module)?;
        }

        self.check_if_imports_exist()?;

        let mut resolved_modules = Vec::new();
        for (module, module_path) in modules.into_iter().zip(module_paths) {
            self.current_module_path = module_path;
            resolved_modules.push(self.module(module)?);
        }

        Ok(definition::program::Observation {
            modules: resolved_modules,
        }
        .into())
    }

    pub fn module(&mut self, module: Module<Unresolved>) -> Result<Module<Resolved>> {
        let definition::module::Observation {
            definitions,
            source_name,
        } = module.observe();

        let definitions = definitions
            .into_iter()
            .map(|definition| self.definition(definition))
            .collect::<Result<Vec<_>>>()?;

        Ok(definition::module::Observation {
            definitions,
            source_name,
        }
        .into())
    }

    fn find_module_name(&mut self, module: &Module<Unresolved>) -> Result<Path> {
        for definition in module.definitions() {
            if let Definition::ModulePath(module_path) = definition {
                let module_path = Path::from_parts(module_path.parts().data().to_vec());
                self.modules.insert(
                    module_path.clone(),
                    ModuleBound::empty(module.source_name().to_string()),
                );
                self.current_module_path = module_path.clone();

                return Ok(module_path);
            }
        }

        // TODO: Check for duplicate module definition
        Err(ReportableError::eof(
            ResolutionError::MissingModuleDefinition,
            module.source_name().to_string(),
        ))
    }

    fn collect_names(&mut self, module: &Module<Unresolved>) -> Result<()> {
        for definition in module.definitions() {
            if let Definition::Name(name) = definition {
                // TODO: Check for duplicate definitions
                self.current_module_mut()
                    .names_mut()
                    .insert(name.identifier().into_data());

                self.names.insert(
                    self.current_module_path
                        .append([name.identifier().into_data()]),
                );
            }

            if let Definition::Structure(structure) = definition {
                let structure_path = self
                    .current_module_path
                    .append([structure.name().into_data()]);

                for constructor in structure.constructors() {
                    self.names
                        .insert(structure_path.append([constructor.data().name().into_data()]));
                }

                self.types.insert(structure_path);

                // TODO: Check for duplicate definitions
                let constructors = structure
                    .constructors()
                    .iter()
                    .map(|constructor| constructor.data().name().into_data())
                    .collect();

                self.current_module_mut()
                    .types_mut()
                    .insert(structure.name().into_data(), constructors);
            }

            if let Definition::Import(import) = definition {
                let base = Path::from_parts(import.module_path().data().to_vec());

                match import.subimport() {
                    Some(import_name) => {
                        self.collect_subimport(import_name, &base)?;
                    }
                    None => {
                        self.current_module_mut().imports_mut().insert(
                            *import.module_path().data().last().unwrap(),
                            Located::new(base, import.module_path().span()),
                        );
                    }
                }
            }
        }

        Ok(())
    }

    fn collect_subimport(
        &mut self,
        import_name: &definition::import::Subimport,
        base: &Path,
    ) -> Result<()> {
        match import_name {
            definition::import::Subimport::As(as_name) => {
                self.current_module_mut()
                    .imports_mut()
                    .insert(*as_name.data(), Located::new(base.clone(), as_name.span()));
            }
            definition::import::Subimport::Import(imports) => {
                for import in imports {
                    let base = base.append(import.module_path().data());

                    match import.subimport() {
                        Some(subimport) => {
                            self.collect_subimport(subimport, &base)?;
                        }
                        None => {
                            self.current_module_mut().imports_mut().insert(
                                *import.module_path().data().last().unwrap(),
                                Located::new(base, import.module_path().span()),
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
                if !(self.names.contains(import.data())
                    || self.types.contains(import.data())
                    || self.modules.contains_key(import.data()))
                {
                    return self.error(
                        ResolutionError::UnresolvedImport(import.data().clone()),
                        import.span(),
                    );
                }
            }
        }

        Ok(())
    }

    fn definition(&mut self, definiton: Definition<Unresolved>) -> Result<Definition<Resolved>> {
        let definition = match definiton {
            Definition::ModulePath(module) => Definition::ModulePath(module),
            Definition::Name(name) => Definition::Name(self.name_definition(name)?),
            Definition::Import(import) => Definition::Import(import),
            Definition::Structure(structure) => {
                Definition::Structure(self.structure_definition(structure)?)
            }
        };

        Ok(definition)
    }

    fn name_definition(
        &mut self,
        name_definition: definition::Name<Unresolved>,
    ) -> Result<definition::Name<Resolved>> {
        let definition::name::UnresolvedObservation {
            identifier,
            expression,
        } = name_definition.observe();

        let expression = self.expression(expression)?;
        let path = self.current_module_path.append([identifier.into_data()]);

        let name = definition::name::ResolvedObservation {
            identifier,
            expression,
            path,
        };

        Ok(name.into())
    }

    fn structure_definition(
        &mut self,
        structure_definition: definition::Structure<Unresolved>,
    ) -> Result<definition::Structure<Resolved>> {
        let definition::structure::UnresolvedObservation {
            name,
            variables,
            constructors,
        } = structure_definition.observe();

        let path = self.current_module_path.append([name.into_data()]);

        self.type_variables
            .extend(variables.iter().map(|v| v.into_data()));

        let constructors = constructors
            .into_iter()
            .map(|constructor| constructor.map(|constructor| self.constructor(constructor, &path)))
            .collect::<Result<_>>()?;

        self.type_variables.clear();

        let structure = definition::structure::ResolvedObservation {
            name,
            variables,
            constructors,
            path,
        };

        Ok(structure.into())
    }

    fn constructor(
        &mut self,
        constructor: definition::structure::Constructor<Unresolved>,
        type_path: &Path,
    ) -> Result<definition::structure::Constructor<Resolved>> {
        let definition::structure::constructor::UnresolvedObservation { name, arguments } =
            constructor.observe();

        let arguments = arguments
            .into_iter()
            .map(|argument| self.type_expression(argument))
            .collect::<Result<_>>()?;

        let path = type_path.append([name.into_data()]);
        let constructor = definition::structure::constructor::ResolvedObservation {
            name,
            arguments,
            path,
        };

        Ok(constructor.into())
    }

    fn error<T>(&self, error: ResolutionError, span: Span) -> Result<T> {
        Err(ReportableError::new(
            error,
            span,
            self.current_module().source_name().to_string(),
        ))
    }
}

/// ANF Name Resolver
/// ANF only affects local variables so ANFResolver only
/// re-resolves local variables
pub struct ANFResolver {
    /// Stack for local variables
    stack: ResolutionStack<anf::Local>,
}

impl ANFResolver {
    pub fn new() -> Self {
        ANFResolver {
            stack: ResolutionStack::new(),
        }
    }

    pub fn interactive_environment(mut self) -> Self {
        self.stack.push_frame();
        self
    }

    pub fn expression(&mut self, anf: anf::Expression<Unresolved>) -> anf::Expression<Resolved> {
        match anf {
            anf::Expression::LetIn(letin) => anf::Expression::LetIn(self.letin(letin)),
            anf::Expression::Application(application) => {
                anf::Expression::Application(self.application(application))
            }
            anf::Expression::Match(matchlet) => anf::Expression::Match(self.matchas(matchlet)),
            anf::Expression::Join(join) => anf::Expression::Join(self.join(join)),
            anf::Expression::Jump(jump) => anf::Expression::Jump(self.jump(jump)),
            anf::Expression::Atom(atom) => anf::Expression::Atom(self.atom(atom)),
        }
    }

    fn atom(&mut self, atom: anf::Atom<Unresolved>) -> anf::Atom<Resolved> {
        match atom {
            anf::Atom::String(id) => anf::Atom::String(id),
            anf::Atom::Path(path) => anf::Atom::Path(self.path(path)),
            anf::Atom::Lambda(lambda) => anf::Atom::Lambda(self.lambda(lambda)),
        }
    }

    fn path(&mut self, path: anf::atom::Path<Unresolved>) -> anf::atom::Path<Resolved> {
        let anf::atom::path::UnresolvedObservation { path, bound } = path.observe();

        let bound = if let Some(bound) = bound {
            bound
        } else {
            match &path {
                anf::Path::ANFLocal(id) => self.identifier(Local::ANFLocal(*id)),
                anf::Path::Local(local) => self.identifier(Local::Standard(*local)),
                anf::Path::Absolute(_) => unreachable!(),
            }
        };

        anf::atom::path::ResolvedObservation { path, bound }.into()
    }

    fn identifier(&mut self, identifier: Local) -> Bound {
        self.stack.locally_resolve(identifier).unwrap()
    }

    fn lambda(&mut self, lambda: anf::atom::Lambda<Unresolved>) -> anf::atom::Lambda<Resolved> {
        let anf::atom::lambda::UnresolvedObservation {
            variable,
            expression,
        } = lambda.observe();

        self.stack.push_frame();
        self.stack.push_local(Local::Standard(variable));
        let expression = self.expression(expression);
        self.stack.pop_local();
        let captures = self.stack.pop_frame();

        anf::atom::lambda::ResolvedObservation {
            variable,
            expression,
            captures,
        }
        .into()
    }

    fn letin(
        &mut self,
        letin: anf::expression::LetIn<Unresolved>,
    ) -> anf::expression::LetIn<Resolved> {
        let anf::expression::letin::Observation {
            variable,
            variable_expression,
            return_expression,
        } = letin.observe();

        let variable_expression = self.atom(variable_expression);
        self.stack.push_local(Local::Standard(variable));
        let return_expression = self.expression(return_expression);
        self.stack.pop_local();

        anf::expression::letin::Observation {
            variable,
            variable_expression,
            return_expression,
        }
        .into()
    }

    fn application(
        &mut self,
        application: anf::expression::Application<Unresolved>,
    ) -> anf::expression::Application<Resolved> {
        let anf::expression::application::Observation {
            variable,
            function,
            argument,
            expression,
        } = application.observe();

        let function = self.atom(function);
        let argument = self.atom(argument);
        self.stack.push_local(variable);
        let expression = self.expression(expression);
        self.stack.pop_local();

        anf::expression::application::Observation {
            variable,
            function,
            argument,
            expression,
        }
        .into()
    }

    fn matchas(
        &mut self,
        matchas: anf::expression::MatchAs<Unresolved>,
    ) -> anf::expression::MatchAs<Resolved> {
        let anf::expression::matchas::Observation {
            expression,
            branches,
        } = matchas.observe();

        let expression = self.atom(expression);
        let branches = branches
            .into_iter()
            .map(|branch| self.branch(branch))
            .collect();

        anf::expression::matchas::Observation {
            expression,
            branches,
        }
        .into()
    }

    fn branch(
        &mut self,
        branch: anf::expression::matchas::Branch<Unresolved>,
    ) -> anf::expression::matchas::Branch<Resolved> {
        let anf::expression::matchas::branch::Observation {
            pattern,
            expression,
        } = branch.observe();

        let len = self.stack.len();
        self.define_pattern_locals(&pattern);
        let expression = self.expression(expression);
        self.stack.truncate(len);

        anf::expression::matchas::branch::Observation {
            pattern,
            expression,
        }
        .into()
    }

    fn define_pattern_locals(&mut self, pattern: &Pattern<Renamed>) {
        match pattern {
            Pattern::Any(any) => {
                self.stack.push_local(Local::Standard(any.unique_name()));
            }
            pattern::Pattern::String(_) => (),
            pattern::Pattern::Structure(structure) => {
                for argument in structure.arguments() {
                    self.define_pattern_locals(argument.data());
                }
            }
        }
    }

    fn join(&mut self, join: anf::expression::Join<Unresolved>) -> anf::expression::Join<Resolved> {
        let anf::expression::join::Observation {
            label,
            variable,
            join,
            expression,
        } = join.observe();

        let join = self.expression(join);
        self.stack.push_local(variable);
        let expression = self.expression(expression);
        self.stack.pop_local();

        anf::expression::join::Observation {
            label,
            variable,
            join,
            expression,
        }
        .into()
    }

    fn jump(&mut self, jump: anf::expression::Jump<Unresolved>) -> anf::expression::Jump<Resolved> {
        let anf::expression::jump::Observation { to, expression } = jump.observe();

        let expression = self.atom(expression);

        anf::expression::jump::Observation { to, expression }.into()
    }

    pub fn program(&mut self, program: anf::Program<Unresolved>) -> anf::Program<Resolved> {
        let anf::definition::program::Observation { modules } = program.observe();

        let modules = modules
            .into_iter()
            .map(|module| self.module(module))
            .collect();

        anf::definition::program::Observation { modules }.into()
    }

    pub fn module(&mut self, module: anf::Module<Unresolved>) -> anf::Module<Resolved> {
        let anf::definition::module::Observation { definitions } = module.observe();

        let definitions = definitions
            .into_iter()
            .map(|definition| self.definition(definition))
            .collect();

        anf::definition::module::Observation { definitions }.into()
    }

    fn definition(&mut self, definition: anf::Definition<Unresolved>) -> anf::Definition<Resolved> {
        match definition {
            anf::Definition::Name(name) => anf::Definition::Name(self.name_definition(name)),
            anf::Definition::Structure(structure) => anf::Definition::Structure(structure),
        }
    }

    fn name_definition(
        &mut self,
        name_definition: anf::definition::Name<Unresolved>,
    ) -> anf::definition::Name<Resolved> {
        let anf::definition::name::Observation {
            identifier,
            expression,
            path,
        } = name_definition.observe();

        let expression = self.expression(expression);

        anf::definition::name::Observation {
            identifier,
            expression,
            path,
        }
        .into()
    }
}

#[derive(Debug, Clone)]
pub enum ResolutionError {
    UnboundPath(Path),
    MissingModuleDefinition,
    UnresolvedImport(Path),
}
