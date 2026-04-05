pub mod bound;
pub mod frame;
pub mod renamer;

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::{
    compilation::anf::{self, ANFLocal},
    error::{Result, eof_error, located_error},
    interner::{InternId, Interner},
    location::{Located, Span},
    parse::{
        definition::{self, Definition},
        expression::{self, Expression},
        pattern::{self, Pattern},
        type_expression::{self, TypeExpression},
    },
};

use bound::{Bound, BoundId, Module, Path};
use frame::ResolutionStack;

#[derive(Clone, Copy)]
pub struct Resolved;
#[derive(Clone, Copy)]
pub struct Unresolved;
#[derive(Clone, Copy)]
pub struct Renamed;

pub struct ExpressionResolver {
    stack: ResolutionStack<InternId>,
    type_variables: Vec<InternId>,
    names: HashSet<Path>,
    types: HashSet<Path>,
    modules: HashMap<Path, Module>,
    current_module_path: Path,
}

impl ExpressionResolver {
    pub fn new() -> Self {
        let mut stack = ResolutionStack::new();
        stack.push_frame();

        ExpressionResolver {
            stack,
            type_variables: Vec::new(),
            names: HashSet::new(),
            types: HashSet::new(),
            modules: HashMap::new(),
            current_module_path: Path::empty(),
        }
    }

    pub fn interactive_environment(mut self, interner: &mut Interner) -> Self {
        let interactive_id = interner.intern(String::from("interactive"));
        let path = Path::from_parts(vec![interactive_id]);
        let module = Module::empty("<interactive>".to_string());

        self.modules.insert(path.clone(), module);
        self.current_module_path = path;

        self
    }

    fn current_module(&self) -> &Module {
        &self.modules[&self.current_module_path]
    }

    fn current_module_mut(&mut self) -> &mut Module {
        self.modules.get_mut(&self.current_module_path).unwrap()
    }

    pub fn expression(
        &mut self,
        expression: Located<Expression<Unresolved>>,
    ) -> Result<Located<Expression<Resolved>>> {
        let (expression, span) = expression.destruct();

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

        Ok(Located::new(expression, span))
    }

    fn base_path(&self, base: &InternId) -> Path {
        if let Some(import_path) = self.current_module().imports().get(base) {
            import_path.clone()
        } else {
            self.current_module_path.append(*base)
        }
    }

    fn identifier(&mut self, identifier: InternId, span: Span) -> Result<Bound> {
        match self.stack.locally_resolve(identifier) {
            Some(bound) => Ok(bound),
            None => {
                let path = self.base_path(&identifier);
                if !self.names.contains(&path) {
                    let error = ResolutionError::UnboundPath(Path::from_parts(vec![identifier]));
                    Err(located_error(
                        error,
                        span,
                        self.current_module().source_name().to_string(),
                    ))
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
                let path = self.base_path(base).append_parts(rest.to_vec());

                let true = self.names.contains(&path) else {
                    let error = ResolutionError::UnboundPath(path);
                    return Err(located_error(
                        error,
                        span,
                        self.current_module().source_name().to_string(),
                    ));
                };

                Bound::Absolute(path)
            }
        };

        Ok(expression::path::ResolvedObservation { parts, bound }.into())
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
        self.stack.push_local(*variable.data());
        let expression = self.expression(expression)?;
        self.stack.pop_local();
        let captures = self.stack.pop_frame();

        Ok(expression::lambda::ResolvedObservation {
            variable,
            expression,
            captures,
        }
        .into())
    }

    fn application(
        &mut self,
        application: expression::Application<Unresolved>,
    ) -> Result<expression::Application<Resolved>> {
        let expression::application::Observation { function, argument } = application.observe();

        let function = self.expression(function)?;
        let argument = self.expression(argument)?;

        Ok(expression::application::Observation { function, argument }.into())
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
        self.stack.push_local(*variable.data());
        let return_expression = self.expression(return_expression)?;
        self.stack.pop_local();

        Ok(expression::letin::Observation {
            variable,
            variable_expression,
            return_expression,
        }
        .into())
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

        let mut resolved_branches = Vec::new();
        for branch in branches {
            let (branch, span) = branch.destruct();
            let branch = Located::new(self.match_branch(branch)?, span);
            resolved_branches.push(branch);
        }

        Ok(expression::matchas::Observation {
            expression,
            branches: resolved_branches,
        }
        .into())
    }

    fn match_branch(
        &mut self,
        branch: expression::matchas::Branch<Unresolved>,
    ) -> Result<expression::matchas::Branch<Resolved>> {
        let expression::matchas::branch::Observation {
            pattern,
            expression,
        } = branch.observe();
        let (pattern, span) = pattern.destruct();

        let len = self.stack.len();
        let pattern = self.define_pattern_locals(pattern, span)?;
        let expression = self.expression(expression)?;
        self.stack.truncate(len);

        Ok(expression::matchas::branch::Observation {
            pattern: Located::new(pattern, span),
            expression,
        }
        .into())
    }

    fn define_pattern_locals(
        &mut self,
        pattern: Pattern<Unresolved>,
        span: Span,
    ) -> Result<Pattern<Resolved>> {
        match pattern {
            Pattern::Any(id) => {
                self.stack.push_local(id);
                Ok(Pattern::Any(id))
            }
            Pattern::String(id) => Ok(Pattern::String(id)),
            Pattern::Structure(structure) => {
                let pattern::structure::UnresolvedObservation { parts, arguments } =
                    structure.observe();

                let mut resolved_arguments = Vec::new();
                for argument in arguments {
                    let (argument, span) = argument.destruct();
                    resolved_arguments.push(Located::new(
                        self.define_pattern_locals(argument, span)?,
                        span,
                    ));
                }

                let (type_path, constructor_name, order) = match parts.data().as_slice() {
                    [] => unreachable!(),
                    [base, rest @ ..] => {
                        let mut path = self.base_path(base).append_parts(rest.to_vec());

                        let true = self.names.contains(&path) else {
                            let error = ResolutionError::UnboundPath(path);
                            return Err(located_error(
                                error,
                                span,
                                self.current_module().source_name().to_string(),
                            ));
                        };

                        let c = path.pop();
                        let t = path.pop();

                        let constructors = &self.modules[&path].types()[&t];
                        let order = constructors.iter().position(|cs| cs == &c).unwrap();

                        (path.append(t), c, order)
                    }
                };

                let structure = pattern::structure::ResolvedObservation {
                    parts,
                    arguments: resolved_arguments,
                    type_path,
                    constructor_name,
                    order,
                }
                .into();

                Ok(Pattern::Structure(structure))
            }
        }
    }

    fn type_expression(
        &mut self,
        expression: Located<TypeExpression<Unresolved>>,
    ) -> Result<Located<TypeExpression<Resolved>>> {
        let (expression, span) = expression.destruct();

        let expression = match expression {
            TypeExpression::Path(path) => TypeExpression::Path(self.type_path(path, span)?),
            TypeExpression::Application(application) => {
                TypeExpression::Application(self.type_application(application)?)
            }
        };

        Ok(Located::new(expression, span))
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
                let path = self.base_path(identifier);
                if !self.types.contains(&path) {
                    let error = ResolutionError::UnboundPath(Path::from_parts(vec![*identifier]));
                    Err(located_error(
                        error,
                        span,
                        self.current_module().source_name().to_string(),
                    ))
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
                let path = self.base_path(base).append_parts(rest.to_vec());

                let true = self.types.contains(&path) else {
                    let error = ResolutionError::UnboundPath(path);
                    return Err(located_error(
                        error,
                        span,
                        self.current_module().source_name().to_string(),
                    ));
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
        let mut resolved_arguments = Vec::new();
        for argument in arguments {
            resolved_arguments.push(self.type_expression(argument)?);
        }

        Ok(type_expression::application::Observation {
            function,
            arguments: resolved_arguments,
        }
        .into())
    }

    pub fn program(
        &mut self,
        modules: Vec<(Vec<Definition<Unresolved>>, String)>,
    ) -> Result<Vec<(Vec<Definition<Resolved>>, String)>> {
        let mut module_paths = vec![];
        for (module, source_name) in &modules {
            module_paths.push(self.find_module_name((module, source_name))?);
            self.collect_names(module)?;
        }

        self.check_if_imports_exist()?;

        let mut resolved_modules = Vec::new();
        for ((module, source_name), module_path) in modules.into_iter().zip(module_paths) {
            self.current_module_path = module_path;
            resolved_modules.push((self.module(module)?, source_name));
        }

        Ok(resolved_modules)
    }

    pub fn module(
        &mut self,
        definitions: Vec<Definition<Unresolved>>,
    ) -> Result<Vec<Definition<Resolved>>> {
        let mut resolved_definitons = Vec::new();
        for definition in definitions {
            resolved_definitons.push(self.definition(definition)?);
        }

        Ok(resolved_definitons)
    }

    fn find_module_name(
        &mut self,
        definitions: (&[Definition<Unresolved>], &String),
    ) -> Result<Path> {
        for definition in definitions.0 {
            if let Definition::ModulePath(module) = definition {
                let module_path = Path::from_parts(module.parts().data().to_vec());
                self.modules
                    .insert(module_path.clone(), Module::empty(definitions.1.clone()));
                self.current_module_path = module_path.clone();

                return Ok(module_path);
            }
        }

        // TODO: Check for duplicate module definition
        Err(eof_error(
            ResolutionError::MissingModuleDefinition,
            definitions.1.clone(),
        ))
    }

    fn collect_names(&mut self, definitions: &[Definition<Unresolved>]) -> Result<()> {
        for definition in definitions {
            if let Definition::Name(name) = definition {
                // TODO: Check for duplicate definitions
                self.current_module_mut()
                    .names_mut()
                    .insert(*name.identifier().data());

                self.names
                    .insert(self.current_module_path.append(*name.identifier().data()));
            }

            if let Definition::Structure(structure) = definition {
                let structure_path = self.current_module_path.append(*structure.name().data());

                for constructor in structure.constructors() {
                    self.names
                        .insert(structure_path.append(*constructor.data().name().data()));
                }

                self.types.insert(structure_path);

                // TODO: Check for duplicate definitions
                let constructors = structure
                    .constructors()
                    .iter()
                    .map(|constructor| *constructor.data().name().data())
                    .collect();

                self.current_module_mut()
                    .types_mut()
                    .insert(*structure.name().data(), constructors);
            }

            if let Definition::Import(import) = definition {
                let base = Path::from_parts(import.module_path().data().to_vec());

                match import.subimport() {
                    Some(import_name) => {
                        self.collect_import_name(import_name, &base)?;
                    }
                    None => {
                        self.current_module_mut()
                            .imports_mut()
                            .insert(*import.module_path().data().last().unwrap(), base);
                    }
                }
            }
        }

        Ok(())
    }

    fn collect_import_name(
        &mut self,
        import_name: &definition::import::Subimport,
        base: &Path,
    ) -> Result<()> {
        match import_name {
            definition::import::Subimport::As(as_name) => {
                self.current_module_mut()
                    .imports_mut()
                    .insert(*as_name, base.clone());
            }
            definition::import::Subimport::Import(imports) => {
                for import in imports {
                    let base = base.append_parts(import.module_path().data().to_vec());

                    match import.subimport() {
                        Some(subimport) => {
                            self.collect_import_name(subimport, &base)?;
                        }
                        None => {
                            self.current_module_mut()
                                .imports_mut()
                                .insert(*import.module_path().data().last().unwrap(), base);
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
                if !(self.names.contains(import)
                    || self.types.contains(import)
                    || self.modules.contains_key(import))
                {
                    return Err(eof_error(
                        ResolutionError::UnresolvedImport(import.clone()),
                        self.current_module().source_name().to_string(),
                    ));
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
        let path = self.current_module_path.append(*identifier.data());

        Ok(definition::name::ResolvedObservation {
            identifier,
            expression,
            path,
        }
        .into())
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

        let structure_path = self.current_module_path.append(*name.data());

        self.type_variables
            .extend(variables.iter().map(|v| *v.data()));
        let mut resolved_constructors = Vec::new();
        for constructor in constructors {
            let (constructor, span) = constructor.destruct();
            let definition::structure::constructor::UnresolvedObservation { name, arguments } =
                constructor.observe();

            let mut resolved_arguments = Vec::new();
            for argument in arguments {
                resolved_arguments.push(self.type_expression(argument)?);
            }

            let path = structure_path.append(*name.data());
            resolved_constructors.push(Located::new(
                definition::structure::constructor::ResolvedObservation {
                    name,
                    arguments: resolved_arguments,
                    path,
                }
                .into(),
                span,
            ));
        }
        self.type_variables.clear();

        Ok(definition::structure::ResolvedObservation {
            name,
            variables,
            constructors: resolved_constructors,
            path: structure_path,
        }
        .into())
    }
}

pub struct ANFResolver {
    stack: ResolutionStack<anf::ANFLocal>,
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
            anf::Expression::Let(letin) => anf::Expression::Let(self.letin(letin)),
            anf::Expression::Application(application) => {
                anf::Expression::Application(self.application(application))
            }
            anf::Expression::Match(matchlet) => anf::Expression::Match(self.matchlet(matchlet)),
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
                anf::ANFPath::ANFLocal(id) => self.identifier(ANFLocal::ANF(*id)),
                anf::ANFPath::Normal(parts) => match parts.as_slice() {
                    [identifier] => self.identifier(ANFLocal::Normal(*identifier)),
                    _ => unreachable!(),
                },
            }
        };

        anf::atom::path::ResolvedObservation { path, bound }.into()
    }

    fn identifier(&mut self, identifier: ANFLocal) -> Bound {
        self.stack.locally_resolve(identifier).unwrap()
    }

    fn lambda(&mut self, lambda: anf::atom::Lambda<Unresolved>) -> anf::atom::Lambda<Resolved> {
        let anf::atom::lambda::UnresolvedObservation {
            variable,
            expression,
        } = lambda.observe();

        self.stack.push_frame();
        self.stack.push_local(ANFLocal::Normal(variable));
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
        self.stack.push_local(ANFLocal::Normal(variable));
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

    fn matchlet(
        &mut self,
        matchas: anf::expression::MatchAs<Unresolved>,
    ) -> anf::expression::MatchAs<Resolved> {
        let anf::expression::matchas::Observation {
            variable,
            variable_expression,
            branches,
        } = matchas.observe();

        let variable_expression = self.atom(variable_expression);
        self.stack.push_local(variable);
        let mut resolved_branches = Vec::new();
        for branch in branches {
            let anf::expression::matchas::branch::Observation {
                pattern,
                matched,
                expression,
            } = branch.observe();

            let matched = self.atom(matched);
            let len = self.stack.len();
            self.define_pattern_locals(&pattern);
            let expression = self.expression(expression);
            self.stack.truncate(len);
            resolved_branches.push(
                anf::expression::matchas::branch::Observation {
                    pattern,
                    matched,
                    expression,
                }
                .into(),
            );
        }
        self.stack.pop_local();

        anf::expression::matchas::Observation {
            variable,
            variable_expression,
            branches: resolved_branches,
        }
        .into()
    }

    fn define_pattern_locals(&mut self, pattern: &Pattern<Renamed>) {
        match pattern {
            Pattern::Any(id) => {
                self.stack.push_local(ANFLocal::Normal(*id));
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

    pub fn program(
        &mut self,
        modules: Vec<Vec<anf::Definition<Unresolved>>>,
    ) -> Vec<Vec<anf::Definition<Resolved>>> {
        let mut resolved_modules = Vec::new();
        for module in modules {
            resolved_modules.push(self.module(module));
        }

        resolved_modules
    }

    pub fn module(
        &mut self,
        definitions: Vec<anf::Definition<Unresolved>>,
    ) -> Vec<anf::Definition<Resolved>> {
        let mut resolved_definitons = Vec::new();
        for definition in definitions {
            resolved_definitons.push(self.definition(definition));
        }

        resolved_definitons
    }

    fn definition(
        &mut self,
        definition: anf::definition::Definition<Unresolved>,
    ) -> anf::Definition<Resolved> {
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
