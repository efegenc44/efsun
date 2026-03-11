pub mod bound;
pub mod frame;
pub mod renamer;

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::{
    compilation::anf::{self, ANF, ANFLocal, Atom},
    error::{Result, eof_error, located_error},
    interner::{InternId, Interner},
    location::{Located, Span},
    parse::{
        definition::{Constructor, Definition, ImportName, LetDefinition, StructureDefinition},
        expression::{
            ApplicationExpression, Expression, LambdaExpression, LetExpression, MatchBranch,
            MatchExpression, PathExpression, Pattern, StructurePattern,
        },
        type_expression::{ApplicationTypeExpression, PathTypeExpression, TypeExpression},
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
            Expression::Let(letin) => Expression::Let(self.letin(letin)?),
            Expression::Match(matchlet) => Expression::Match(self.matchlet(matchlet)?),
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
        path: PathExpression<Unresolved>,
        span: Span,
    ) -> Result<PathExpression<Resolved>> {
        let bound = match path.parts().data().as_slice() {
            [] => unreachable!(),
            [identifier] => self.identifier(*identifier, span),
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

                Ok(Bound::Absolute(path))
            }
        };

        Ok(path.resolve(bound?))
    }

    fn lambda(
        &mut self,
        lambda: LambdaExpression<Unresolved>,
    ) -> Result<LambdaExpression<Resolved>> {
        let (variable, expression) = lambda.destruct();

        self.stack.push_frame();
        self.stack.push_local(*variable.data());
        let expression = self.expression(expression)?;
        self.stack.pop_local();
        let captures = self.stack.pop_frame();

        Ok(LambdaExpression::<Resolved>::new(
            variable, expression, captures,
        ))
    }

    fn application(
        &mut self,
        application: ApplicationExpression<Unresolved>,
    ) -> Result<ApplicationExpression<Resolved>> {
        let (function, argument) = application.destruct();

        let function = self.expression(function)?;
        let argument = self.expression(argument)?;

        Ok(ApplicationExpression::new(function, argument))
    }

    fn letin(&mut self, letin: LetExpression<Unresolved>) -> Result<LetExpression<Resolved>> {
        let (variable, variable_expression, return_expression) = letin.destruct();

        let variable_expression = self.expression(variable_expression)?;
        self.stack.push_local(*variable.data());
        let return_expression = self.expression(return_expression)?;
        self.stack.pop_local();

        Ok(LetExpression::new(
            variable,
            variable_expression,
            return_expression,
        ))
    }

    fn matchlet(
        &mut self,
        matchlet: MatchExpression<Unresolved>,
    ) -> Result<MatchExpression<Resolved>> {
        let (expression, branches) = matchlet.destruct();

        let expression = self.expression(expression)?;

        let mut resolved_branches = Vec::new();
        for branch in branches {
            let (branch, span) = branch.destruct();
            let branch = Located::new(self.match_branch(branch)?, span);
            resolved_branches.push(branch);
        }

        Ok(MatchExpression::new(expression, resolved_branches))
    }

    fn match_branch(&mut self, branch: MatchBranch<Unresolved>) -> Result<MatchBranch<Resolved>> {
        let (pattern, expression) = branch.destruct();
        let (pattern, span) = pattern.destruct();

        let len = self.stack.len();
        let pattern = self.define_pattern_locals(pattern, span)?;
        let expression = self.expression(expression)?;
        self.stack.truncate(len);

        Ok(MatchBranch::new(Located::new(pattern, span), expression))
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
                let (parts, arguments) = structure.destruct();

                let mut renamed_arguments = Vec::new();
                for argument in arguments {
                    let (argument, span) = argument.destruct();
                    renamed_arguments.push(Located::new(
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

                Ok(Pattern::Structure(StructurePattern::<Resolved>::new(
                    parts,
                    renamed_arguments,
                    type_path,
                    constructor_name,
                    order,
                )))
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
        path: PathTypeExpression<Unresolved>,
        span: Span,
    ) -> Result<PathTypeExpression<Resolved>> {
        let bound = match &path.parts().data()[..] {
            [] => unreachable!(),
            [identifier] => self.type_identifier(identifier, span),
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

                Ok(Bound::Absolute(path))
            }
        };

        Ok(path.resolve(bound?))
    }

    fn type_application(
        &mut self,
        application: ApplicationTypeExpression<Unresolved>,
    ) -> Result<ApplicationTypeExpression<Resolved>> {
        let (function, arguments) = application.destruct();

        let function = self.type_expression(function)?;
        let mut resolved_arguments = Vec::new();
        for argument in arguments {
            resolved_arguments.push(self.type_expression(argument)?);
        }

        Ok(ApplicationTypeExpression::new(function, resolved_arguments))
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
            if let Definition::Module(module) = definition {
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

                match import.name() {
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

    fn collect_import_name(&mut self, import_name: &ImportName, base: &Path) -> Result<()> {
        match import_name {
            ImportName::As(as_name) => {
                self.current_module_mut()
                    .imports_mut()
                    .insert(*as_name, base.clone());
            }
            ImportName::Import(imports) => {
                for import in imports {
                    let base = base.append_parts(import.module_path().data().to_vec());

                    match import.name() {
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
            Definition::Module(module) => Definition::Module(module),
            Definition::Name(name) => Definition::Name(self.let_definition(name)?),
            Definition::Import(import) => Definition::Import(import),
            Definition::Structure(structure) => {
                Definition::Structure(self.structure_definition(structure)?)
            }
        };

        Ok(definition)
    }

    fn let_definition(
        &mut self,
        let_definition: LetDefinition<Unresolved>,
    ) -> Result<LetDefinition<Resolved>> {
        let (identifier, expression) = let_definition.destruct();

        let expression = self.expression(expression)?;
        let path = self.current_module_path.append(*identifier.data());

        Ok(LetDefinition::<Resolved>::new(identifier, expression, path))
    }

    fn structure_definition(
        &mut self,
        structure_definition: StructureDefinition<Unresolved>,
    ) -> Result<StructureDefinition<Resolved>> {
        let (name, variables, constructors) = structure_definition.destruct();

        let structure_path = self.current_module_path.append(*name.data());

        self.type_variables
            .extend(variables.iter().map(|v| *v.data()));
        let mut resolved_constructors = Vec::new();
        for constructor in constructors {
            let (constructor, span) = constructor.destruct();
            let (name, arguments) = constructor.destruct();

            let mut resolved_arguments = Vec::new();
            for argument in arguments {
                resolved_arguments.push(self.type_expression(argument)?);
            }

            let path = structure_path.append(*name.data());
            resolved_constructors.push(Located::new(
                Constructor::<Resolved>::new(name, resolved_arguments, path),
                span,
            ));
        }
        self.type_variables.clear();

        Ok(StructureDefinition::<Resolved>::new(
            name,
            variables,
            resolved_constructors,
            structure_path,
        ))
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

    pub fn expression(&mut self, anf: ANF<Unresolved>) -> ANF<Resolved> {
        match anf {
            ANF::Let(letin) => ANF::Let(self.letin(letin)),
            ANF::Application(application) => ANF::Application(self.application(application)),
            ANF::Match(matchlet) => ANF::Match(self.matchlet(matchlet)),
            ANF::Join(join) => ANF::Join(self.join(join)),
            ANF::Jump(jump) => ANF::Jump(self.jump(jump)),
            ANF::Atom(atom) => ANF::Atom(self.atom(atom)),
        }
    }

    fn atom(&mut self, atom: Atom<Unresolved>) -> Atom<Resolved> {
        match atom {
            Atom::String(id) => Atom::String(id),
            Atom::Path(path) => Atom::Path(self.path(path)),
            Atom::Lambda(lambda) => Atom::Lambda(self.lambda(lambda)),
        }
    }

    fn path(&mut self, path: anf::PathExpression<Unresolved>) -> anf::PathExpression<Resolved> {
        if let Some(bound) = path.bound() {
            let bound = bound.clone();
            path.resolve(bound)
        } else {
            let bound = match path.path() {
                anf::ANFPath::ANFLocal(id) => self.identifier(ANFLocal::ANF(*id)),
                anf::ANFPath::Normal(parts) => match parts.as_slice() {
                    [identifier] => self.identifier(ANFLocal::Normal(*identifier)),
                    _ => unreachable!(),
                },
            };

            path.resolve(bound)
        }
    }

    fn identifier(&mut self, identifier: ANFLocal) -> Bound {
        self.stack.locally_resolve(identifier).unwrap()
    }

    fn lambda(
        &mut self,
        lambda: anf::LambdaExpression<Unresolved>,
    ) -> anf::LambdaExpression<Resolved> {
        let (variable, expression) = lambda.destruct();

        self.stack.push_frame();
        self.stack.push_local(ANFLocal::Normal(variable));
        let expression = self.expression(expression);
        self.stack.pop_local();
        let captures = self.stack.pop_frame();

        anf::LambdaExpression::new(variable, expression, captures)
    }

    fn letin(&mut self, letin: anf::LetExpression<Unresolved>) -> anf::LetExpression<Resolved> {
        let (variable, variable_expression, return_expression) = letin.destruct();

        let variable_expression = self.atom(variable_expression);
        self.stack.push_local(ANFLocal::Normal(variable));
        let return_expression = self.expression(return_expression);
        self.stack.pop_local();

        anf::LetExpression::new(variable, variable_expression, return_expression)
    }

    fn application(
        &mut self,
        application: anf::ApplicationExpression<Unresolved>,
    ) -> anf::ApplicationExpression<Resolved> {
        let (variable, function, argument, expression) = application.destruct();

        let function = self.atom(function);
        let argument = self.atom(argument);
        self.stack.push_local(variable);
        let expression = self.expression(expression);
        self.stack.pop_local();

        anf::ApplicationExpression::new(variable, function, argument, expression)
    }

    fn matchlet(
        &mut self,
        matchlet: anf::MatchExpression<Unresolved>,
    ) -> anf::MatchExpression<Resolved> {
        let (variable, variable_expression, branches) = matchlet.destruct();

        let variable_expression = self.atom(variable_expression);
        self.stack.push_local(variable);
        let mut resolved_branches = Vec::new();
        for branch in branches {
            let (pattern, matched, expression) = branch.destruct();
            let matched = self.atom(matched);
            let len = self.stack.len();
            self.define_pattern_locals(&pattern);
            let expression = self.expression(expression);
            self.stack.truncate(len);
            resolved_branches.push(anf::MatchBranch::new(pattern, matched, expression));
        }
        self.stack.pop_local();

        anf::MatchExpression::new(variable, variable_expression, resolved_branches)
    }

    fn define_pattern_locals(&mut self, pattern: &Pattern<Renamed>) {
        match pattern {
            Pattern::Any(id) => {
                self.stack.push_local(ANFLocal::Normal(*id));
            }
            Pattern::String(_) => (),
            Pattern::Structure(structure) => {
                for argument in structure.arguments() {
                    self.define_pattern_locals(argument.data());
                }
            }
        }
    }

    fn join(&mut self, join: anf::Join<Unresolved>) -> anf::Join<Resolved> {
        let (label, variable, join, expression) = join.destruct();

        let join = self.expression(join);
        self.stack.push_local(variable);
        let expression = self.expression(expression);
        self.stack.pop_local();

        anf::Join::new(label, variable, join, expression)
    }

    fn jump(&mut self, jump: anf::Jump<Unresolved>) -> anf::Jump<Resolved> {
        let (to, expression) = jump.destruct();

        let expression = self.atom(expression);

        anf::Jump::new(to, expression)
    }

    pub fn program(
        &mut self,
        modules: Vec<Vec<anf::ANFDefinition<Unresolved>>>,
    ) -> Vec<Vec<anf::ANFDefinition<Resolved>>> {
        let mut resolved_modules = Vec::new();
        for module in modules {
            resolved_modules.push(self.module(module));
        }

        resolved_modules
    }

    pub fn module(
        &mut self,
        definitions: Vec<anf::ANFDefinition<Unresolved>>,
    ) -> Vec<anf::ANFDefinition<Resolved>> {
        let mut resolved_definitons = Vec::new();
        for definition in definitions {
            resolved_definitons.push(self.definition(definition));
        }

        resolved_definitons
    }

    fn definition(
        &mut self,
        definition: anf::ANFDefinition<Unresolved>,
    ) -> anf::ANFDefinition<Resolved> {
        match definition {
            anf::ANFDefinition::Let(name) => anf::ANFDefinition::Let(self.let_definition(name)),
            anf::ANFDefinition::Structure(structure) => anf::ANFDefinition::Structure(structure),
        }
    }

    fn let_definition(
        &mut self,
        let_definition: anf::LetDefinition<Unresolved>,
    ) -> anf::LetDefinition<Resolved> {
        let (identifier, expression, path) = let_definition.destruct();

        let expression = self.expression(expression);

        anf::LetDefinition::new(identifier, expression, path)
    }
}

#[derive(Debug, Clone)]
pub enum ResolutionError {
    UnboundPath(Path),
    MissingModuleDefinition,
    UnresolvedImport(Path),
}
