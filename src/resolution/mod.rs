pub mod frame;
pub mod bound;
pub mod renamer;

use std::{collections::HashMap, fmt::Debug};

use crate::{
    error::{Result, located_error, eof_error},
    parse::{
        expression::{
            ApplicationExpression, Expression, PathExpression, LambdaExpression,
            LetExpression, MatchExpression, MatchBranch, Pattern,
            StructurePattern
        },
        type_expression::{
            TypeExpression, PathTypeExpression, ApplicationTypeExpression
        },
        definition::{
            Definition, NameDefinition, ImportName, StructureDefinition, Constructor
        }
    },
    interner::{Interner, InternId},
    location::{Located, Span},
    compilation::anf::{self, Atom, ANF, ANFLocal},
};

use bound::{Bound, BoundId, Path, Module};
use frame::ResolutionStack;

#[derive(Clone, Copy)]
pub struct Resolved;
#[derive(Clone, Copy)]
pub struct Unresolved;
#[derive(Clone, Copy)]
pub struct Renamed;

pub struct ExpressionResolver {
    stack: ResolutionStack<InternId>,
    type_locals: Vec<InternId>,

    modules: HashMap<Path, Module>,
    current_module_path: Path
}

impl ExpressionResolver {
    pub fn new() -> Self {
        let mut stack = ResolutionStack::new();
        stack.push_frame();

        ExpressionResolver {
            stack,
            type_locals: Vec::new(),
            modules: HashMap::new(),
            current_module_path: Path::empty(),
        }
    }

    pub fn interactive(interner: &mut Interner) -> Self {
        let mut resolver = Self::new();
        let interactive_id = interner.intern(String::from("interactive"));
        let path = Path::from_parts(vec![interactive_id]);
        let module = Module::empty("<interactive>".to_string());

        resolver.modules.insert(path.clone(), module);
        resolver.current_module_path = path;

        resolver
    }

    fn current_module(&self) -> &Module {
        &self.modules[&self.current_module_path]
    }

    fn current_module_mut(&mut self) -> &mut Module {
        self.modules.get_mut(&self.current_module_path).unwrap()
    }

    pub fn expression(&mut self, expression: Located<Expression<Unresolved>>) -> Result<Located<Expression<Resolved>>> {
        let (expression, span) = expression.destruct();

        let expression = match expression {
            Expression::String(string) => Expression::String(string),
            Expression::Path(path) => Expression::Path(self.path(path, span)?),
            Expression::Lambda(lambda) => Expression::Lambda(self.lambda(lambda)?),
            Expression::Application(application) => Expression::Application(self.application(application)?),
            Expression::Let(letin) => Expression::Let(self.letin(letin)?),
            Expression::Match(matchlet) => Expression::Match(self.matchlet(matchlet)?),
        };

        Ok(Located::new(expression, span))
    }

    fn identifier(&mut self, identifier: InternId, span: Span) -> Result<Bound> {
        match self.stack.locally_resolve(identifier) {
            Some(bound) => Ok(bound),
            None => {
                if self.current_module().names().contains(&identifier) {
                    Ok(Bound::Absolute(self.current_module_path.append(identifier)))
                } else {
                    let error = ResolutionError::UnboundPath(Path::from_parts(vec![identifier]));
                    Err(located_error(error, span, self.current_module().source_name().to_string()))
                }
            },
        }
    }

    fn path(&mut self, path: PathExpression<Unresolved>, span: Span) -> Result<PathExpression<Resolved>> {
        let bound = match &path.parts().data()[..] {
            [] => unreachable!(),
            [identifier] => self.identifier(*identifier, span),
            [base, mid@.., name] => {
                let mut module_path = if let Some(path) = self.current_module().imports().get(base) {
                    path.append_parts(mid.to_vec())
                } else {
                    let mut parts = vec![*base];
                    parts.extend_from_slice(mid);
                    Path::from_parts(parts)
                };

                if let Some(constructors) = self.current_module().types().get(base) {
                    assert!(mid.is_empty());

                    let true = constructors.contains(name) else {
                        todo!("Error");
                    };

                    let path = self.current_module_path
                        .append(*base)
                        .append(*name);

                    Ok(Bound::Absolute(path))
                } else {
                    if let Some(module) = self.modules.get(&module_path) {
                        module_path.push(*name);
                        if module.names().contains(name) {
                            Ok(Bound::Absolute(module_path))
                        } else {
                            let error = ResolutionError::UnboundPath(module_path);
                            Err(located_error(error, span, self.current_module().source_name().to_string()))
                        }
                    } else {
                        let type_name = module_path.pop();

                        let Some(module) = self.modules.get(&module_path) else {
                            let error = ResolutionError::UnboundPath(module_path);
                            return Err(located_error(error, span, self.current_module().source_name().to_string()));
                        };

                        module_path.push(type_name);
                        let Some(constructors) = module.types().get(&type_name) else {
                            let error = ResolutionError::UnboundPath(module_path);
                            return Err(located_error(error, span, self.current_module().source_name().to_string()));
                        };

                        module_path.push(*name);
                        let true = constructors.contains(name) else {
                            let error = ResolutionError::UnboundPath(module_path);
                            return Err(located_error(error, span, self.current_module().source_name().to_string()));
                        };

                        Ok(Bound::Absolute(module_path))
                    }
                }
            }
        };

        Ok(path.resolve(bound?))
    }

    fn lambda(&mut self, lambda: LambdaExpression<Unresolved>) -> Result<LambdaExpression<Resolved>> {
        let (variable, expression) = lambda.destruct();

        self.stack.push_frame();
        self.stack.push_local(*variable.data());
        let expression = self.expression(expression)?;
        self.stack.pop_local();
        let captures = self.stack.pop_frame();

        Ok(LambdaExpression::<Resolved>::new(variable, expression, captures))
    }

    fn application(&mut self, application: ApplicationExpression<Unresolved>) -> Result<ApplicationExpression<Resolved>> {
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

        Ok(LetExpression::new(variable, variable_expression, return_expression))
    }

    fn matchlet(&mut self, matchlet: MatchExpression<Unresolved>) -> Result<MatchExpression<Resolved>> {
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
        let pattern = self.pattern(pattern)?;
        let expression = self.expression(expression)?;
        self.stack.truncate(len);

        Ok(MatchBranch::new(Located::new(pattern, span), expression))
    }

    fn pattern(&mut self, pattern: Pattern<Unresolved>) -> Result<Pattern<Resolved>> {
        match pattern {
            Pattern::Any(id) => {
                self.stack.push_local(id);
                Ok(Pattern::Any(id))
            },
            Pattern::String(id) => Ok(Pattern::String(id)),
            Pattern::Structure(structure) => {
                let (parts, arguments) = structure.destruct();

                let mut renamed_arguments = Vec::new();
                for argument in arguments {
                    let (argument, span) = argument.destruct();
                    renamed_arguments.push(Located::new(self.pattern(argument)?, span));
                }

                let (type_path, order) = match &parts.data()[..] {
                    [] | [_] => unreachable!(),
                    [t, c] => {
                        let Some(constructors) = self.current_module().types().get(t) else {
                            todo!("Error");
                        };

                        let Some(order) = constructors.iter().position(|cs| cs == c) else {
                            todo!("Error");
                        };

                        (self.current_module_path.append(*t), order)
                    },
                    [module@.., t, c] => {
                        dbg!(parts.data());

                        let module_path = Path::from_parts(module.to_vec());
                        let module = &self.modules[&module_path];

                        let Some(constructors) = module.types().get(t) else {
                            todo!("Error");
                        };

                        let Some(order) = constructors.iter().position(|cs| cs == c) else {
                            todo!("Error");
                        };

                        (module_path.append(*t), order)
                    }
                };

                Ok(Pattern::Structure(StructurePattern::<Resolved>::new(parts, renamed_arguments, type_path, order)))
            }
        }
    }


    fn type_expression(&mut self, expression: Located<TypeExpression<Unresolved>>) -> Result<Located<TypeExpression<Resolved>>> {
        let (expression, span) = expression.destruct();

        let expression = match expression {
            TypeExpression::Path(path) => TypeExpression::Path(self.type_path(path, span)?),
            TypeExpression::Application(application) => TypeExpression::Application(self.type_application(application)?),
        };

        Ok(Located::new(expression, span))
    }

    fn type_path(&mut self, path: PathTypeExpression<Unresolved>, span: Span) -> Result<PathTypeExpression<Resolved>> {
        let bound = match &path.parts().data()[..] {
            [] => unreachable!(),
            [identifier] => {
                let mut bound = None;
                for (index, intern_id) in self.type_locals.iter().rev().enumerate() {
                    if identifier == intern_id {
                        bound = Some(BoundId::new(self.type_locals.len() - 1 - index));
                        break;
                    }
                }

                if let Some(bound) = bound {
                    Ok(Bound::Local(bound))
                } else {
                    if self.current_module().types().get(&identifier).is_some() {
                        Ok(Bound::Absolute(self.current_module_path.append(*identifier)))
                    } else {
                        let error = ResolutionError::UnboundPath(Path::from_parts(vec![*identifier]));
                        Err(located_error(error, span, self.current_module().source_name().to_string()))
                    }
                }
            },
            [base, mid@.., name] => {
                let mut module_path = if let Some(path) = self.current_module().imports().get(base) {
                    path.append_parts(mid.to_vec())
                } else {
                    let mut parts = vec![*base];
                    parts.extend_from_slice(mid);
                    Path::from_parts(parts)
                };

                let Some(module) = self.modules.get(&module_path) else {
                    let error = ResolutionError::UnboundPath(module_path);
                    return Err(located_error(error, span, self.current_module().source_name().to_string()));
                };

                module_path.push(*name);
                if module.types().get(name).is_some() {
                    Ok(Bound::Absolute(module_path))
                } else {
                    let error = ResolutionError::UnboundPath(module_path);
                    Err(located_error(error, span, self.current_module().source_name().to_string()))
                }
            }
        };

        Ok(path.resolve(bound?))
    }

    fn type_application(&mut self, application: ApplicationTypeExpression<Unresolved>) -> Result<ApplicationTypeExpression<Resolved>> {
        let (function, arguments) = application.destruct();

        let function = self.type_expression(function)?;
        let mut resolved_arguments = Vec::new();
        for argument in arguments {
            resolved_arguments.push(self.type_expression(argument)?);
        }

        Ok(ApplicationTypeExpression::new(function, resolved_arguments))
    }

    pub fn program(&mut self, modules: Vec<(Vec<Definition<Unresolved>>, String)>) -> Result<Vec<(Vec<Definition<Resolved>>, String)>> {
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

    pub fn module(&mut self, definitions: Vec<Definition<Unresolved>>) -> Result<Vec<Definition<Resolved>>> {
        let mut resolved_definitons = Vec::new();
        for definition in definitions {
            resolved_definitons.push(self.definition(definition)?);
        }

        Ok(resolved_definitons)
    }

    fn find_module_name(&mut self, definitions: (&[Definition<Unresolved>], &String)) -> Result<Path> {
        for definition in definitions.0 {
            if let Definition::Module(module) = definition {
                let module_path = Path::from_parts(module.parts().data().to_vec());
                self.modules.insert(module_path.clone(), Module::empty(definitions.1.clone()));
                self.current_module_path = module_path.clone();

                return Ok(module_path)
            }
        }

        // TODO: Check for duplicate module definition
        Err(eof_error(ResolutionError::MissingModuleDefinition, definitions.1.clone()))
    }

    fn collect_names(&mut self, definitions: &[Definition<Unresolved>]) -> Result<()> {
        for definition in definitions {
            if let Definition::Name(name) = definition {
                // TODO: Check for duplicate definitions
                self
                    .current_module_mut()
                    .names_mut()
                    .insert(*name.identifier().data());
            }

            if let Definition::Structure(structure) = definition {
                // TODO: Check for duplicate definitions
                let mut constructors = Vec::new();
                for constructor in structure.constructors() {
                    constructors.push(*constructor.name().data());
                }

                self
                    .current_module_mut()
                    .types_mut()
                    .insert(*structure.name().data(), constructors);
            }

            if let Definition::Import(import) = definition {
                let base = Path::from_parts(import.module_path().to_vec());

                match import.name() {
                    Some(import_name) => {
                        self.collect_import_name(import_name, &base)?;
                    },
                    None => {
                        self.current_module_mut().imports_mut().insert(
                            *import.module_path().last().unwrap(),
                            base
                        );
                    },
                }
            }
        }

        Ok(())
    }

    fn collect_import_name(&mut self, import_name: &ImportName, base: &Path) -> Result<()> {
        match import_name {
            ImportName::As(as_name) => {
                self.current_module_mut().imports_mut().insert(
                    *as_name, base.clone()
                );
            },
            ImportName::Import(imports) => {
                for import in imports {
                    let base = base.append_parts(import.module_path().to_vec());

                    match import.name() {
                        Some(import_name) => {
                            self.collect_import_name(import_name, &base)?;
                        },
                        None => {
                            self.current_module_mut().imports_mut().insert(
                                *import.module_path().last().unwrap(),
                                base
                            );
                        },
                    }
                }
            },
        }

        Ok(())
    }

    fn check_if_imports_exist(&self) -> Result<()> {
        for module in self.modules.values() {
            for import in module.imports().values() {
                if !self.modules.contains_key(import) {
                    let mut module = import.clone();
                    let name = module.pop();

                    let Some(module) = self.modules.get(&module) else {
                        return Err(eof_error(
                            ResolutionError::UnresolvedImport(import.clone()),
                            self.current_module().source_name().to_string()
                        ));
                    };

                    let true = module.names().contains(&name) else {
                        return Err(eof_error(
                            ResolutionError::UnresolvedImport(import.clone()),
                            self.current_module().source_name().to_string()
                        ));
                    };
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
            Definition::Structure(structure) => Definition::Structure(self.structure_definition(structure)?),
        };

        Ok(definition)
    }

    fn let_definition(&mut self, let_definition: NameDefinition<Unresolved>) -> Result<NameDefinition<Resolved>> {
        let (identifier, expression) = let_definition.destruct();

        let expression = self.expression(expression)?;
        let path = self.current_module_path.append(*identifier.data());

        Ok(NameDefinition::<Resolved>::new(identifier, expression, path))
    }

    fn structure_definition(&mut self, structure_definition: StructureDefinition<Unresolved>) -> Result<StructureDefinition<Resolved>> {
        let (name, variables, constructors) = structure_definition.destruct();

        let structure_path = self.current_module_path.append(*name.data());

        self.type_locals.extend(variables.iter().map(|v| *v.data()));
        let mut resolved_constructors = Vec::new();
        for constructor in constructors {
            let (name, arguments) = constructor.destruct();

            let mut resolved_arguments = Vec::new();
            for argument in arguments {
                resolved_arguments.push(self.type_expression(argument)?);
            }

            let path = structure_path.append(*name.data());
            resolved_constructors.push(Constructor::<Resolved>::new(name, resolved_arguments, path));
        }
        self.type_locals.clear();

        Ok(StructureDefinition::<Resolved>::new(name, variables, resolved_constructors, structure_path))
    }
}

pub struct ANFResolver {
    stack: ResolutionStack<anf::ANFLocal>,
}

impl ANFResolver {
    pub fn new() -> Self {
        let mut stack = ResolutionStack::new();
        stack.push_frame();

        ANFResolver {
            stack,
        }
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
                anf::ANFPath::ANF(id) => {
                    self.identifier(ANFLocal::ANF(*id))
                },
                anf::ANFPath::Normal(parts) => {
                    match &parts[..] {
                        [identifier] => self.identifier(ANFLocal::Normal(*identifier)),
                        _ => unreachable!(),
                    }
                },
            };

            path.resolve(bound)
        }
    }

    fn identifier(&mut self, identifier: ANFLocal) -> Bound {
        self.stack.locally_resolve(identifier).unwrap()
    }

    fn lambda(&mut self, lambda: anf::LambdaExpression<Unresolved>) -> anf::LambdaExpression<Resolved> {
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

    fn application(&mut self, application: anf::ApplicationExpression<Unresolved>) -> anf::ApplicationExpression<Resolved> {
        let (variable, function, argument, expression) = application.destruct();

        let function = self.atom(function);
        let argument = self.atom(argument);
        self.stack.push_local(variable.clone());
        let expression = self.expression(expression);
        self.stack.pop_local();

        anf::ApplicationExpression::new(variable, function, argument, expression)
    }

    fn matchlet(&mut self, matchlet: anf::MatchExpression<Unresolved>) -> anf::MatchExpression<Resolved> {
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
            Pattern::Any(_) => (),
            Pattern::String(_) => (),
            Pattern::Structure(structure) => {
                for argument in structure.arguments() {
                    self.define_pattern_locals(argument.data());
                }
            },
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

    pub fn program(&mut self, modules: Vec<Vec<anf::ANFDefinition<Unresolved>>>) -> Vec<Vec<anf::ANFDefinition<Resolved>>> {
        let mut resolved_modules = Vec::new();
        for module in modules {
            resolved_modules.push(self.module(module));
        }

        resolved_modules
    }

    pub fn module(&mut self, definitions: Vec<anf::ANFDefinition<Unresolved>>) -> Vec<anf::ANFDefinition<Resolved>> {
        let mut resolved_definitons = Vec::new();
        for definition in definitions {
            resolved_definitons.push(self.definition(definition));
        }

        resolved_definitons
    }

    fn definition(&mut self, definition: anf::ANFDefinition<Unresolved>) -> anf::ANFDefinition<Resolved> {
        match definition {
            anf::ANFDefinition::Name(name) => anf::ANFDefinition::Name(self.let_definition(name)),
            anf::ANFDefinition::Structure(structure) => anf::ANFDefinition::Structure(structure)
        }
    }

    fn let_definition(&mut self, let_definition: anf::NameDefinition<Unresolved>) -> anf::NameDefinition<Resolved> {
        let (identifier, expression, path) = let_definition.destruct();

        let expression = self.expression(expression);

        anf::NameDefinition::new(identifier, expression, path)
    }
}

#[derive(Debug, Clone)]
pub enum ResolutionError {
    UnboundPath(Path),
    MissingModuleDefinition,
    UnresolvedImport(Path)
}
