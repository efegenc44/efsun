pub mod frame;
pub mod bound;
pub mod renamer;

use std::{collections::HashMap, fmt::Debug};

use crate::{
    error::{Result, located_error},
    parse::{
        expression::{
            ApplicationExpression, Expression, PathExpression, LambdaExpression,
            LetExpression
        },
        definition::{Definition, NameDefinition, ImportName}
    },
    interner::{Interner, InternId},
    location::{Located, SourceLocation},
    compilation::anf::{self, Atom, ANF, ANFLocal},
};

use bound::{Bound, Path, Module};
use frame::ResolutionStack;

#[derive(Clone, Copy)]
pub struct Resolved;
#[derive(Clone, Copy)]
pub struct Unresolved;
#[derive(Clone, Copy)]
pub struct Renamed;

pub struct ExpressionResolver {
    stack: ResolutionStack<InternId>,

    modules: HashMap<Path, Module>,
    current_module_path: Path
}

impl ExpressionResolver {
    pub fn new() -> Self {
        let mut stack = ResolutionStack::new();
        stack.push_frame();

        ExpressionResolver {
            stack,
            modules: HashMap::new(),
            current_module_path: Path::empty(),
        }
    }

    pub fn interactive(interner: &mut Interner) -> Self {
        let mut resolver = Self::new();
        let interactive_id = interner.intern(String::from("interactive"));
        let path = Path::from_parts(vec![interactive_id]);
        let module = Module::empty();

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
        let (expression, start, end) = expression.destruct();

        let expression = match expression {
            Expression::String(string) => Expression::String(string),
            Expression::Path(path) => Expression::Path(self.path(path, start, end)?),
            Expression::Lambda(lambda) => Expression::Lambda(self.lambda(lambda)?),
            Expression::Application(application) => Expression::Application(self.application(application)?),
            Expression::Let(letin) => Expression::Let(self.letin(letin)?),
        };

        Ok(Located::new(expression, start, end))
    }

    fn identifier(&mut self, identifier: InternId, start: SourceLocation, end: SourceLocation) -> Result<Bound> {
        match self.stack.locally_resolve(identifier) {
            Some(bound) => Ok(bound),
            None => {
                if self.current_module().names().contains(&identifier) {
                    Ok(Bound::Absolute(self.current_module_path.append(identifier)))
                } else {
                    let error = ResolutionError::UnboundPath(Path::from_parts(vec![identifier]));
                    Err(located_error(error, start, end))
                }
            },
        }
    }

    fn path(
        &mut self,
        path: PathExpression<Unresolved>,
        start: SourceLocation,
        end: SourceLocation
    ) -> Result<PathExpression<Resolved>> {
        let bound = match &path.parts().data()[..] {
            [] => unreachable!(),
            [identifier] => self.identifier(*identifier, start, end),
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
                    return Err(located_error(error, start, end));
                };

                module_path.push(*name);
                if module.names().contains(name) {
                    Ok(Bound::Absolute(module_path))
                } else {
                    let error = ResolutionError::UnboundPath(module_path);
                    Err(located_error(error, start, end))
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

    pub fn program(&mut self, modules: Vec<Vec<Definition<Unresolved>>>) -> Result<Vec<Vec<Definition<Resolved>>>> {
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

        Ok(resolved_modules)
    }

    pub fn module(&mut self, definitions: Vec<Definition<Unresolved>>) -> Result<Vec<Definition<Resolved>>> {
        let mut resolved_definitons = Vec::new();
        for definition in definitions {
            resolved_definitons.push(self.definition(definition)?);
        }

        Ok(resolved_definitons)
    }

    fn find_module_name(&mut self, definitions: &[Definition<Unresolved>]) -> Result<Path> {
        for definition in definitions {
            if let Definition::Module(module) = definition {
                let module_path = Path::from_parts(module.parts().data().to_vec());
                self.modules.insert(module_path.clone(), Module::empty());
                self.current_module_path = module_path.clone();

                return Ok(module_path)
            }
        }

        // TODO: Check for duplicate module definition
        Err(located_error(
            ResolutionError::MissingModuleDefinition,
            SourceLocation::eof(),
            SourceLocation::eof())
        )
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
                        return Err(located_error(
                            ResolutionError::UnresolvedImport(import.clone()),
                            SourceLocation::eof(),
                            SourceLocation::eof()
                        ));
                    };

                    let true = module.names().contains(&name) else {
                        return Err(located_error(
                            ResolutionError::UnresolvedImport(import.clone()),
                            SourceLocation::eof(),
                            SourceLocation::eof()
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
            Definition::Import(import) => Definition::Import(import)
        };

        Ok(definition)
    }

    fn let_definition(&mut self, let_definition: NameDefinition<Unresolved>) -> Result<NameDefinition<Resolved>> {
        let (identifier, expression) = let_definition.destruct();

        let expression = self.expression(expression)?;
        let path = self.current_module_path.append(*identifier.data());

        Ok(NameDefinition::<Resolved>::new(identifier, expression, path))
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
