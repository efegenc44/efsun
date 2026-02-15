pub mod frame;
pub mod bound;

use std::{collections::HashMap, fmt::Debug};

use crate::{
    error::{Result, located_error},
    parse::{
        expression::{
            ApplicationExpression, Expression, PathExpression, LambdaExpression,
            LetExpression
        },
        definition::{Definition, NameDefinition}
    },
    interner::{Interner, InternId},
    location::{Located, SourceLocation},
    compilation::anf::{self, Atom, ANF, ANFLocal},
};

use bound::{Bound, BoundId, Path, Module};
use frame::Stack;

#[derive(Clone, Copy)]
pub struct Resolved;
#[derive(Clone, Copy)]
pub struct Unresolved;

pub struct ExpressionResolver {
    stack: Stack<InternId>,

    modules: HashMap<Path, Module>,
    current_module_path: Path
}

impl ExpressionResolver {
    pub fn new() -> Self {
        let mut stack = Stack::new();
        stack.push_frame();

        ExpressionResolver {
            stack,
            modules: HashMap::new(),
            current_module_path: Path::empty(),
        }
    }

    pub fn interactive_module(&mut self, interner: &mut Interner) {
        let interactive_id = interner.intern(String::from("interactive"));
        let path = Path::from_parts(vec![interactive_id]);
        let module = Module::empty();

        self.modules.insert(path.clone(), module);
        self.current_module_path = path;
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
        match self.stack.current_frame().resolve(identifier) {
            Some(id) => Ok(Bound::Local(id)),
            None => {
                match self.stack.capture(identifier) {
                    Some(capture) => {
                        let id = match self.stack.current_frame().captures().iter().position(|c| *c == capture) {
                            Some(id) => id,
                            None => {
                                self.stack.current_frame_mut().captures_mut().push(capture);
                                self.stack.current_frame().captures().len() - 1
                            }
                        };

                        Ok(Bound::Capture(BoundId::new(id)))
                    },
                    None => {
                        if self.current_module().names().contains(&identifier) {
                            Ok(Bound::Absolute(self.current_module_path.append(identifier)))
                        } else {
                            let error = ResolutionError::UnboundIdentifier(identifier);
                            Err(located_error(error, start, end))
                        }
                    }
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
            [module@.., name] => {
                let mut module_path = Path::from_parts(module.to_vec());
                if self.modules[&module_path].names().contains(name) {
                    module_path.push(*name);
                    Ok(Bound::Absolute(module_path))
                } else {
                    let error = ResolutionError::UnboundIdentifier(*name);
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
        self.stack.push_frame();
        self.stack.push_local(*variable.data());
        let return_expression = self.expression(return_expression)?;
        self.stack.pop_local();
        let captures = self.stack.pop_frame();

        Ok(LetExpression::<Resolved>::new(variable, variable_expression, return_expression, captures))
    }

    pub fn module(&mut self, definitions: Vec<Definition<Unresolved>>) -> Result<Vec<Definition<Resolved>>> {
        self.find_module_name(&definitions)?;
        self.collect_names(&definitions)?;

        let mut resolved_definitons = Vec::new();
        for definition in definitions {
            resolved_definitons.push(self.definition(definition)?);
        }

        Ok(resolved_definitons)
    }

    fn find_module_name(&mut self, definitions: &[Definition<Unresolved>]) -> Result<()> {
        for definition in definitions {
            if let Definition::Module(module) = definition {
                let module_path = Path::from_parts(module.parts().data().to_vec());
                self.modules.insert(module_path.clone(), Module::empty());
                self.current_module_path = module_path;

                return Ok(())
            }
        }

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
        }

        Ok(())
    }

    fn definition(&mut self, definiton: Definition<Unresolved>) -> Result<Definition<Resolved>> {
        let definition = match definiton {
            Definition::Module(module) => Definition::Module(module),
            Definition::Name(name) => Definition::Name(self.let_definition(name)?),
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
    stack: Stack<anf::ANFLocal>,

    modules: HashMap<Path, Module>,
    current_module_path: Path
}

impl ANFResolver {
    pub fn new() -> Self {
        let mut stack = Stack::new();
        stack.push_frame();

        ANFResolver {
            stack,
            modules: HashMap::new(),
            current_module_path: Path::empty()
        }
    }

    fn current_module_mut(&mut self) -> &mut Module {
        self.modules.get_mut(&self.current_module_path).unwrap()
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
        let bound = match path.path() {
            anf::ANFPath::ANF(id) => {
                self.identifier(ANFLocal::ANF(*id))
            },
            anf::ANFPath::Normal(parts) => {
                match &parts[..] {
                    [] => unreachable!(),
                    [identifier] => self.identifier(ANFLocal::Normal(*identifier)),
                    [module@.., name] => {
                        let mut module_path = Path::from_parts(module.to_vec());
                        module_path.push(*name);
                        Bound::Absolute(module_path)
                    }
                }
            },
        };

        path.resolve(bound)
    }

    fn identifier(&mut self, identifier: ANFLocal) -> Bound {
        match self.stack.current_frame().resolve(identifier) {
            Some(id) => Bound::Local(id),
            None => {
                match self.stack.capture(identifier) {
                    Some(capture) => {
                        let id = match self.stack.current_frame().captures().iter().position(|c| *c == capture) {
                            Some(id) => id,
                            None => {
                                self.stack.current_frame_mut().captures_mut().push(capture);
                                self.stack.current_frame().captures().len() - 1
                            }
                        };

                        Bound::Capture(BoundId::new(id))
                    },
                    None => {
                        let ANFLocal::Normal(identifier) = identifier else {
                            panic!()
                        };

                        let path = self.current_module_path.append(identifier);
                        Bound::Absolute(path)
                    },
                }
            },
        }
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

    pub fn module(&mut self, definitions: Vec<anf::ANFDefinition<Unresolved>>) -> Vec<anf::ANFDefinition<Resolved>> {
        self.find_module_name(&definitions);
        self.collect_names(&definitions);

        let mut resolved_definitons = Vec::new();
        for definition in definitions {
            resolved_definitons.push(self.definition(definition));
        }

        resolved_definitons
    }

    fn find_module_name(&mut self, definitions: &[anf::ANFDefinition<Unresolved>]) {
        for definition in definitions {
            if let anf::ANFDefinition::Module(module) = definition {
                let module_path = Path::from_parts(module.parts().to_vec());
                self.modules.insert(module_path.clone(), Module::empty());
                self.current_module_path = module_path;
                return;
            }
        }

        unreachable!()
    }

    fn collect_names(&mut self, definitions: &[anf::ANFDefinition<Unresolved>]) {
        for definition in definitions {
            if let anf::ANFDefinition::Name(name) = definition {
                self
                    .current_module_mut()
                    .names_mut()
                    .insert(name.identifier());
            }
        }
    }

    fn definition(&mut self, definition: anf::ANFDefinition<Unresolved>) -> anf::ANFDefinition<Resolved> {
        match definition {
            anf::ANFDefinition::Module(module) => anf::ANFDefinition::Module(module),
            anf::ANFDefinition::Name(name) => anf::ANFDefinition::Name(self.let_definition(name)),
        }
    }

    fn let_definition(&mut self, let_definition: anf::NameDefinition<Unresolved>) -> anf::NameDefinition<Resolved> {
        let (identifier, expression) = let_definition.destruct();

        let expression = self.expression(expression);
        let path = self.current_module_path.append(identifier);

        anf::NameDefinition::<Resolved>::new(identifier, expression, path)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ResolutionError {
    UnboundIdentifier(InternId),
    MissingModuleDefinition,
}
