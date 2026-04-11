use std::fmt::Display;

use crate::{
    location::Located,
    parse::{
        definition::{self, Definition, Module, Program},
        expression::{self, Expression},
        pattern::{self, Pattern},
    },
    resolution::{Renamed, Resolved, bound::Bound, frame::CheckStack},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UniqueName(usize);

impl Display for UniqueName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// AST Alpha Renamer
/// Generates and assigned unique names for every local identifier
pub struct Renamer {
    /// Stack to keep track of unique names
    stack: CheckStack<UniqueName>,
    /// State for unique name generation
    unique_name_counter: usize,
}

impl Renamer {
    pub fn new() -> Self {
        Self {
            stack: CheckStack::new(),
            unique_name_counter: 0,
        }
    }

    pub fn interactive_environment(mut self) -> Self {
        self.stack.push_frame(Vec::new());
        self
    }

    fn unique_name(&mut self) -> UniqueName {
        let name = UniqueName(self.unique_name_counter);
        self.unique_name_counter += 1;
        name
    }

    pub fn expression(
        &mut self,
        expression: Located<Expression<Resolved>>,
    ) -> Located<Expression<Renamed>> {
        let (data, span) = expression.destruct();

        let expression = match data {
            Expression::String(id) => Expression::String(id),
            Expression::Path(path) => Expression::Path(self.path(path)),
            Expression::Application(application) => {
                Expression::Application(self.application(application))
            }
            Expression::Lambda(lambda) => Expression::Lambda(self.lambda(lambda)),
            Expression::LetIn(letin) => Expression::LetIn(self.letin(letin)),
            Expression::MatchAs(matchlet) => Expression::MatchAs(self.matchlet(matchlet)),
        };

        Located::new(expression, span)
    }

    fn path(&mut self, path: expression::Path<Resolved>) -> expression::Path<Renamed> {
        let expression::path::ResolvedObservation { parts, bound } = path.observe();

        let unique_name = match &bound {
            Bound::Local(id) => {
                Some(self.stack.get_local(*id))
            }
            Bound::Capture(id) => {
                Some(self.stack.get_capture(*id))
            }
            Bound::Absolute(_) => None,
        };

        expression::path::RenamedObservation {
            parts,
            bound,
            unique_name,
        }
        .into()
    }

    fn application(
        &mut self,
        application: expression::Application<Resolved>,
    ) -> expression::Application<Renamed> {
        let expression::application::Observation { function, argument } = application.observe();

        let function = self.expression(function);
        let argument = self.expression(argument);

        expression::application::Observation { function, argument }.into()
    }

    fn lambda(&mut self, lambda: expression::Lambda<Resolved>) -> expression::Lambda<Renamed> {
        let expression::lambda::ResolvedObservation {
            variable,
            expression,
            captures,
        } = lambda.observe();

        let unique_variable = self.unique_name();

        self.stack.push_frame(captures.clone());
        self.stack.push_local(unique_variable);
        let expression = self.expression(expression);
        self.stack.pop_local();
        self.stack.pop_frame();

        expression::lambda::RenamedObservation {
            variable,
            expression,
            captures,
            unique_variable
        }
        .into()
    }

    fn letin(&mut self, letin: expression::LetIn<Resolved>) -> expression::LetIn<Renamed> {
        let expression::letin::Observation {
            variable,
            variable_expression,
            return_expression,
        } = letin.observe();

        let variable_expression = self.expression(variable_expression);

        let unique_variable = self.unique_name();

        self.stack.push_local(unique_variable);
        let return_expression = self.expression(return_expression);
        self.stack.pop_local();

        expression::letin::RenamedObservation {
            variable,
            variable_expression,
            return_expression,
            unique_variable
        }
        .into()
    }

    fn matchlet(&mut self, matchas: expression::MatchAs<Resolved>) -> expression::MatchAs<Renamed> {
        let expression::matchas::Observation {
            expression,
            branches,
        } = matchas.observe();

        let expression = self.expression(expression);

        let branches = branches
            .into_iter()
            .map(|branch| branch.map(|branch| self.match_branch(branch)))
            .collect();

        expression::matchas::Observation {
            expression,
            branches,
        }
        .into()
    }

    fn match_branch(
        &mut self,
        branch: expression::matchas::Branch<Resolved>,
    ) -> expression::matchas::Branch<Renamed> {
        let expression::matchas::branch::Observation {
            pattern,
            expression,
        } = branch.observe();
        let len = self.stack.len();
        let pattern = pattern.map(|pattern| self.define_locals_and_pattern(pattern));
        let expression = self.expression(expression);
        self.stack.truncate(len);

        expression::matchas::branch::Observation {
            pattern,
            expression,
        }
        .into()
    }

    fn define_locals_and_pattern(&mut self, pattern: Pattern<Resolved>) -> Pattern<Renamed> {
        match pattern {
            Pattern::Any(any) => {
                Pattern::Any(self.any_pattern(any))
            }
            Pattern::String(id) => Pattern::String(id),
            Pattern::Structure(structure) => {
                Pattern::Structure(self.structure_pattern(structure))
            }
        }
    }

    fn any_pattern(&mut self, any: pattern::Any<Resolved>) -> pattern::Any<Renamed> {
        let pattern::any::Observation { identifier } = any.observe();

        let unique_name = self.unique_name();
        self.stack.push_local(unique_name);

        pattern::any::RenamedObservation { identifier, unique_name }.into()
    }

    fn structure_pattern(&mut self, structure: pattern::Structure<Resolved>) -> pattern::Structure<Renamed> {
        let pattern::structure::ResolvedObservation {
            parts,
            arguments,
            type_path,
            constructor_name,
            order,
        } = structure.observe();

        let arguments = arguments
            .into_iter()
            .map(|argument| argument.map(|pattern| self.define_locals_and_pattern(pattern)))
            .collect();

        pattern::structure::RenamedObservation {
            parts,
            arguments,
            type_path,
            constructor_name,
            order,
        }
        .into()
    }

    pub fn program(&mut self, program: Program<Resolved>) -> Program<Renamed> {
        let definition::program::Observation { modules } = program.observe();

        let modules = modules
            .into_iter()
            .map(|module| self.module(module))
            .collect::<Vec<_>>();

        definition::program::Observation { modules }.into()
    }

    pub fn module(&mut self, module: Module<Resolved>) -> Module<Renamed> {
        let definition::module::Observation {
            definitions,
            source_name,
        } = module.observe();

        let mut renamed_definitions = vec![];

        for definition in definitions {
            match definition {
                Definition::Name(name) => {
                    let definition = Definition::Name(self.name_definition(name));
                    renamed_definitions.push(definition);
                }
                Definition::Structure(structure) => {
                    let definition = Definition::Structure(self.structure_definition(structure));
                    renamed_definitions.push(definition);
                }
                Definition::ModulePath(_) | Definition::Import(_) => (),
            }
        }

        definition::module::Observation {
            definitions: renamed_definitions,
            source_name,
        }
        .into()
    }

    fn name_definition(
        &mut self,
        name_definition: definition::Name<Resolved>,
    ) -> definition::Name<Renamed> {
        let definition::name::ResolvedObservation {
            identifier,
            expression,
            path,
        } = name_definition.observe();

        let expression = self.expression(expression);

        definition::name::RenamedObservation {
            identifier,
            expression,
            path,
        }
        .into()
    }

    fn structure_definition(
        &self,
        structure_definition: definition::Structure<Resolved>,
    ) -> definition::Structure<Renamed> {
        // NOTE: Nothing to rename in structure definitions
        //   make state Renamed for bureaucracy
        unsafe { std::mem::transmute(structure_definition) }
    }
}
