use std::fmt::Display;

use crate::{
    location::Located,
    metadata::Metadata,
    parse::{
        definition::{self, Definition, Module, Program},
        expression::{self, Expression},
        pattern::{self, Pattern},
    },
    resolution::{bound::Bound, frame::CheckStack},
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
pub struct Renamer<'metadata> {
    /// Stack to keep track of unique names
    stack: CheckStack<UniqueName>,
    /// State for unique name generation
    unique_name_counter: usize,
    /// Metadata
    metadata: &'metadata mut Metadata,
}

impl<'metadata> Renamer<'metadata> {
    pub fn new(metadata: &'metadata mut Metadata) -> Self {
        Self {
            stack: CheckStack::new(),
            unique_name_counter: 0,
            metadata,
        }
    }

    fn unique_name(&mut self) -> UniqueName {
        let name = UniqueName(self.unique_name_counter);
        self.unique_name_counter += 1;
        name
    }

    pub fn expression(&mut self, expression: &Located<Expression>) {
        match expression.data() {
            Expression::String(_) => (),
            Expression::Path(path) => self.path(path),
            Expression::Application(application) => self.application(application),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::LetIn(letin) => self.letin(letin),
            Expression::MatchAs(matchas) => self.matchas(matchas),
        };
    }

    fn path(&mut self, path: &expression::Path) {
        let bound = self.metadata.get_bound(path.bound_id());

        let unique_name = match bound {
            Bound::Local(id) => Some(self.stack.get_local(*id)),
            Bound::Capture(id) => Some(self.stack.get_capture(*id)),
            Bound::Absolute(_) => None,
        };

        self.metadata
            .set_unique_name(path.unique_name_id(), unique_name);
    }

    fn application(&mut self, application: &expression::Application) {
        self.expression(application.function());
        self.expression(application.argument());
    }

    fn lambda(&mut self, lambda: &expression::Lambda) {
        let unique_variable = self.unique_name();

        let capture = self.metadata.get_capture(lambda.capture_id());
        self.stack.push_frame(capture.to_vec());
        self.stack.push_local(unique_variable);
        self.expression(lambda.expression());
        self.stack.pop_local();
        self.stack.pop_frame();

        self.metadata
            .set_unique_name(lambda.unique_name_id(), Some(unique_variable));
    }

    fn letin(&mut self, letin: &expression::LetIn) {
        self.expression(letin.variable_expression());

        let unique_variable = self.unique_name();

        self.stack.push_local(unique_variable);
        self.expression(letin.return_expression());
        self.stack.pop_local();

        self.metadata
            .set_unique_name(letin.unique_name_id(), Some(unique_variable));
    }

    fn matchas(&mut self, matchas: &expression::MatchAs) {
        self.expression(matchas.expression());

        for branch in matchas.branches() {
            self.match_branch(branch.data());
        }
    }

    fn match_branch(&mut self, branch: &expression::matchas::Branch) {
        let len = self.stack.len();
        self.define_pattern_locals(branch.pattern().data());
        self.expression(branch.expression());
        self.stack.truncate(len);
    }

    fn define_pattern_locals(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Any(any) => self.any_pattern(any),
            Pattern::String(_) => (),
            Pattern::Structure(structure) => self.structure_pattern(structure),
        }
    }

    fn any_pattern(&mut self, any: &pattern::Any) {
        let unique_name = self.unique_name();
        self.stack.push_local(unique_name);

        self.metadata
            .set_unique_name(any.unique_name_id(), Some(unique_name));
    }

    fn structure_pattern(&mut self, structure: &pattern::Structure) {
        for argument in structure.arguments() {
            self.define_pattern_locals(argument.data());
        }
    }

    pub fn program(&mut self, program: &Program) {
        for module in program.modules() {
            self.module(module);
        }
    }

    pub fn module(&mut self, module: &Module) {
        for definition in module.definitions() {
            match definition {
                Definition::Name(name) => self.name_definition(name),
                Definition::Structure(_) => (),
                Definition::ModulePath(_) | Definition::Import(_) => (),
            }
        }
    }

    fn name_definition(&mut self, name_definition: &definition::Name) {
        self.expression(name_definition.expression());
    }
}
