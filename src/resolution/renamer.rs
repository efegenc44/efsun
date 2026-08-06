use std::fmt::Display;

use crate::{
    data_table::{DataTable, OptionalDataTable, PathUniqueNameDataId, UniqueNameDataId},
    location::Located,
    parse::{
        definition::{self, Definition, Module, Program},
        expression::{self, Expression},
        pattern::{self, Pattern},
    },
    resolution::{ResolutionData, bound::Bound, frame::CheckStack},
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
pub struct Renamer<'resolution_data> {
    /// Stack to keep track of unique names
    stack: CheckStack<UniqueName>,
    /// State for unique name generation
    unique_name_counter: usize,
    /// Resolution produced data
    resolution_data: &'resolution_data ResolutionData,
    /// Rename produced data
    data: RenameData,
}

impl<'resolution_data> Renamer<'resolution_data> {
    pub fn new(resolution_data: &'resolution_data ResolutionData) -> Self {
        Self {
            stack: CheckStack::new(),
            unique_name_counter: 0,
            resolution_data,
            data: RenameData::default(),
        }
    }

    fn unique_name(&mut self) -> UniqueName {
        let name = UniqueName(self.unique_name_counter);
        self.unique_name_counter += 1;
        name
    }

    fn expression(&mut self, expression: &Located<Expression>) {
        match &expression.data {
            Expression::String(_) => (),
            Expression::Path(path) => self.path(path),
            Expression::Application(application) => self.application(application),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::LetIn(letin) => self.letin(letin),
            Expression::MatchAs(matchas) => self.matchas(matchas),
        };
    }

    pub fn expression_repl(mut self, expression: &Located<Expression>) -> RenameData {
        self.expression(expression);

        self.data
    }

    fn path(&mut self, path: &expression::Path) {
        let bound = self.resolution_data.bounds.get(&path.bound_id);

        let unique_name = match bound {
            Bound::Local(id) => Some(self.stack.get_local(*id)),
            Bound::Capture(id) => Some(self.stack.get_capture(*id)),
            Bound::Absolute(_) => None,
        };

        if let Some(unique_name) = unique_name {
            self.data
                .path_unique_names
                .set(path.unique_name_id, unique_name);
        }
    }

    fn application(&mut self, application: &expression::Application) {
        self.expression(&application.function);
        self.expression(&application.argument);
    }

    fn lambda(&mut self, lambda: &expression::Lambda) {
        let unique_variable = self.unique_name();

        let capture = self.resolution_data.captures.get(&lambda.capture_id);
        self.stack.push_frame(capture.to_vec());
        self.stack.push_local(unique_variable);
        self.expression(&lambda.expression);
        self.stack.pop_local();
        self.stack.pop_frame();

        self.data
            .unique_names
            .set(lambda.unique_name_id, unique_variable);
    }

    fn letin(&mut self, letin: &expression::LetIn) {
        let unique_variable = self.unique_name();
        self.stack.push_local(unique_variable);
        self.expression(&letin.variable_expression);
        self.expression(&letin.return_expression);
        self.stack.pop_local();

        self.data
            .unique_names
            .set(letin.unique_name_id, unique_variable);
    }

    fn matchas(&mut self, matchas: &expression::MatchAs) {
        self.expression(&matchas.expression);

        for branch in &matchas.branches {
            self.match_branch(&branch.data);
        }
    }

    fn match_branch(&mut self, branch: &expression::Branch) {
        let len = self.stack.len();
        self.define_pattern_locals(&branch.pattern.data);
        self.expression(&branch.expression);
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

        self.data.unique_names.set(any.unique_name_id, unique_name);
    }

    fn structure_pattern(&mut self, structure: &pattern::Structure) {
        for argument in &structure.arguments {
            self.define_pattern_locals(&argument.data);
        }
    }

    pub fn program(mut self, program: &Program) -> RenameData {
        for module in &program.modules {
            self.module(module);
        }

        self.data
    }

    pub fn module(&mut self, module: &Module) {
        for definition in &module.definitions {
            match definition {
                Definition::Name(name) => self.name_definition(name),
                Definition::Structure(_) => (),
                Definition::ModulePath(_) | Definition::Import(_) => (),
            }
        }
    }

    fn name_definition(&mut self, name_definition: &definition::Name) {
        self.expression(&name_definition.expression);
    }
}

#[derive(Default)]
pub struct RenameData {
    pub unique_names: DataTable<UniqueNameDataId, UniqueName>,
    pub path_unique_names: OptionalDataTable<PathUniqueNameDataId, UniqueName>,
}
