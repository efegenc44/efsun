use crate::{
    parse::{
        expression::{
            ApplicationExpression, Expression, PathExpression,
            LambdaExpression, LetExpression, MatchExpression, MatchBranch, Pattern,
            StructurePattern
        },
        definition::{Definition, NameDefinition}
    },
    interner::InternId,
    location::Located,
    resolution::{Resolved, Renamed, frame::CheckStack, bound::Bound},
};

pub struct Renamer {
    stack: CheckStack<InternId>,
    newname_counter: usize,
}

impl Renamer {
    pub fn new() -> Self {
        let mut stack = CheckStack::new();
        stack.push_frame(vec![]);

        Self {
            stack,
            newname_counter: 0,
        }
    }

    fn new_name(&mut self) -> InternId {
        let id = InternId::new(self.newname_counter);
        self.newname_counter += 1;
        id
    }

    pub fn expression(&mut self, expression: Located<Expression<Resolved>>) -> Located<Expression<Renamed>> {
        let (data, span) = expression.destruct();

        let expression = match data {
            Expression::String(id) => Expression::String(id),
            Expression::Path(path) => Expression::Path(self.path(path)),
            Expression::Application(application) => Expression::Application(self.application(application)),
            Expression::Lambda(lambda) => Expression::Lambda(self.lambda(lambda)),
            Expression::Let(letin) => Expression::Let(self.letin(letin)),
            Expression::Match(matchlet) => Expression::Match(self.matchlet(matchlet)),
        };

        Located::new(expression, span)
    }

    fn path(&mut self, path: PathExpression<Resolved>) -> PathExpression<Renamed> {
        match path.bound() {
            Bound::Local(id) => {
                let name = self.stack.get_local(*id);
                path.rename(name)
            },
            Bound::Capture(id) => {
                let name = self.stack.get_capture(*id);
                path.rename(name)
            }
            Bound::Absolute(_) => path.rename_absolute(),
        }
    }

    fn application(&mut self, application: ApplicationExpression<Resolved>) -> ApplicationExpression<Renamed> {
        let (function, argument) = application.destruct();

        let function = self.expression(function);
        let argument = self.expression(argument);

        ApplicationExpression::new(function, argument)
    }

    fn lambda(&mut self, lambda: LambdaExpression<Resolved>) -> LambdaExpression<Renamed> {
        let captures = lambda.captures().to_vec();
        let (variable, experssion) = lambda.destruct();

        let newname = self.new_name();

        self.stack.push_frame(captures.clone());
        self.stack.push_local(newname);
        let expression = self.expression(experssion);
        self.stack.pop_local();
        self.stack.pop_frame();

        LambdaExpression::<Renamed>::new(
            Located::new(newname, variable.span()),
            expression,
            captures
        )
    }

    fn letin(&mut self, letin: LetExpression<Resolved>) -> LetExpression<Renamed> {
        let (variable, variable_expression, return_expression) = letin.destruct();

        let variable_expression = self.expression(variable_expression);

        let new_name = self.new_name();

        self.stack.push_local(new_name);
        let return_expression = self.expression(return_expression);
        self.stack.pop_local();

        LetExpression::new(
            Located::new(new_name, variable.span()),
            variable_expression,
            return_expression
        )
    }

    fn matchlet(&mut self, matchlet: MatchExpression<Resolved>) -> MatchExpression<Renamed> {
        let (expression, branches) = matchlet.destruct();

        let expression = self.expression(expression);

        let mut renamed_branches = Vec::new();
        for branch in branches {
            let (branch, span) = branch.destruct();
            let branch = Located::new(self.match_branch(branch), span);
            renamed_branches.push(branch);
        }

        MatchExpression::new(expression, renamed_branches)
    }

    fn match_branch(&mut self, branch: MatchBranch<Resolved>) -> MatchBranch<Renamed> {
        let (pattern, expression) = branch.destruct();
        let (pattern, span) = pattern.destruct();

        let len = self.stack.len();
        let pattern = self.pattern(pattern);
        let expression = self.expression(expression);
        self.stack.truncate(len);

        MatchBranch::new(Located::new(pattern, span), expression)
    }

    fn pattern(&mut self, pattern: Pattern<Resolved>) -> Pattern<Renamed> {
        match pattern {
            Pattern::Any(_) => {
                let new_name = self.new_name();
                self.stack.push_local(new_name);
                Pattern::Any(new_name)
            },
            Pattern::String(id) => Pattern::String(id),
            Pattern::Structure(structure) => {
                let (parts, arguments, bound, order) = structure.destruct();

                let mut renamed_arguments = Vec::new();
                for argument in arguments {
                    let (argument, span) = argument.destruct();
                    renamed_arguments.push(Located::new(self.pattern(argument), span));
                }

                Pattern::Structure(StructurePattern::<Renamed>::new(parts, renamed_arguments, bound, order))
            },
        }
    }

    pub fn program(&mut self, modules: Vec<(Vec<Definition<Resolved>>, String)>) -> Vec<Vec<Definition<Renamed>>> {
        let mut renamed_modules = vec![];

        for (module, _) in modules {
            renamed_modules.push(self.module(module));
        }

        renamed_modules
    }

    pub fn module(&mut self, definitions: Vec<Definition<Resolved>>) -> Vec<Definition<Renamed>> {
        let mut renamed_definitions = vec![];

        for definition in definitions {
            match definition {
                Definition::Name(name) => {
                    let definition = Definition::Name(self.let_definition(name));
                    renamed_definitions.push(definition);
                },
                Definition::Structure(structure) => {
                    let definition = Definition::Structure(structure.renamed());
                    renamed_definitions.push(definition);
                },
                Definition::Module(_) |
                Definition::Import(_) => ()
            }
        }

        renamed_definitions
    }

    fn let_definition(&mut self, let_definition: NameDefinition<Resolved>) -> NameDefinition<Renamed> {
        let path = let_definition.path().clone();
        let (name, expression) = let_definition.destruct();

        let expression = self.expression(expression);

        NameDefinition::<Renamed>::new(name, expression, path)
    }
}
