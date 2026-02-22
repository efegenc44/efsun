use crate::{
    parse::{
        expression::{
            ApplicationExpression, Expression, PathExpression,
            LambdaExpression, LetExpression
        },
        definition::{Definition, NameDefinition}
    },
    interner::InternId,
    location::Located,
    resolution::{Resolved, Renamed, bound::{Bound, Capture}},
};

pub struct Renamer {
    frames: Vec<Frame>,
    newname_counter: usize,
}

impl Renamer {
    pub fn new() -> Self {
        Self {
            frames: vec![Frame::with_captures(vec![])],
            newname_counter: 0,
        }
    }

    fn newname(&mut self) -> InternId {
        let id = InternId::new(self.newname_counter);
        self.newname_counter += 1;
        id
    }

    fn current_frame(&self) -> &Frame {
        self.frames.last().unwrap()
    }

    fn push_frame(&mut self, captures: Vec<Capture>) {
        self.frames.push(Frame::with_captures(captures));
    }

    fn pop_frame(&mut self) {
        self.frames.pop().unwrap();
    }

    fn push_local(&mut self, local: InternId) {
        self.frames.last_mut().unwrap().locals.push(local);
    }

    fn pop_local(&mut self) {
        self.frames.last_mut().unwrap().locals.pop();
    }

    pub fn expression(&mut self, expression: Located<Expression<Resolved>>) -> Located<Expression<Renamed>> {
        let (data, start, end) = expression.destruct();

        let expression = match data {
            Expression::String(id) => Expression::String(id),
            Expression::Path(path) => Expression::Path(self.path(path)),
            Expression::Application(application) => Expression::Application(self.application(application)),
            Expression::Lambda(lambda) => Expression::Lambda(self.lambda(lambda)),
            Expression::Let(letin) => Expression::Let(self.letin(letin)),
        };

        Located::new(expression, start, end)
    }

    fn path(&mut self, path: PathExpression<Resolved>) -> PathExpression<Renamed> {
        match path.bound() {
            Bound::Local(id) => {
                let index = self.current_frame().locals.len() - 1 - id.value();
                let name = self.current_frame().locals[index].clone();

                path.rename(name)
            },
            Bound::Capture(capture) => {
                let capture = self.current_frame().captures[capture.value()];
                let name = self.get_capture(capture);

                path.rename(name)
            }
            Bound::Absolute(_) => path.rename_absolute(),
        }
    }

    fn get_capture(&mut self, capture: Capture) -> InternId {
        self.get_capture_in_frame(capture, 1)
    }

    fn get_capture_in_frame(&mut self, capture: Capture, frame_depth: usize) -> InternId {
        let index = self.frames.len() - 1 - frame_depth;

        match capture {
            Capture::Local(id) => {
                let local_index = self.frames[index].locals.len() - 1 - id.value();
                let t = self.frames[index].locals[local_index].clone();
                t
            },
            Capture::Outer(id) => {
                let capture = self.frames[index].captures[id.value()];
                self.get_capture_in_frame(capture, frame_depth + 1)
            },
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

        let newname = self.newname();

        self.push_frame(captures.clone());
        self.push_local(newname);
        let expression = self.expression(experssion);
        self.pop_local();
        self.pop_frame();

        LambdaExpression::<Renamed>::new(
            Located::new(newname, variable.start(), variable.end()),
            expression,
            captures
        )
    }

    fn letin(&mut self, letin: LetExpression<Resolved>) -> LetExpression<Renamed> {
        let (variable, vexpr, rexpr) = letin.destruct();

        let vexpr = self.expression(vexpr);

        let newname = self.newname();

        self.push_local(newname);
        let rexpr = self.expression(rexpr);
        self.pop_local();

        LetExpression::new(
            Located::new(newname, variable.start(), variable.end()),
            vexpr,
            rexpr
        )
    }

    pub fn program(&mut self, modules: Vec<Vec<Definition<Resolved>>>) -> Vec<Vec<Definition<Renamed>>> {
        let mut renamed_modules = vec![];

        for module in modules {
            renamed_modules.push(self.module(module));
        }

        renamed_modules
    }

    pub fn module(&mut self, definitions: Vec<Definition<Resolved>>) -> Vec<Definition<Renamed>> {
        let mut renamed_definitions = vec![];

        for definition in definitions {
            match definition {
                Definition::Module(_) => (),
                Definition::Name(name) => renamed_definitions.push(
                    Definition::Name(self.let_definition(name))
                ),
                Definition::Import(_) => (),
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

struct Frame {
    locals: Vec<InternId>,
    captures: Vec<Capture>,
}

impl Frame {
    pub fn with_captures(captures: Vec<Capture>) -> Self {
        Self {
            locals: Vec::new(),
            captures,
        }
    }
}
