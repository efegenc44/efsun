use std::{cell::RefCell, marker::PhantomData};

use crate::{
    parse::{definition::Definition, expression::Expression},
    interner::{InternId, Interner},
    resolution::{Resolved, Unresolved, Renamed, bound::{Bound, Capture, Path}}
};

pub enum ANFDefinition<State> {
    Name(NameDefinition<State>),
}

impl<T> ANFDefinition<T> {
    #[allow(unused)]
    pub fn print(&self, interner: &Interner, depth: usize) {
        let indent = depth*2;

        match self {
            ANFDefinition::Name(name) => {
                println!("{:indent$}Let:", "");
                println!("{:indent$}{}", "", interner.lookup(name.identifier), indent=indent + 2);
                name.expression.print(depth + 1, interner);
            },
        }
    }
}

pub struct NameDefinition<T> {
    identifier: InternId,
    expression: ANF<T>,
    path: Path
}

impl<T> NameDefinition<T> {
    pub fn new(identifier: InternId, expression: ANF<T>, path: Path) -> Self {
        Self { identifier, expression, path }
    }

    #[allow(unused)]
    pub fn identifier(&self) -> InternId {
        self.identifier
    }

    pub fn expression(&self) -> &ANF<T> {
        &self.expression
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}

impl NameDefinition<Unresolved> {
    pub fn destruct(self) -> (InternId, ANF<Unresolved>, Path) {
        (self.identifier, self.expression, self.path)
    }
}

#[derive(Clone)]
pub enum ANF<State> {
    Let(LetExpression<State>),
    Application(ApplicationExpression<State>),
    Atom(Atom<State>)
}

impl<T> ANF<T> {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        let indent = 2*depth;

        match self {
            ANF::Let(letin) => {
                print!("{:indent$}let {} = ", "", interner.lookup(letin.variable));
                letin.variable_expression.print(0, interner);
                println!(" in");
                letin.return_expression.print(depth + 1, interner);
                println!();
            }
            ,
            ANF::Application(application) => {
                print!("{:indent$}let {} = ", "", application.variable.display(interner));
                application.function.print(0, interner);
                application.argument.print(0, interner);
                println!(" in");
                application.expression.print(depth + 1, interner);
                println!();
            },
            ANF::Atom(atom) => {
                atom.print(depth, interner);
                println!();
            },
        }
    }
}

#[derive(Clone)]
pub struct LetExpression<T> {
    variable: InternId,
    variable_expression: Atom<T>,
    return_expression: Box<ANF<T>>
}

impl<T> LetExpression<T> {
    pub fn new(variable: InternId, variable_expression: Atom<T>, return_expression: ANF<T>) -> Self {
        Self { variable, variable_expression, return_expression: Box::new(return_expression) }
    }

    pub fn destruct(self) -> (InternId, Atom<T>, ANF<T>) {
        (self.variable, self.variable_expression, *self.return_expression)
    }
}

impl LetExpression<Resolved> {
    pub fn variable_expression(&self) -> &Atom<Resolved> {
        &self.variable_expression
    }

    pub fn return_expression(&self) -> &ANF<Resolved> {
        &self.return_expression
    }
}

#[derive(Clone)]
pub struct ApplicationExpression<T> {
    variable: ANFLocal,
    function: Atom<T>,
    argument: Atom<T>,
    expression: Box<ANF<T>>
}

impl<T> ApplicationExpression<T> {
    pub fn new(variable: ANFLocal, function: Atom<T>, argument: Atom<T>, expression: ANF<T>) -> Self {
        Self { variable, function, argument, expression: Box::new(expression) }
    }
}

impl ApplicationExpression<Unresolved> {
    pub fn destruct(self) -> (ANFLocal, Atom<Unresolved>, Atom<Unresolved>, ANF<Unresolved>) {
        (self.variable, self.function, self.argument, *self.expression)
    }
}

impl ApplicationExpression<Resolved> {
    pub fn function(&self) -> &Atom<Resolved> {
        &self.function
    }

    pub fn argument(&self) -> &Atom<Resolved> {
        &self.argument
    }

    pub fn expression(&self) -> &ANF<Resolved> {
        &self.expression
    }
}

#[derive(Clone)]
pub enum Atom<State> {
    String(InternId),
    Path(PathExpression<State>),
    Lambda(LambdaExpression<State>)
}

impl<T> Atom<T> {
    fn print(&self, depth: usize, interner: &Interner) {
        let indent = 2*depth;

        match self {
            Atom::String(id) => print!("{:indent$}\"{}\"", "", interner.lookup(*id)),
            Atom::Path(path) => {
                print!(
                    "{:indent$}{}{}", "",
                    path.path.display(interner),
                    if let Some(bound) = &path.bound { format!("#{}", bound.display(interner)) } else { "".to_string() }
                )
            },
            Atom::Lambda(lambda) => {
                print!("{:indent$}\\{} ", "", interner.lookup(lambda.variable));
                if let Some(captures) = &lambda.captures {
                    if !captures.is_empty() {
                        print!("{:?} ", captures);
                    }
                }
                lambda.expression.print(0, interner);
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ANFLocal {
    ANF(usize),
    Normal(InternId),
}

impl ANFLocal {
    fn display(&self, interner: &Interner) -> String {
        match self {
            Self::ANF(id) => format!("{{ANF{id}}}"),
            Self::Normal(id) => interner.lookup(*id).to_string(),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum ANFPath {
    ANF(usize),
    Normal(Vec<InternId>),
}

impl ANFPath {
    fn display(&self, interner: &Interner) -> String {
        match self {
            Self::ANF(id) => format!("{{ANF{id}}}"),
            Self::Normal(parts) => {
                parts
                    .iter()
                    .map(|id| interner.lookup(*id))
                    .collect::<Vec<_>>()
                    .join(".")
            },
        }
    }
}

#[derive(Clone)]
pub struct PathExpression<T> {
    path: ANFPath,
    bound: Option<Bound>,
    state: PhantomData<T>,
}

impl PathExpression<Unresolved> {
    fn new(path: ANFPath, bound: Bound) -> Self {
        Self { path, bound: Some(bound), state: PhantomData }
    }

    fn local(path: ANFPath) -> Self {
        Self { path, bound: None, state: PhantomData }
    }

    pub fn bound(&self) -> &Option<Bound> {
        &self.bound
    }

    pub fn resolve(self, bound: Bound) -> PathExpression<Resolved> {
        PathExpression {
            path: self.path,
            bound: Some(bound),
            state: PhantomData
        }
    }

    pub fn path(&self) -> &ANFPath {
        &self.path
    }
}

impl PathExpression<Resolved> {
    pub fn bound(&self) -> &Bound {
        self.bound.as_ref().unwrap()
    }
}

#[derive(Clone)]
pub struct LambdaExpression<T> {
    variable: InternId,
    expression: Box<ANF<T>>,
    captures: Option<Vec<Capture>>
}

impl LambdaExpression<Unresolved> {
    fn new(variable: InternId, expression: ANF<Unresolved>) -> Self {
        Self { variable, expression: Box::new(expression), captures: None }
    }

    pub fn destruct(self) -> (InternId, ANF<Unresolved>) {
        (self.variable, *self.expression)
    }
}

impl LambdaExpression<Resolved> {
    pub fn new(variable: InternId, expression: ANF<Resolved>, captures: Vec<Capture>) -> Self {
        Self { variable, expression: Box::new(expression), captures: Some(captures) }
    }

    pub fn expression(&self) -> &ANF<Resolved> {
        &self.expression
    }

    pub fn captures(&self) -> &[Capture] {
        self.captures.as_ref().unwrap()
    }
}

pub struct ANFTransformer {
    counter: RefCell<usize>,
}

impl ANFTransformer {
    pub fn new() -> Self {
        Self { counter: RefCell::new(0) }
    }

    fn new_local_id(&self) -> usize {
        let id = *self.counter.borrow();
        *self.counter.borrow_mut() += 1;
        id
    }

    pub fn program(&self, modules: Vec<Vec<Definition<Renamed>>>) -> Vec<Vec<ANFDefinition<Unresolved>>> {
        let mut anf = Vec::new();
        for module in modules {
            anf.push(self.module(module));
        }

        anf
    }

    pub fn module(&self, definitions: Vec<Definition<Renamed>>) -> Vec<ANFDefinition<Unresolved>> {
        let mut anf_definitions = Vec::new();

        for definition in definitions {
            match definition {
                Definition::Name(name) => {
                    let path = name.path().clone();
                    let (identifier, expression) = name.destruct();
                    let expression = self.transform(expression.destruct().0);
                    let definition = NameDefinition::new(*identifier.data(), expression, path);
                    anf_definitions.push(ANFDefinition::Name(definition))
                },
                Definition::Module(_) |
                Definition::Import(_) => ()
            }
        }

        anf_definitions
    }

    fn expression(&self, e: Expression<Renamed>, k: Box<dyn FnOnce(Atom<Unresolved>) -> ANF<Unresolved> + '_>) -> ANF<Unresolved> {
        match e {
            Expression::String(id) => k(Atom::String(id)),
            Expression::Path(path) => {
                let (parts, bound) = path.destruct();
                let (parts, _, _) = parts.destruct();

                let path_expression = match &bound {
                    Bound::Absolute(_) => PathExpression::new(ANFPath::Normal(parts), bound),
                    Bound::Local(_) |
                    Bound::Capture(_) => PathExpression::local(ANFPath::Normal(parts)),
                };

                k(Atom::Path(path_expression))
            },
            Expression::Application(application) => {
                let (function, argument) = application.destruct();

                let k1 = |argument: Atom<_>| {
                    let k2 = |function| {
                        let id = self.new_local_id();
                        let path = Atom::Path(PathExpression::local(ANFPath::ANF(id)));
                        let application = ApplicationExpression::new(ANFLocal::ANF(id), function, argument, k(path));
                        ANF::Application(application)
                    };

                    let (function, _, _) = function.destruct();
                    self.expression(function, Box::new(k2))
                };

                let (argument, _, _) = argument.destruct();
                self.expression(argument, Box::new(k1))
            },
            Expression::Lambda(lambda) => {
                let (variable, expression) = lambda.destruct();
                let (expression, _, _) = expression.destruct();

                let expression = self.transform(expression);
                k(Atom::Lambda(LambdaExpression::<Unresolved>::new(*variable.data(), expression)))
            },
            Expression::Let(letin) => {
                let (variable, variable_expression, return_expression) = letin.destruct();

                let k1 = |variable_expression| {
                    let return_expression = self.expression(return_expression.destruct().0, k);
                    ANF::Let(LetExpression::new(*variable.data(), variable_expression, return_expression))
                };

                self.expression(variable_expression.destruct().0, Box::new(k1))
            },
        }
    }

    pub fn transform(&self, e: Expression<Renamed>) -> ANF<Unresolved> {
        self.expression(e, Box::new(ANF::Atom))
    }
}

