use std::{cell::RefCell, fmt::Display, marker::PhantomData};

use crate::{
    interner::{InternId, Interner, WithInterner},
    parse::{
        definition::{self, Definition, Module, Program},
        expression::{self, Expression, Pattern},
    },
    resolution::{
        Renamed, Resolved, Unresolved,
        bound::{Bound, Capture, Path},
    },
};

pub type ANFModule<T> = Vec<ANFDefinition<T>>;
pub type ANFProgram<T> = Vec<ANFModule<T>>;

pub enum ANFDefinition<State> {
    Let(LetDefinition<State>),
    Structure(StructureDefinition),
}

impl<T> ANFDefinition<T> {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        let indent = depth * 2;

        match self {
            ANFDefinition::Let(name) => {
                println!("{:indent$}Let:", "");
                println!(
                    "{:indent$}{}",
                    "",
                    interner.lookup(&name.identifier),
                    indent = indent + 2
                );
                name.expression.print(depth + 1, interner);
            }
            ANFDefinition::Structure(_) => todo!(),
        }
    }
}

pub struct LetDefinition<T> {
    identifier: InternId,
    expression: ANF<T>,
    path: Path,
}

impl<T> LetDefinition<T> {
    pub fn new(identifier: InternId, expression: ANF<T>, path: Path) -> Self {
        Self {
            identifier,
            expression,
            path,
        }
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

impl LetDefinition<Unresolved> {
    pub fn destruct(self) -> (InternId, ANF<Unresolved>, Path) {
        (self.identifier, self.expression, self.path)
    }
}

pub struct StructureDefinition {
    constructors: Vec<(Path, usize)>,
}

impl StructureDefinition {
    pub fn new(constructors: Vec<(Path, usize)>) -> Self {
        Self { constructors }
    }

    pub fn constructors(&self) -> &[(Path, usize)] {
        &self.constructors
    }
}

#[allow(clippy::upper_case_acronyms)]
pub enum ANF<State> {
    Let(LetExpression<State>),
    Application(ApplicationExpression<State>),
    Match(MatchExpression<State>),
    Join(Join<State>),
    Jump(Jump<State>),
    Atom(Atom<State>),
}

impl<T> ANF<T> {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        let indent = 2 * depth;

        match self {
            ANF::Let(letin) => {
                print!("{:indent$}let {} = ", "", interner.lookup(&letin.variable));
                letin.variable_expression.print(depth, interner);
                println!(" in");
                letin.return_expression.print(depth + 1, interner);
                println!();
            }
            ANF::Application(application) => {
                print!(
                    "{:indent$}let {} = ",
                    "",
                    WithInterner::new(&application.variable, interner)
                );
                application.function.print(depth, interner);
                application.argument.print(depth, interner);
                println!(" in");
                application.expression.print(depth + 1, interner);
                println!();
            }
            ANF::Match(matchlet) => {
                println!("{:indent$}Match:", "");
                print!(
                    "{:indent$}let {} = ",
                    "",
                    WithInterner::new(&matchlet.variable, interner)
                );
                matchlet.variable_expression.print(depth, interner);
                println!(" in");
                for branch in &matchlet.branches {
                    branch.expression.print(depth + 1, interner);
                }
                println!();
            }
            ANF::Join(join) => {
                join.join.print(depth + 1, interner);
                println!("{:indent$}Join({}):", "", join.label);
                print!(
                    "{:indent$}let {} = ",
                    "",
                    WithInterner::new(&join.variable, interner)
                );
                println!(" in");
                join.expression.print(depth + 1, interner);
            }
            ANF::Jump(jump) => {
                println!("{:indent$}Jump: {}", "", jump.to);
                jump.expression.print(depth + 1, interner);
            }
            ANF::Atom(atom) => {
                atom.print(depth, interner);
                println!();
            }
        }
    }
}

pub struct LetExpression<T> {
    variable: InternId,
    variable_expression: Atom<T>,
    return_expression: Box<ANF<T>>,
}

impl<T> LetExpression<T> {
    pub fn new(
        variable: InternId,
        variable_expression: Atom<T>,
        return_expression: ANF<T>,
    ) -> Self {
        Self {
            variable,
            variable_expression,
            return_expression: Box::new(return_expression),
        }
    }

    pub fn destruct(self) -> (InternId, Atom<T>, ANF<T>) {
        (
            self.variable,
            self.variable_expression,
            *self.return_expression,
        )
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

pub struct ApplicationExpression<T> {
    variable: ANFLocal,
    function: Atom<T>,
    argument: Atom<T>,
    expression: Box<ANF<T>>,
}

impl<T> ApplicationExpression<T> {
    pub fn new(
        variable: ANFLocal,
        function: Atom<T>,
        argument: Atom<T>,
        expression: ANF<T>,
    ) -> Self {
        Self {
            variable,
            function,
            argument,
            expression: Box::new(expression),
        }
    }
}

impl ApplicationExpression<Unresolved> {
    pub fn destruct(
        self,
    ) -> (
        ANFLocal,
        Atom<Unresolved>,
        Atom<Unresolved>,
        ANF<Unresolved>,
    ) {
        (
            self.variable,
            self.function,
            self.argument,
            *self.expression,
        )
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

pub struct MatchExpression<T> {
    variable: ANFLocal,
    variable_expression: Atom<T>,
    branches: Vec<MatchBranch<T>>,
}

impl<T> MatchExpression<T> {
    pub fn new(
        variable: ANFLocal,
        variable_expression: Atom<T>,
        branches: Vec<MatchBranch<T>>,
    ) -> Self {
        Self {
            variable,
            variable_expression,
            branches,
        }
    }

    pub fn destruct(self) -> (ANFLocal, Atom<T>, Vec<MatchBranch<T>>) {
        (self.variable, self.variable_expression, self.branches)
    }

    pub fn variable_expression(&self) -> &Atom<T> {
        &self.variable_expression
    }

    pub fn branches(&self) -> &[MatchBranch<T>] {
        &self.branches
    }
}

pub struct MatchBranch<T> {
    pattern: Pattern<Renamed>,
    matched: Atom<T>,
    expression: ANF<T>,
}

impl<T> MatchBranch<T> {
    pub fn new(pattern: Pattern<Renamed>, matched: Atom<T>, expression: ANF<T>) -> Self {
        Self {
            pattern,
            matched,
            expression,
        }
    }

    pub fn destruct(self) -> (Pattern<Renamed>, Atom<T>, ANF<T>) {
        (self.pattern, self.matched, self.expression)
    }

    pub fn pattern(&self) -> &Pattern<Renamed> {
        &self.pattern
    }

    pub fn matched(&self) -> &Atom<T> {
        &self.matched
    }

    pub fn expression(&self) -> &ANF<T> {
        &self.expression
    }
}

pub struct Join<T> {
    label: usize,
    variable: ANFLocal,
    join: Box<ANF<T>>,
    expression: Box<ANF<T>>,
}

impl<T> Join<T> {
    pub fn new(label: usize, variable: ANFLocal, join: ANF<T>, expression: ANF<T>) -> Self {
        Self {
            label,
            variable,
            join: Box::new(join),
            expression: Box::new(expression),
        }
    }

    pub fn destruct(self) -> (usize, ANFLocal, ANF<T>, ANF<T>) {
        (self.label, self.variable, *self.join, *self.expression)
    }

    pub fn label(&self) -> usize {
        self.label
    }

    pub fn join(&self) -> &ANF<T> {
        &self.join
    }

    pub fn expression(&self) -> &ANF<T> {
        &self.expression
    }
}

pub struct Jump<T> {
    to: usize,
    expression: Atom<T>,
}

impl<T> Jump<T> {
    pub fn new(to: usize, expression: Atom<T>) -> Self {
        Self { to, expression }
    }

    pub fn destruct(self) -> (usize, Atom<T>) {
        (self.to, self.expression)
    }

    pub fn to(&self) -> usize {
        self.to
    }

    pub fn expression(&self) -> &Atom<T> {
        &self.expression
    }
}

pub enum Atom<State> {
    String(InternId),
    Path(PathExpression<State>),
    Lambda(LambdaExpression<State>),
}

impl<T> Atom<T> {
    fn print(&self, depth: usize, interner: &Interner) {
        let indent = 2 * depth;

        match self {
            Atom::String(id) => print!("{:indent$}\"{}\"", "", interner.lookup(id)),
            Atom::Path(path) => {
                print!(
                    "{:indent$}{}{}",
                    "",
                    WithInterner::new(&path.path, interner),
                    if let Some(bound) = &path.bound {
                        format!("#{}", WithInterner::new(bound, interner))
                    } else {
                        "".to_string()
                    }
                )
            }
            Atom::Lambda(lambda) => {
                print!("{:indent$}\\{} ", "", interner.lookup(&lambda.variable));
                if let Some(captures) = &lambda.captures
                    && !captures.is_empty()
                {
                    print!("[ ");
                    for capture in captures {
                        print!("{} ", capture);
                    }
                    print!("]");
                }
                lambda.expression.print(0, interner);
            }
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ANFLocal {
    ANF(usize),
    Normal(InternId),
}

impl<'interner> Display for WithInterner<'interner, &ANFLocal> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            ANFLocal::ANF(id) => write!(f, "<ANF{id}>"),
            ANFLocal::Normal(identifier) => write!(f, "{}", self.interner().lookup(identifier)),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum ANFPath {
    ANFLocal(usize),
    Normal(Vec<InternId>),
}

impl<'interner> Display for WithInterner<'interner, &ANFPath> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.data() {
            ANFPath::ANFLocal(id) => write!(f, "<ANF{id}>"),
            ANFPath::Normal(parts) => match parts.as_slice() {
                [] => unreachable!(),
                [identifier] => {
                    write!(f, "{}", self.interner().lookup(identifier))
                }
                [x, xs @ ..] => {
                    write!(f, "{}", self.interner().lookup(x))?;
                    for x in xs {
                        write!(f, ".{}", self.interner().lookup(x))?;
                    }

                    Ok(())
                }
            },
        }
    }
}

pub struct PathExpression<T> {
    path: ANFPath,
    bound: Option<Bound>,
    state: PhantomData<T>,
}

impl PathExpression<Unresolved> {
    fn new(path: ANFPath, bound: Bound) -> Self {
        Self {
            path,
            bound: Some(bound),
            state: PhantomData,
        }
    }

    fn local(path: ANFPath) -> Self {
        Self {
            path,
            bound: None,
            state: PhantomData,
        }
    }

    pub fn bound(&self) -> &Option<Bound> {
        &self.bound
    }

    pub fn resolve(self, bound: Bound) -> PathExpression<Resolved> {
        PathExpression {
            path: self.path,
            bound: Some(bound),
            state: PhantomData,
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

pub struct LambdaExpression<T> {
    variable: InternId,
    expression: Box<ANF<T>>,
    captures: Option<Vec<Capture>>,
}

impl LambdaExpression<Unresolved> {
    fn new(variable: InternId, expression: ANF<Unresolved>) -> Self {
        Self {
            variable,
            expression: Box::new(expression),
            captures: None,
        }
    }

    pub fn destruct(self) -> (InternId, ANF<Unresolved>) {
        (self.variable, *self.expression)
    }
}

impl LambdaExpression<Resolved> {
    pub fn new(variable: InternId, expression: ANF<Resolved>, captures: Vec<Capture>) -> Self {
        Self {
            variable,
            expression: Box::new(expression),
            captures: Some(captures),
        }
    }

    pub fn expression(&self) -> &ANF<Resolved> {
        &self.expression
    }

    pub fn captures(&self) -> &[Capture] {
        self.captures.as_ref().unwrap()
    }
}

type Continuation<'a> = Box<dyn FnOnce(Atom<Unresolved>) -> ANF<Unresolved> + 'a>;

pub struct ANFTransformer {
    counter: RefCell<usize>,
}

impl ANFTransformer {
    pub fn new() -> Self {
        Self {
            counter: RefCell::new(0),
        }
    }

    fn new_local_id(&self) -> usize {
        let id = *self.counter.borrow();
        *self.counter.borrow_mut() += 1;
        id
    }

    pub fn program(&self, program: Program<Renamed>) -> ANFProgram<Unresolved> {
        program
            .into_iter()
            .map(|module| self.module(module))
            .collect()
    }

    pub fn module(&self, module: Module<Renamed>) -> ANFModule<Unresolved> {
        let mut anf_module = Vec::new();

        for definition in module {
            match definition {
                Definition::Name(let_definition) => {
                    anf_module.push(self.let_definition(let_definition));
                }
                Definition::Structure(structure_definition) => {
                    anf_module.push(self.structure_definition(structure_definition));
                }
                _ => (),
            }
        }

        anf_module
    }

    fn let_definition(
        &self,
        let_definition: definition::LetDefinition<Renamed>,
    ) -> ANFDefinition<Unresolved> {
        let (identifier, expression, path) = let_definition.destruct();

        let expression = self.transform(expression.destruct().0);

        ANFDefinition::Let(LetDefinition::new(*identifier.data(), expression, path))
    }

    fn structure_definition(
        &self,
        structure_definition: definition::StructureDefinition<Renamed>,
    ) -> ANFDefinition<Unresolved> {
        let constructors = structure_definition
            .constructors()
            .iter()
            .map(|constructor| (constructor.path().clone(), constructor.arguments().len()))
            .collect();

        ANFDefinition::Structure(StructureDefinition::new(constructors))
    }

    fn expression(&self, expression: Expression<Renamed>, k: Continuation) -> ANF<Unresolved> {
        match expression {
            Expression::String(id) => k(Atom::String(id)),
            Expression::Path(path) => self.path(path, k),
            Expression::Application(application) => self.application(application, k),
            Expression::Lambda(lambda) => self.lambda(lambda, k),
            Expression::Let(letin) => self.letin(letin, k),
            Expression::Match(matc) => self.matc(matc, k),
        }
    }

    fn path(&self, path: expression::PathExpression<Renamed>, k: Continuation) -> ANF<Unresolved> {
        let (parts, bound) = path.destruct();

        let path_expression = match &bound {
            Bound::Absolute(_) => PathExpression::new(ANFPath::Normal(parts.into_data()), bound),
            Bound::Local(_) | Bound::Capture(_) => {
                PathExpression::local(ANFPath::Normal(parts.into_data()))
            }
        };

        k(Atom::Path(path_expression))
    }

    fn application(
        &self,
        application: expression::ApplicationExpression<Renamed>,
        k: Continuation,
    ) -> ANF<Unresolved> {
        let (function, argument) = application.destruct();

        self.expression(
            argument.into_data(),
            Box::new(|argument| {
                self.expression(
                    function.clone().into_data(),
                    Box::new(|function| {
                        let id = self.new_local_id();
                        let path = Atom::Path(PathExpression::local(ANFPath::ANFLocal(id)));
                        let application = ApplicationExpression::new(
                            ANFLocal::ANF(id),
                            function,
                            argument,
                            k(path),
                        );
                        ANF::Application(application)
                    }),
                )
            }),
        )
    }

    fn lambda(
        &self,
        lambda: expression::LambdaExpression<Renamed>,
        k: Continuation,
    ) -> ANF<Unresolved> {
        let (variable, expression) = lambda.destruct();

        k(Atom::Lambda(LambdaExpression::<Unresolved>::new(
            *variable.data(),
            self.transform(expression.into_data()),
        )))
    }

    fn letin(&self, letin: expression::LetExpression<Renamed>, k: Continuation) -> ANF<Unresolved> {
        let (variable, variable_expression, return_expression) = letin.destruct();

        self.expression(
            variable_expression.into_data(),
            Box::new(|variable_expression| {
                ANF::Let(LetExpression::new(
                    variable.into_data(),
                    variable_expression,
                    self.expression(return_expression.clone().into_data(), k),
                ))
            }),
        )
    }

    fn matc(&self, matc: expression::MatchExpression<Renamed>, k: Continuation) -> ANF<Unresolved> {
        let (expression, branches) = matc.destruct();

        self.expression(
            expression.into_data(),
            Box::new(|expression| {
                let join_label_id = self.new_local_id();

                let join_var_id = self.new_local_id();
                let join_var_path =
                    Atom::Path(PathExpression::local(ANFPath::ANFLocal(join_var_id)));

                let id = self.new_local_id();

                let mut branch_anfs = Vec::new();
                for branch in branches.clone() {
                    let (pattern, branch_expression) = branch.into_data().destruct();
                    let path = Atom::Path(PathExpression::local(ANFPath::ANFLocal(id)));

                    let k = |expression| ANF::Jump(Jump::new(join_label_id, expression));
                    let branch_anf = self.expression(branch_expression.into_data(), Box::new(k));
                    let branch = MatchBranch::new(pattern.into_data(), path, branch_anf);
                    branch_anfs.push(branch);
                }

                ANF::Join(Join::new(
                    join_label_id,
                    ANFLocal::ANF(join_var_id),
                    ANF::Match(MatchExpression::new(
                        ANFLocal::ANF(id),
                        expression,
                        branch_anfs,
                    )),
                    k(join_var_path),
                ))
            }),
        )
    }

    pub fn transform(&self, expression: Expression<Renamed>) -> ANF<Unresolved> {
        self.expression(expression, Box::new(ANF::Atom))
    }
}
