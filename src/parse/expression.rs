use std::marker::PhantomData;

use crate::{
    error::{Result, located_error},
    interner::{InternId, Interner, WithInterner},
    location::{Located, Span},
    resolution::{
        ExpressionResolver, Renamed, ResolutionError, Resolved, Unresolved,
        bound::{Bound, Capture, Path},
    },
};

#[derive(Clone)]
pub enum Expression<State> {
    String(InternId),
    Path(PathExpression<State>),
    Application(ApplicationExpression<State>),
    Lambda(LambdaExpression<State>),
    Let(LetExpression<State>),
    Match(MatchExpression<State>),
}

impl Located<Expression<Unresolved>> {
    pub fn resolve(
        self,
        resolver: &mut ExpressionResolver,
    ) -> Result<Located<Expression<Resolved>>> {
        let span = self.span();

        self.map_result(|expression| {
            let expression = match expression {
                Expression::String(string) => Expression::String(string),
                Expression::Path(path) => Expression::Path(path.resolve(resolver, span)?),
                Expression::Application(application) => {
                    Expression::Application(application.resolve(resolver)?)
                }
                Expression::Lambda(lambda) => Expression::Lambda(lambda.resolve(resolver)?),
                Expression::Let(letin) => Expression::Let(letin.resolve(resolver)?),
                Expression::Match(matc) => Expression::Match(matc.resolve(resolver)?),
            };

            Ok(expression)
        })
    }
}

impl<T> Expression<T> {
    pub fn print(&self, depth: usize, interner: &Interner) {
        let indent = depth * 2;

        match self {
            Self::String(string) => {
                println!("{:indent$}\"{}\"", "", interner.lookup(string));
            }
            Self::Path(path) => {
                let path_string = path
                    .parts()
                    .data()
                    .iter()
                    .map(|id| interner.lookup(id))
                    .collect::<Vec<_>>()
                    .join(".");

                println!(
                    "{:indent$}Identifier: {}{}",
                    "",
                    path_string,
                    if let Some(bound) = &path.bound {
                        format!("#{}", WithInterner::new(bound, interner))
                    } else {
                        "".to_string()
                    }
                )
            }
            Self::Lambda(lambda) => {
                println!("{:indent$}Lambda:", "");
                if let Some(captures) = &lambda.captures
                    && !captures.is_empty()
                {
                    print!("{:indent$}Captures: ", "", indent = indent + 2);
                    for capture in captures {
                        print!("{} ", capture);
                    }
                    println!()
                }
                println!(
                    "{:indent$}{}",
                    "",
                    interner.lookup(lambda.variable.data()),
                    indent = indent + 2
                );
                lambda.expression.data().print(depth + 1, interner);
            }
            Self::Application(application) => {
                println!("{:indent$}Application:", "");
                application.function.data().print(depth + 1, interner);
                application.argument.data().print(depth + 1, interner);
            }
            Self::Let(letin) => {
                println!("{:indent$}Let:", "");
                println!(
                    "{:indent$}{}",
                    "",
                    interner.lookup(letin.variable.data()),
                    indent = indent + 2
                );
                letin.variable_expression.data().print(depth + 2, interner);
                letin.return_expression.data().print(depth + 1, interner);
            }
            Self::Match(_) => todo!(),
        }
    }
}

#[derive(Clone)]
pub struct PathExpression<State> {
    parts: Located<Vec<InternId>>,
    bound: Option<Bound>,
    state: PhantomData<State>,
}

impl<T> PathExpression<T> {
    pub fn parts(&self) -> &Located<Vec<InternId>> {
        &self.parts
    }
}

impl PathExpression<Unresolved> {
    pub fn new(parts: Located<Vec<InternId>>) -> Self {
        Self {
            parts,
            bound: Option::None,
            state: PhantomData,
        }
    }

    pub fn resolve(
        self,
        resolver: &mut ExpressionResolver,
        span: Span,
    ) -> Result<PathExpression<Resolved>> {
        let bound = match self.parts.data().as_slice() {
            [] => unreachable!(),
            [identifier] => resolver.identifier(*identifier, span)?,
            [base, rest @ ..] => {
                let path = resolver.base_path(base).append_parts(rest.to_vec());

                let true = resolver.names().contains(&path) else {
                    let error = ResolutionError::UnboundPath(path);
                    return Err(located_error(
                        error,
                        span,
                        resolver.current_module().source_name().to_string(),
                    ));
                };

                Bound::Absolute(path)
            }
        };

        Ok(PathExpression {
            parts: self.parts,
            bound: Some(bound),
            state: PhantomData,
        })
    }
}

impl PathExpression<Resolved> {
    pub fn bound(&self) -> &Bound {
        self.bound.as_ref().unwrap()
    }

    pub fn rename(self, name: InternId) -> PathExpression<Renamed> {
        let (mut data, span) = self.parts.destruct();

        *data.last_mut().unwrap() = name;

        PathExpression {
            parts: Located::new(data, span),
            bound: self.bound,
            state: PhantomData,
        }
    }

    pub fn rename_absolute(self) -> PathExpression<Renamed> {
        PathExpression {
            parts: self.parts,
            bound: self.bound,
            state: PhantomData,
        }
    }
}

impl PathExpression<Renamed> {
    pub fn destruct(self) -> (Located<Vec<InternId>>, Bound) {
        (self.parts, self.bound.unwrap())
    }
}

#[derive(Clone)]
pub struct ApplicationExpression<T> {
    function: Box<Located<Expression<T>>>,
    argument: Box<Located<Expression<T>>>,
}

impl ApplicationExpression<Unresolved> {
    pub fn resolve(
        self,
        resolver: &mut ExpressionResolver,
    ) -> Result<ApplicationExpression<Resolved>> {
        let function = self.function.resolve(resolver)?;
        let argument = self.argument.resolve(resolver)?;

        Ok(ApplicationExpression::new(function, argument))
    }
}

impl<T> ApplicationExpression<T> {
    pub fn new(function: Located<Expression<T>>, argument: Located<Expression<T>>) -> Self {
        Self {
            function: Box::new(function),
            argument: Box::new(argument),
        }
    }

    pub fn destruct(self) -> (Located<Expression<T>>, Located<Expression<T>>) {
        (*self.function, *self.argument)
    }

    pub fn function(&self) -> &Located<Expression<T>> {
        &self.function
    }

    pub fn argument(&self) -> &Located<Expression<T>> {
        &self.argument
    }
}

#[derive(Clone)]
pub struct LambdaExpression<T> {
    variable: Located<InternId>,
    expression: Box<Located<Expression<T>>>,
    captures: Option<Vec<Capture>>,
}

impl<T> LambdaExpression<T> {
    #[allow(unused)]
    pub fn variable(&self) -> Located<InternId> {
        self.variable
    }

    pub fn expression(&self) -> &Located<Expression<T>> {
        &self.expression
    }

    pub fn destruct(self) -> (Located<InternId>, Located<Expression<T>>) {
        (self.variable, *self.expression)
    }
}

impl LambdaExpression<Unresolved> {
    pub fn new(variable: Located<InternId>, expression: Located<Expression<Unresolved>>) -> Self {
        Self {
            variable,
            expression: Box::new(expression),
            captures: None,
        }
    }

    pub fn resolve(self, resolver: &mut ExpressionResolver) -> Result<LambdaExpression<Resolved>> {
        resolver.stack_mut().push_frame();
        resolver.stack_mut().push_local(self.variable.into_data());
        let expression = self.expression.resolve(resolver)?;
        resolver.stack_mut().pop_local();
        let captures = resolver.stack_mut().pop_frame();

        Ok(LambdaExpression {
            variable: self.variable,
            expression: Box::new(expression),
            captures: Some(captures),
        })
    }
}

impl LambdaExpression<Resolved> {
    pub fn captures(&self) -> &[Capture] {
        self.captures.as_ref().unwrap()
    }
}

impl LambdaExpression<Renamed> {
    pub fn new(
        variable: Located<InternId>,
        expression: Located<Expression<Renamed>>,
        captures: Vec<Capture>,
    ) -> Self {
        Self {
            variable,
            expression: Box::new(expression),
            captures: Some(captures),
        }
    }
}

#[derive(Clone)]
pub struct LetExpression<T> {
    variable: Located<InternId>,
    variable_expression: Box<Located<Expression<T>>>,
    return_expression: Box<Located<Expression<T>>>,
}

impl LetExpression<Unresolved> {
    pub fn resolve(self, resolver: &mut ExpressionResolver) -> Result<LetExpression<Resolved>> {
        let variable_expression = self.variable_expression.resolve(resolver)?;
        resolver.stack_mut().push_local(self.variable.into_data());
        let return_expression = self.return_expression.resolve(resolver)?;
        resolver.stack_mut().pop_local();

        Ok(LetExpression {
            variable: self.variable,
            variable_expression: Box::new(variable_expression),
            return_expression: Box::new(return_expression),
        })
    }
}

impl<T> LetExpression<T> {
    pub fn new(
        variable: Located<InternId>,
        variable_expression: Located<Expression<T>>,
        return_expression: Located<Expression<T>>,
    ) -> Self {
        Self {
            variable,
            variable_expression: Box::new(variable_expression),
            return_expression: Box::new(return_expression),
        }
    }

    #[allow(unused)]
    pub fn variable(&self) -> Located<InternId> {
        self.variable
    }

    pub fn variable_expression(&self) -> &Located<Expression<T>> {
        &self.variable_expression
    }

    pub fn return_expression(&self) -> &Located<Expression<T>> {
        &self.return_expression
    }

    pub fn destruct(
        self,
    ) -> (
        Located<InternId>,
        Located<Expression<T>>,
        Located<Expression<T>>,
    ) {
        (
            self.variable,
            *self.variable_expression,
            *self.return_expression,
        )
    }
}

#[derive(Clone)]
pub struct MatchExpression<T> {
    expression: Box<Located<Expression<T>>>,
    branches: Vec<Located<MatchBranch<T>>>,
}

impl MatchExpression<Unresolved> {
    pub fn resolve(self, resolver: &mut ExpressionResolver) -> Result<MatchExpression<Resolved>> {
        let expression = self.expression.resolve(resolver)?;

        let mut resolved_branches = Vec::new();
        for branch in self.branches {
            let branch = branch.map_result(|branch| branch.resolve(resolver))?;
            resolved_branches.push(branch);
        }

        Ok(MatchExpression::new(expression, resolved_branches))
    }
}

impl<T> MatchExpression<T> {
    pub fn new(expression: Located<Expression<T>>, branches: Vec<Located<MatchBranch<T>>>) -> Self {
        Self {
            expression: Box::new(expression),
            branches,
        }
    }

    pub fn destruct(self) -> (Located<Expression<T>>, Vec<Located<MatchBranch<T>>>) {
        (*self.expression, self.branches)
    }

    pub fn expression(&self) -> &Located<Expression<T>> {
        &self.expression
    }

    pub fn branches(&self) -> &[Located<MatchBranch<T>>] {
        &self.branches
    }
}

#[derive(Clone)]
pub struct MatchBranch<T> {
    pattern: Located<Pattern<T>>,
    expression: Located<Expression<T>>,
}

impl MatchBranch<Unresolved> {
    pub fn resolve(self, resolver: &mut ExpressionResolver) -> Result<MatchBranch<Resolved>> {
        let len = resolver.stack_mut().len();
        let span = self.pattern.span();
        let pattern = self
            .pattern
            .map_result(|pattern| resolver.define_pattern_locals(pattern, span))?;
        let expression = self.expression.resolve(resolver)?;
        resolver.stack_mut().truncate(len);

        Ok(MatchBranch {
            pattern,
            expression,
        })
    }
}

impl<T> MatchBranch<T> {
    pub fn new(pattern: Located<Pattern<T>>, expression: Located<Expression<T>>) -> Self {
        Self {
            pattern,
            expression,
        }
    }

    pub fn destruct(self) -> (Located<Pattern<T>>, Located<Expression<T>>) {
        (self.pattern, self.expression)
    }

    pub fn pattern(&self) -> &Located<Pattern<T>> {
        &self.pattern
    }

    pub fn expression(&self) -> &Located<Expression<T>> {
        &self.expression
    }
}

#[derive(Clone)]
pub enum Pattern<State> {
    Any(InternId),
    Structure(StructurePattern<State>),
    String(InternId),
}

#[derive(Clone)]
pub struct StructurePattern<State> {
    parts: Located<Vec<InternId>>,
    arguments: Vec<Located<Pattern<State>>>,
    type_path: Option<Path>,
    constructor_name: Option<InternId>,
    order: Option<usize>,
    state: PhantomData<State>,
}

impl<T> StructurePattern<T> {
    pub fn arguments(&self) -> &[Located<Pattern<T>>] {
        &self.arguments
    }
}

impl StructurePattern<Unresolved> {
    pub fn new(
        parts: Located<Vec<InternId>>,
        arguments: Vec<Located<Pattern<Unresolved>>>,
    ) -> Self {
        Self {
            parts,
            arguments,
            type_path: None,
            constructor_name: None,
            order: None,
            state: PhantomData,
        }
    }

    pub fn destruct(self) -> (Located<Vec<InternId>>, Vec<Located<Pattern<Unresolved>>>) {
        (self.parts, self.arguments)
    }
}

impl StructurePattern<Resolved> {
    pub fn new(
        parts: Located<Vec<InternId>>,
        arguments: Vec<Located<Pattern<Resolved>>>,
        type_path: Path,
        constructor_name: InternId,
        order: usize,
    ) -> Self {
        Self {
            parts,
            arguments,
            type_path: Some(type_path),
            constructor_name: Some(constructor_name),
            order: Some(order),
            state: PhantomData,
        }
    }

    pub fn destruct(
        self,
    ) -> (
        Located<Vec<InternId>>,
        Vec<Located<Pattern<Resolved>>>,
        Path,
        InternId,
        usize,
    ) {
        (
            self.parts,
            self.arguments,
            self.type_path.unwrap(),
            *self.constructor_name.as_ref().unwrap(),
            self.order.unwrap(),
        )
    }

    pub fn type_path(&self) -> &Path {
        self.type_path.as_ref().unwrap()
    }

    pub fn constructor_name(&self) -> InternId {
        *self.constructor_name.as_ref().unwrap()
    }
}

impl StructurePattern<Renamed> {
    pub fn new(
        parts: Located<Vec<InternId>>,
        arguments: Vec<Located<Pattern<Renamed>>>,
        type_path: Path,
        constructor_name: InternId,
        order: usize,
    ) -> Self {
        Self {
            parts,
            arguments,
            type_path: Some(type_path),
            constructor_name: Some(constructor_name),
            order: Some(order),
            state: PhantomData,
        }
    }

    pub fn order(&self) -> usize {
        self.order.unwrap()
    }
}
