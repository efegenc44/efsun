use crate::{
    interner::{InternId, Interner},
    location::Located,
    parse::{expression::Expression, type_expression::TypeExpression},
    resolution::{Renamed, Resolved, Unresolved, bound::Path},
};

pub enum Definition<State> {
    Module(ModuleDefinition),
    Name(LetDefinition<State>),
    Import(ImportDefinition),
    Structure(StructureDefinition<State>),
}

impl<T> Definition<T> {
    #[allow(unused)]
    pub fn print(&self, interner: &Interner, depth: usize) {
        let indent = depth * 2;

        match self {
            Definition::Module(module) => {
                let path_string = module
                    .parts
                    .data()
                    .iter()
                    .map(|id| interner.lookup(id))
                    .collect::<Vec<_>>()
                    .join(".");

                println!("{:indent$}module {}", "", path_string);
            }
            Definition::Name(name) => {
                println!("{:indent$}Let:", "");
                println!(
                    "{:indent$}{}",
                    "",
                    interner.lookup(name.identifier.data()),
                    indent = indent + 2
                );
                name.expression.data().print(depth + 1, interner);
            }
            Definition::Import(import) => {
                println!("{:indent$}Import:", "");
            }
            Definition::Structure(_) => todo!(),
        }
    }
}

#[derive(Clone)]
pub struct ModuleDefinition {
    parts: Located<Vec<InternId>>,
}

impl ModuleDefinition {
    pub fn new(parts: Located<Vec<InternId>>) -> Self {
        Self { parts }
    }

    pub fn parts(&self) -> &Located<Vec<InternId>> {
        &self.parts
    }
}

pub struct LetDefinition<T> {
    identifier: Located<InternId>,
    expression: Located<Expression<T>>,
    path: Option<Path>,
}

impl<T> LetDefinition<T> {
    pub fn identifier(&self) -> Located<InternId> {
        self.identifier
    }

    pub fn expression(&self) -> &Located<Expression<T>> {
        &self.expression
    }
}

impl LetDefinition<Unresolved> {
    pub fn new(identifier: Located<InternId>, expression: Located<Expression<Unresolved>>) -> Self {
        Self {
            identifier,
            expression,
            path: None,
        }
    }

    pub fn destruct(self) -> (Located<InternId>, Located<Expression<Unresolved>>) {
        (self.identifier, self.expression)
    }
}

impl LetDefinition<Resolved> {
    pub fn new(
        identifier: Located<InternId>,
        expression: Located<Expression<Resolved>>,
        path: Path,
    ) -> Self {
        Self {
            identifier,
            expression,
            path: Some(path),
        }
    }

    pub fn path(&self) -> &Path {
        self.path.as_ref().unwrap()
    }

    pub fn destruct(self) -> (Located<InternId>, Located<Expression<Resolved>>, Path) {
        (self.identifier, self.expression, self.path.unwrap())
    }
}

impl LetDefinition<Renamed> {
    pub fn new(
        identifier: Located<InternId>,
        expression: Located<Expression<Renamed>>,
        path: Path,
    ) -> Self {
        Self {
            identifier,
            expression,
            path: Some(path),
        }
    }

    pub fn destruct(self) -> (Located<InternId>, Located<Expression<Renamed>>, Path) {
        (self.identifier, self.expression, self.path.unwrap())
    }
}

#[derive(Clone)]
pub struct ImportDefinition {
    module_path: Located<Vec<InternId>>,
    name: Option<ImportName>,
}

impl ImportDefinition {
    pub fn new(module_path: Located<Vec<InternId>>, name: Option<ImportName>) -> Self {
        Self { module_path, name }
    }

    pub fn module_path(&self) -> &Located<Vec<InternId>> {
        &self.module_path
    }

    pub fn name(&self) -> Option<&ImportName> {
        self.name.as_ref()
    }
}

#[derive(Clone)]
pub enum ImportName {
    As(InternId),
    Import(Vec<ImportDefinition>),
}

#[derive(Clone)]
pub struct StructureDefinition<T> {
    name: Located<InternId>,
    variables: Vec<Located<InternId>>,
    constructors: Vec<Located<Constructor<T>>>,
    path: Option<Path>,
}

impl<T> StructureDefinition<T> {
    pub fn name(&self) -> Located<InternId> {
        self.name
    }

    pub fn variables(&self) -> &[Located<InternId>] {
        &self.variables
    }

    pub fn constructors(&self) -> &[Located<Constructor<T>>] {
        &self.constructors
    }
}

impl StructureDefinition<Unresolved> {
    pub fn new(
        name: Located<InternId>,
        variables: Vec<Located<InternId>>,
        constructors: Vec<Located<Constructor<Unresolved>>>,
    ) -> Self {
        Self {
            name,
            variables,
            constructors,
            path: None,
        }
    }

    pub fn destruct(
        self,
    ) -> (
        Located<InternId>,
        Vec<Located<InternId>>,
        Vec<Located<Constructor<Unresolved>>>,
    ) {
        (self.name, self.variables, self.constructors)
    }
}

impl StructureDefinition<Resolved> {
    pub fn new(
        name: Located<InternId>,
        variables: Vec<Located<InternId>>,
        constructors: Vec<Located<Constructor<Resolved>>>,
        path: Path,
    ) -> Self {
        Self {
            name,
            variables,
            constructors,
            path: Some(path),
        }
    }

    pub fn path(&self) -> &Path {
        self.path.as_ref().unwrap()
    }

    pub fn renamed(self) -> StructureDefinition<Renamed> {
        let constructors = self
            .constructors
            .into_iter()
            .map(|argument| {
                let span = argument.span();
                Located::new(argument.into_data().renamed(), span)
            })
            .collect();

        StructureDefinition {
            name: self.name,
            variables: self.variables,
            constructors,
            path: self.path,
        }
    }
}

#[derive(Clone)]
pub struct Constructor<T> {
    name: Located<InternId>,
    arguments: Vec<Located<TypeExpression<T>>>,
    path: Option<Path>,
}

impl<T> Constructor<T> {
    pub fn name(&self) -> Located<InternId> {
        self.name
    }

    pub fn arguments(&self) -> &[Located<TypeExpression<T>>] {
        &self.arguments
    }
}

impl Constructor<Unresolved> {
    pub fn new(
        name: Located<InternId>,
        arguments: Vec<Located<TypeExpression<Unresolved>>>,
    ) -> Self {
        Self {
            name,
            arguments,
            path: None,
        }
    }

    pub fn destruct(self) -> (Located<InternId>, Vec<Located<TypeExpression<Unresolved>>>) {
        (self.name, self.arguments)
    }
}

impl Constructor<Resolved> {
    pub fn new(
        name: Located<InternId>,
        arguments: Vec<Located<TypeExpression<Resolved>>>,
        path: Path,
    ) -> Self {
        Self {
            name,
            arguments,
            path: Some(path),
        }
    }

    pub fn path(&self) -> &Path {
        self.path.as_ref().unwrap()
    }

    pub fn renamed(self) -> Constructor<Renamed> {
        Constructor {
            name: self.name,
            arguments: self
                .arguments
                .into_iter()
                .map(|argument| argument.renamed())
                .collect(),
            path: self.path,
        }
    }
}

impl Constructor<Renamed> {
    pub fn path(&self) -> &Path {
        self.path.as_ref().unwrap()
    }
}

pub type Module<T> = Vec<Definition<T>>;
pub type Program<T> = Vec<Module<T>>;
