use crate::{
    interner::{InternId, Interner},
    location::Located,
    parse::expression::Expression, resolution::{Unresolved, Resolved, Renamed, bound::Path}
};

#[derive(Clone)]
pub enum Definition<State> {
    Module(ModuleDefinition),
    Name(NameDefinition<State>),
    Import(ImportDefinition)
}

impl<T> Definition<T> {
    #[allow(unused)]
    pub fn print(&self, interner: &Interner, depth: usize) {
        let indent = depth*2;

        match self {
            Definition::Module(module) => {
                let path_string = module.parts
                    .data()
                    .iter()
                    .map(|id| interner.lookup(*id))
                    .collect::<Vec<_>>()
                    .join(".");

                println!("{:indent$}module {}", "", path_string);
            },
            Definition::Name(name) => {
                println!("{:indent$}Let:", "");
                println!("{:indent$}{}", "", interner.lookup(*name.identifier.data()), indent=indent + 2);
                name.expression.data().print(interner, depth + 1);
            },
            Definition::Import(import) => {
                println!("{:indent$}Import:", "");
            }
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

#[derive(Clone)]
pub struct NameDefinition<T> {
    identifier: Located<InternId>,
    expression: Located<Expression<T>>,
    path: Option<Path>
}

impl<T> NameDefinition<T> {
    pub fn identifier(&self) -> Located<InternId> {
        self.identifier
    }

    pub fn expression(&self) -> &Located<Expression<T>> {
        &self.expression
    }

    pub fn destruct(self) -> (Located<InternId>, Located<Expression<T>>) {
        (self.identifier, self.expression)
    }
}

impl NameDefinition<Unresolved> {
    pub fn new(identifier: Located<InternId>, expression: Located<Expression<Unresolved>>) -> Self {
        Self { identifier, expression, path: None }
    }
}

impl NameDefinition<Resolved> {
    pub fn new(identifier: Located<InternId>, expression: Located<Expression<Resolved>>, path: Path) -> Self {
        Self { identifier, expression, path: Some(path) }
    }

    pub fn path(&self) -> &Path {
        self.path.as_ref().unwrap()
    }
}

impl NameDefinition<Renamed> {
    pub fn path(&self) -> &Path {
        self.path.as_ref().unwrap()
    }
}

impl NameDefinition<Renamed> {
    pub fn new(identifier: Located<InternId>, expression: Located<Expression<Renamed>>, path: Path) -> Self {
        Self { identifier, expression, path: Some(path) }
    }
}

#[derive(Clone)]
pub struct ImportDefinition {
    module_path: Vec<InternId>,
    name: Option<ImportName>
}

impl ImportDefinition {
    pub fn new(module_path: Vec<InternId>, name: Option<ImportName>) -> Self {
        Self { module_path, name }
    }

    pub fn module_path(&self) -> &[InternId] {
        &self.module_path
    }

    pub fn name(&self) -> Option<&ImportName> {
        self.name.as_ref()
    }
}

#[derive(Clone)]
pub enum ImportName {
    As(InternId),
    Import(Vec<ImportDefinition>)
}