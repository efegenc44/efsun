use crate::{
    interner::{InternId, Interner},
    location::Located,
    parse::expression::Expression, resolution::{Unresolved, Resolved, bound::Path}
};

pub enum Definition<State> {
    Module(ModuleDefinition),
    Name(NameDefinition<State>)
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
        }
    }
}

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
}

impl NameDefinition<Unresolved> {
    pub fn new(identifier: Located<InternId>, expression: Located<Expression<Unresolved>>) -> Self {
        Self { identifier, expression, path: None }
    }

    pub fn destruct(self) -> (Located<InternId>, Located<Expression<Unresolved>>) {
        (self.identifier, self.expression)
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

