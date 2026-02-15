use crate::{
    interner::{InternId, Interner},
    location::Located,
    parse::expression::Expression, resolution::Unresolved
};

pub enum Definition<State> {
    Module(ModuleDefinition),
    Name(NameDefinition<State>)
}

impl<T> Definition<T> {
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
}

impl<T> NameDefinition<T> {
    pub fn new(identifier: Located<InternId>, expression: Located<Expression<T>>) -> Self {
        Self { identifier, expression }
    }

    pub fn identifier(&self) -> Located<InternId> {
        self.identifier
    }
}

impl NameDefinition<Unresolved> {
    pub fn destruct(self) -> (Located<InternId>, Located<Expression<Unresolved>>) {
        (self.identifier, self.expression)
    }
}

