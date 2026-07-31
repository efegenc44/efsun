use crate::{
    compilation::anf::{self, atom},
    metadata::TailCallMetadataId,
    parse::pattern::Pattern,
    resolution::renamer::UniqueName,
};

pub enum Expression {
    LetIn(LetIn),
    Application(Application),
    MatchAs(MatchAs),
    Join(Join),
    Jump(Jump),
    Atom(atom::Atom),
}

pub struct Application {
    pub variable: anf::Local,
    pub function: anf::Atom,
    pub arguments: Vec<anf::Atom>,
    pub tail_call_id: TailCallMetadataId,
    pub expression: Box<anf::Expression>,
}

pub struct LetIn {
    pub variable: UniqueName,
    pub variable_expression: anf::Atom,
    pub return_expression: Box<anf::Expression>,
}

pub struct MatchAs {
    pub expression: anf::Atom,
    pub branches: Vec<Branch>,
}

pub struct Branch {
    pub pattern: Pattern,
    pub expression: anf::Expression,
}

pub struct Join {
    pub label: usize,
    pub variable: anf::Local,
    pub join: Box<anf::Expression>,
    pub expression: Box<anf::Expression>,
}

pub struct Jump {
    pub to: usize,
    pub expression: anf::Atom,
}
