use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone)]
pub enum MonoType {
    Variable(usize),
    Arrow(ArrowType)
}

impl MonoType {
    pub fn substitute(self, table: &HashMap<usize, MonoType>) -> Self {
        match self {
            MonoType::Variable(id) => {
                match table.get(&id) {
                    Some(t) => t.clone().substitute(table),
                    None => self,
                }
            },
            MonoType::Arrow(arrow) => {
                let from = arrow.from.substitute(table);
                let to = arrow.to.substitute(table);
                let arrow = ArrowType::new(from, to);

                Self::Arrow(arrow)
            },
        }
    }

    pub fn includes(&self, variable: usize) -> bool {
        match self {
            MonoType::Variable(id) => *id == variable,
            MonoType::Arrow(arrow) => {
                arrow.from().includes(variable) || arrow.to().includes(variable)
            },
        }
    }
}

impl Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(id) => write!(f, "a{id}"),
            Self::Arrow(arrow) => write!(f, "({} -> {})", arrow.from(), arrow.to()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrowType {
    from: Box<MonoType>,
    to: Box<MonoType>
}

impl ArrowType {
    pub fn new(from: MonoType, to: MonoType) -> Self {
        ArrowType {
            from: Box::new(from),
            to: Box::new(to)
        }
    }

    pub fn from(&self) -> &MonoType {
        &self.from
    }

    pub fn to(&self) -> &MonoType {
        &self.to
    }
}
