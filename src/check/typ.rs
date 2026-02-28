use std::{collections::HashMap, fmt::Display};

use crate::resolution::bound::Path;

#[derive(Clone)]
pub enum Type {
    Mono(MonoType),
    Poly(Vec<usize>, MonoType)
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Mono(m) => write!(f, "{m}"),
            Type::Poly(variables, m) => {
                for variable in variables {
                    write!(f, "a{variable} ")?;
                }

                write!(f, "; {m}")
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum MonoType {
    Variable(usize),
    Arrow(ArrowType),
    Structure(StructureType),
    String,
}

impl MonoType {
    pub fn generalize(self) -> Type {
        let mut variables = Vec::new();
        self.gather_variables(&mut variables);

        Type::Poly(variables, self)
    }

    fn gather_variables(&self, variables: &mut Vec<usize>) {
        match self {
            Self::Variable(id) => {
                if !variables.contains(id) {
                    variables.push(*id);
                }
            },
            Self::Arrow(arrow) => {
                arrow.from().gather_variables(variables);
                arrow.to().gather_variables(variables);
            },
            Self::Structure(structure) => {
                for argument in structure.arguments() {
                    argument.gather_variables(variables);
                }
            },
            Self::String => (),
        }
    }

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
            MonoType::Structure(structure) => {
                let arguments = structure
                    .arguments
                    .into_iter()
                    .map(|argument| argument.substitute(table))
                    .collect();

                let structure = StructureType::new(structure.path, arguments);
                Self::Structure(structure)
            },
            Self::String => self,
        }
    }

    pub fn includes(&self, variable: usize) -> bool {
        match self {
            Self::Variable(id) => *id == variable,
            Self::Arrow(arrow) => {
                arrow.from().includes(variable) || arrow.to().includes(variable)
            },
            Self::Structure(structure) => {
                structure
                    .arguments()
                    .iter()
                    .any(|argument| argument.includes(variable))
            },
            Self::String => false,
        }
    }
}

impl Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(id) => write!(f, "a{id}"),
            Self::Arrow(arrow) => write!(f, "({} -> {})", arrow.from(), arrow.to()),
            Self::Structure(structure) => {
                let mut string = String::from("{ ");
                for argument in structure.arguments().iter() {
                    string.push_str(&argument.to_string());
                }
                string.push_str(" }");
                write!(f, "{string}")
            },
            Self::String => write!(f, "String"),
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

#[derive(Debug, Clone)]
pub struct StructureType {
    path: Path,
    arguments: Vec<MonoType>
}

impl StructureType {
    pub fn new(path: Path, arguments: Vec<MonoType>) -> Self {
        Self { path, arguments }
    }

    pub fn arguments(&self) -> &[MonoType] {
        &self.arguments
    }

    pub fn path(&self) -> &Path {
        &self.path
    }
}