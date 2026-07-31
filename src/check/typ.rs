use std::{collections::HashMap, fmt::Display};

use crate::{interner::WithInterner, resolution::bound::Path};

#[derive(Clone, Debug)]
pub enum Type {
    Mono(MonoType),
    Poly(Vec<usize>, MonoType),
}

impl<'interner> Display for WithInterner<'interner, &Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;

        match &self.data {
            Type::Mono(m) => {
                let m = WithInterner { data: m, interner };
                write!(f, "{}", m)
            }
            Type::Poly(variables, m) => {
                // TODO: Print type variables with more care (greek letters?)
                if !variables.is_empty() {
                    write!(f, "∀")?;
                    match variables.as_slice() {
                        [] => unreachable!(),
                        [x, xs @ ..] => {
                            write!(f, "a{x}")?;
                            for x in xs {
                                write!(f, ", a{x}")?;
                            }
                        }
                    }
                    write!(f, " ")?;
                }
                let m = WithInterner { data: m, interner };
                write!(f, "{}", m)
            }
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
        Type::Poly(self.variables(), self)
    }

    pub fn variables(&self) -> Vec<usize> {
        let mut variables = Vec::new();
        self.gather_variables(&mut variables);
        variables
    }

    fn gather_variables(&self, variables: &mut Vec<usize>) {
        match self {
            Self::Variable(id) => {
                if !variables.contains(id) {
                    variables.push(*id);
                }
            }
            Self::Arrow(arrow) => {
                arrow.from.gather_variables(variables);
                arrow.to.gather_variables(variables);
            }
            Self::Structure(structure) => {
                for argument in &structure.arguments {
                    argument.gather_variables(variables);
                }
            }
            Self::String => (),
        }
    }

    pub fn substitute(self, table: &HashMap<usize, MonoType>) -> Self {
        match self {
            Self::Variable(id) => match table.get(&id) {
                Some(t) => t.clone().substitute(table),
                None => self,
            },
            Self::Arrow(arrow) => {
                let from = arrow.from.substitute(table);
                let to = arrow.to.substitute(table);
                let arrow = ArrowType {
                    from: Box::new(from),
                    to: Box::new(to),
                };

                Self::Arrow(arrow)
            }
            Self::Structure(structure) => {
                let arguments = structure
                    .arguments
                    .into_iter()
                    .map(|argument| argument.substitute(table))
                    .collect();

                let structure = StructureType {
                    path: structure.path,
                    arguments,
                };
                Self::Structure(structure)
            }
            Self::String => self,
        }
    }

    pub fn includes(&self, variable: usize) -> bool {
        match self {
            Self::Variable(id) => *id == variable,
            Self::Arrow(arrow) => arrow.from.includes(variable) || arrow.to.includes(variable),
            Self::Structure(structure) => structure
                .arguments
                .iter()
                .any(|argument| argument.includes(variable)),
            Self::String => false,
        }
    }
}

impl<'interner> Display for WithInterner<'interner, &MonoType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = self.interner;

        match &self.data {
            MonoType::Variable(id) => write!(f, "a{id}"),
            MonoType::Arrow(arrow) => {
                let from = WithInterner {
                    data: arrow.from.as_ref(),
                    interner,
                };

                if let MonoType::Arrow(_) = arrow.from.as_ref() {
                    write!(f, "({})", from)?;
                } else {
                    write!(f, "{}", from)?;
                }

                let to = WithInterner {
                    data: arrow.to.as_ref(),
                    interner,
                };

                write!(f, " -> {}", to)
            }
            MonoType::Structure(structure) => {
                let path = WithInterner {
                    data: &structure.path,
                    interner,
                };

                write!(f, "{}", path)?;

                match structure.arguments.as_slice() {
                    [] => Ok(()),
                    [x, xs @ ..] => {
                        let x = WithInterner { data: x, interner };
                        write!(f, "[{}", x)?;
                        for x in xs {
                            let x = WithInterner { data: x, interner };
                            write!(f, " {}", x)?;
                        }
                        write!(f, "]")
                    }
                }
            }
            MonoType::String => write!(f, "String"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrowType {
    pub from: Box<MonoType>,
    pub to: Box<MonoType>,
}

#[derive(Debug, Clone)]
pub struct StructureType {
    pub path: Path,
    pub arguments: Vec<MonoType>,
}
