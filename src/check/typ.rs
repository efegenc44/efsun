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
            Type::Mono(m) => write!(f, "{}", WithInterner { data: m, interner }),
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
                write!(f, "{}", WithInterner { data: m, interner })
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
                if let MonoType::Arrow(_) = *arrow.from {
                    write!(
                        f,
                        "({})",
                        WithInterner {
                            data: arrow.from.as_ref(),
                            interner
                        }
                    )?;
                } else {
                    write!(
                        f,
                        "{}",
                        WithInterner {
                            data: arrow.from.as_ref(),
                            interner
                        }
                    )?;
                }
                write!(
                    f,
                    " -> {}",
                    WithInterner {
                        data: arrow.to.as_ref(),
                        interner
                    }
                )
            }
            MonoType::Structure(structure) => {
                write!(
                    f,
                    "{}",
                    WithInterner {
                        data: &structure.path,
                        interner
                    }
                )?;
                match structure.arguments.as_slice() {
                    [] => Ok(()),
                    [argument] => {
                        write!(
                            f,
                            "[{}]",
                            WithInterner {
                                data: argument,
                                interner
                            }
                        )
                    }
                    [x, xs @ ..] => {
                        write!(f, "[{}", WithInterner { data: x, interner })?;
                        for x in xs {
                            write!(f, " {}", WithInterner { data: x, interner })?;
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
