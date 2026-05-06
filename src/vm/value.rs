use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Unit,
    Lambda(LambdaValue),
    Constructor(ConstructorValue),
    Structure(StructureValue),
    String(usize),
    Bool(bool),

    StackPointer(usize),
    Closure(Rc<Vec<Value>>),
}

impl Value {
    pub fn display(&self, strings: &[String]) -> String {
        match self {
            Self::Unit => "Unit".to_string(),
            Self::Lambda(lambda) => format!("<lambda {}>", lambda.address),
            Self::Constructor(constructor) => {
                format!(
                    "<{} {}>",
                    &strings[constructor.name_offset],
                    constructor.captures.len()
                )
            }
            Self::Structure(structure) => {
                let Some(values) = &structure.values else {
                    return strings[structure.name_offset].to_string();
                };

                let mut string = format!("({}", &strings[structure.name_offset]);
                match values.as_ref().as_slice() {
                    [] => (),
                    [x, xs @ ..] => {
                        string.push(' ');
                        string.push_str(&x.display(strings));
                        for x in xs {
                            string.push(' ');
                            string.push_str(&x.display(strings));
                        }
                    }
                }
                string.push(')');

                string
            }
            Self::String(offset) => {
                format!("\"{}\"", &strings[*offset])
            }
            Self::Bool(bool) => bool.to_string(),

            Self::StackPointer(sp) => format!("<sp {sp}>"),
            Self::Closure(_) => "<closure>".to_string(),
        }
    }

    #[allow(unused)]
    pub fn into_lambda(self) -> LambdaValue {
        let Self::Lambda(lambda) = self else {
            panic!("Expected lambda");
        };

        lambda
    }

    #[allow(unused)]
    pub fn into_constructor(self) -> ConstructorValue {
        let Self::Constructor(constructor) = self else {
            panic!("Expected constructor");
        };

        constructor
    }

    #[allow(unused)]
    pub fn into_structure(self) -> StructureValue {
        let Self::Structure(structure) = self else {
            panic!("Expected structure");
        };

        structure
    }

    pub fn into_string(self) -> usize {
        let Self::String(string) = self else {
            panic!("Expected string")
        };

        string
    }

    pub fn into_bool(self) -> bool {
        let Self::Bool(bool) = self else {
            panic!("Expected bool")
        };

        bool
    }

    pub fn into_stack_pointer(self) -> usize {
        let Self::StackPointer(sp) = self else {
            panic!("Expected stack pointer")
        };

        sp
    }

    pub fn into_closure(self) -> Rc<Vec<Value>> {
        let Self::Closure(closure) = self else {
            panic!("Expected closure")
        };

        closure
    }
}
#[derive(Clone)]
pub struct LambdaValue {
    address: usize,
    captures: Rc<Vec<Value>>,
}

impl LambdaValue {
    pub fn new(address: usize, captures: Vec<Value>) -> Self {
        Self {
            address,
            captures: Rc::new(captures),
        }
    }

    pub fn destruct(self) -> (usize, Rc<Vec<Value>>) {
        (self.address, self.captures)
    }
}

#[derive(Clone)]
pub struct ConstructorValue {
    name_offset: usize,
    order: usize,
    arity: usize,
    captures: Rc<Vec<Value>>,
}

impl ConstructorValue {
    pub fn new(name_offset: usize, order: usize, arity: usize, captures: Vec<Value>) -> Self {
        Self {
            name_offset,
            order,
            arity,
            captures: Rc::new(captures),
        }
    }

    pub fn destruct(self) -> (usize, usize, usize, Rc<Vec<Value>>) {
        (self.name_offset, self.order, self.arity, self.captures)
    }
}

#[derive(Clone)]
pub struct StructureValue {
    name_offset: usize,
    order: usize,
    values: Option<Rc<Vec<Value>>>,
}

impl StructureValue {
    pub fn new(name_offset: usize, order: usize, values: Option<Vec<Value>>) -> Self {
        Self {
            name_offset,
            order,
            values: values.map(Rc::new),
        }
    }

    pub fn order(&self) -> usize {
        self.order
    }

    pub fn values(&self) -> &Vec<Value> {
        self.values.as_ref().unwrap()
    }
}
