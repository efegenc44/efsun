use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Value {
    Lambda(LambdaValue),
    Constructor(ConstructorValue),
    Structure(StructureValue),
    String(usize),
    Bool(bool)
}

impl Value {
    pub fn display(&self, strings: &[String]) -> String {
        match self {
            Self::Lambda(lambda) => format!("<lambda@{}>", lambda.address),
            Self::Constructor(_) => format!("<constructor>"),
            Self::Structure(structure) => {
                let mut string = String::from("{ ");
                for value in structure.values.iter() {
                    string.push_str(&value.display(strings));
                    string.push(' ');
                }
                string.push('}');
                string
            },
            Self::String(offset) => strings[*offset].to_string(),
            Self::Bool(bool) => bool.to_string(),
        }
    }

    #[allow(unused)]
    pub fn into_lambda(self) -> LambdaValue {
        let Self::Lambda(lambda) = self else {
            panic!();
        };

        lambda
    }

    #[allow(unused)]
    pub fn into_constructor(self) -> ConstructorValue {
        let Self::Constructor(constructor) = self else {
            panic!();
        };

        constructor
    }

    #[allow(unused)]
    pub fn into_structure(self) -> StructureValue {
        let Self::Structure(structure) = self else {
            panic!();
        };

        structure
    }

    pub fn into_string(self) -> usize {
        let Self::String(string) = self else {
            panic!()
        };

        string
    }

    pub fn into_bool(self) -> bool {
        let Self::Bool(bool) = self else {
            panic!()
        };

        bool
    }
}
#[derive(Clone, Debug)]
pub struct LambdaValue {
    address: usize,
    captures: Rc<Vec<Value>>
}

impl LambdaValue {
    pub fn new(address: usize, captures: Vec<Value>) -> Self {
        Self { address, captures: Rc::new(captures) }
    }

    pub fn destruct(self) -> (usize, Rc<Vec<Value>>) {
        (self.address, self.captures)
    }
}

#[derive(Clone, Debug)]
pub struct ConstructorValue {
    order: usize,
    arity: usize,
    captures: Rc<Vec<Value>>,
}

impl ConstructorValue {
    pub fn new(order: usize, arity: usize, captures: Vec<Value>) -> Self {
        Self { order, arity, captures: Rc::new(captures) }
    }

    pub fn destruct(self) -> (usize, usize, Rc<Vec<Value>>) {
        (self.order, self.arity, self.captures)
    }
}

#[derive(Clone, Debug)]
pub struct StructureValue {
    #[allow(unused)]
    order: usize,
    values: Rc<Vec<Value>>,
}

impl StructureValue {
    pub fn new(order: usize, values: Vec<Value>) -> Self {
        Self { order, values: Rc::new(values) }
    }

    #[allow(unused)]
    pub fn destruct(self) -> (usize, Rc<Vec<Value>>) {
        (self.order, self.values)
    }
}
