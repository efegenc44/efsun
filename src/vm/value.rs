use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Value {
    Lambda(LambdaValue),
    String(usize)
}

impl Value {
    pub fn display(&self, strings: &[String]) -> String {
        match self {
            Value::Lambda(lambda) => format!("<lambda@{}>", lambda.address),
            Value::String(offset) => strings[*offset].to_string(),
        }
    }

    pub fn into_lambda(self) -> LambdaValue {
        let Self::Lambda(lambda) = self else {
            panic!();
        };

        lambda
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
