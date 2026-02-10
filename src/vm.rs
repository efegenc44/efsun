use std::rc::Rc;

use crate::{compiler::Instruction, resolver::Capture};

pub struct VM {
    stack: Vec<Frame>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: vec![Frame::new()],
        }
    }

    fn current_frame(&self) -> &Frame {
        self.stack.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut Frame {
        self.stack.last_mut().unwrap()
    }

    fn push(&mut self, value: Value) {
        self.current_frame_mut().stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.current_frame_mut().stack.pop().unwrap()
    }

    pub fn run(&mut self, instructions: &[Instruction]) -> Value {
        self.stack = vec![Frame::new()];

        let mut ip = 0;

        while ip < instructions.len() {
            let instruction = instructions[ip].clone();
            ip += 1;

            match instruction {
                Instruction::String(offset) => {
                    self.push(Value::String(offset));
                }
                Instruction::MakeLambda(address, captures) => {
                    let mut closure = vec![];

                    for capture in captures {
                        let value = match capture {
                            Capture::Local(id) => {
                                self.current_frame().stack[id.value()].clone()
                            },
                            Capture::Outer(id) => {
                                self.current_frame().closure[id.value()].clone()
                            },
                        };

                        closure.push(value);
                    }

                    self.push(Value::Lambda(LambdaValue {
                        address, captures: Rc::new(closure)
                    }));
                },
                Instruction::GetCapture(id) => {
                    let value = self.current_frame().closure[id].clone();
                    self.push(value);
                },
                Instruction::GetLocal(id) => {
                    let value = self.current_frame().stack[id].clone();
                    self.push(value);
                },
                Instruction::Jump(address) => {
                    ip = address;
                },
                Instruction::Call => {
                    let lambda = self.pop().into_lambda();
                    let argument = self.pop();

                    self.stack.push(Frame::new());
                    self.push(argument);

                    self.current_frame_mut().closure = lambda.captures;
                    self.current_frame_mut().return_ip = ip;

                    ip = lambda.address;
                },
                Instruction::Return => {
                    let return_value = self.pop();
                    let return_ip = self.stack.pop().unwrap().return_ip;
                    self.push(return_value);
                    ip = return_ip;
                },
            }
        }

        if self.stack.len() != 1 {
            println!("{:?}", self.current_frame().stack);
            panic!();
        }

        self.pop()
    }
}

struct Frame {
    stack: Vec<Value>,
    closure: Rc<Vec<Value>>,
    return_ip: usize,
}

impl Frame {
    fn new() -> Self {
        Self::with_closure(vec![])
    }

    fn with_closure(closure: Vec<Value>) -> Self {
        Self {
            stack: Vec::new(),
            closure: Rc::new(closure),
            return_ip: 0
        }
    }
}

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

    fn into_lambda(self) -> LambdaValue {
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
