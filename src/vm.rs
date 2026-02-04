use std::{fmt::Display, rc::Rc};

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
        let mut ip = 0;

        while ip < instructions.len() {
            let instruction = instructions[ip].clone();
            ip += 1;

            match instruction {
                Instruction::MakeString(string) => {
                    self.push(Value::String(string));
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
                Instruction::CapturingEnter(captures) => {
                    let argument = self.pop();

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

                    self.stack.push(Frame::with_closure(closure));
                    self.push(argument);
                }
                Instruction::Leave => {
                    let return_value = self.pop();
                    self.stack.pop().unwrap();
                    self.push(return_value);
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

        if self.current_frame().stack.len() != 1 || self.stack.len() != 1 {
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

#[derive(Clone)]
pub enum Value {
    Lambda(LambdaValue),
    String(String)
}

impl Value {
    fn into_lambda(self) -> LambdaValue {
        let Self::Lambda(lambda) = self else {
            dbg!(self);
            panic!();
        };

        lambda
    }
}

#[derive(Clone)]
pub struct LambdaValue {
    address: usize,
    captures: Rc<Vec<Value>>
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lambda(lambda) => write!(f, "<lambda@{:#x}>", lambda.address),
            Self::String(string) => write!(f, "\"{string}\"")
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}