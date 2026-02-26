pub mod value;

use std::rc::Rc;

use crate::{compilation::{ConstantPool, instruction::Instruction}, resolution::bound::Capture};

use value::{Value, LambdaValue};

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

    pub fn reset_state(&mut self) {
        self.stack = vec![Frame::new()];
    }

    pub fn run(&mut self, instructions: &[Instruction], pool: &ConstantPool, is_main: bool) -> Value {
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

                    self.push(Value::Lambda(LambdaValue::new(address, closure)));
                },
                Instruction::GetCapture(id) => {
                    let value = self.current_frame().closure[id].clone();
                    self.push(value);
                },
                Instruction::GetLocal(id) => {
                    let value = self.current_frame().stack[id].clone();
                    self.push(value);
                },
                Instruction::GetAbsolute(id) => {
                    let value = self.stack.first().unwrap().stack[id].clone();
                    self.push(value);
                },
                Instruction::Jump(_label) => {
                    panic!()
                },
                Instruction::StringEquals => {
                    let s1 = &pool.strings()[self.pop().into_string()];
                    let s2 = &pool.strings()[self.pop().into_string()];

                    self.push(Value::Bool(s1 == s2));
                }
                Instruction::Skip(count) => {
                    ip += count;
                }
                Instruction::SkipIfFalse(count) => {
                    if !self.pop().into_bool() {
                        ip += count;
                    }
                }
                Instruction::EnterFrame => {
                    self.stack.push(Frame::new());
                }
                Instruction::ExitFrame => {
                    let return_value = self.pop();
                    self.stack.pop().unwrap();
                    self.push(return_value);
                }
                Instruction::Call => {
                    let (address, captures) = self.pop().into_lambda().destruct();
                    let argument = self.pop();

                    self.stack.push(Frame::new());
                    self.push(argument);
                    self.current_frame_mut().closure = captures;

                    // FIX THIS
                    let value = self.run(&pool.lambdas()[address], pool, false);
                    self.push(value);
                },
                Instruction::Return => {
                    let return_value = self.pop();
                    self.stack.pop().unwrap();
                    self.push(return_value);
                },
            }
        }

        if self.stack.len() != 1 && is_main {
            println!("{:?}", self.current_frame().stack);
            panic!();
        }

        self.pop()
    }
}

struct Frame {
    stack: Vec<Value>,
    closure: Rc<Vec<Value>>,
}

impl Frame {
    fn new() -> Self {
        Self::with_closure(vec![])
    }

    fn with_closure(closure: Vec<Value>) -> Self {
        Self {
            stack: Vec::new(),
            closure: Rc::new(closure),
        }
    }
}
