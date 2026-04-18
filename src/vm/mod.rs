pub mod value;

use std::rc::Rc;

use crate::{
    compilation::{ConstantPool, instruction::Instruction},
    resolution::bound::Capture,
};

use value::{ConstructorValue, LambdaValue, StructureValue, Value};

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

    pub fn run(
        &mut self,
        instructions: &[Instruction],
        pool: &ConstantPool<Instruction>,
        is_main: bool,
    ) -> Value {
        let mut ip = 0;

        while ip < instructions.len() {
            let instruction = instructions[ip].clone();
            ip += 1;

            match instruction {
                Instruction::Unit => {
                    self.push(Value::Unit);
                }
                Instruction::String(offset) => {
                    self.push(Value::String(offset));
                }
                Instruction::Constructor(order, arity) => {
                    if arity == 0 {
                        self.push(Value::Structure(StructureValue::new(order, Vec::new())));
                    } else {
                        self.push(Value::Constructor(ConstructorValue::new(
                            order,
                            arity,
                            vec![],
                        )));
                    }
                }
                Instruction::MakeLambda(address, captures) => {
                    let mut closure = vec![];

                    for capture in captures {
                        let value = match capture {
                            Capture::Local(id) => self.current_frame().stack[id.value()].clone(),
                            Capture::Outer(id) => self.current_frame().closure[id.value()].clone(),
                        };

                        closure.push(value);
                    }

                    self.push(Value::Lambda(LambdaValue::new(address, closure)));
                }
                Instruction::GetCapture(id) => {
                    let value = self.current_frame().closure[id].clone();
                    self.push(value);
                }
                Instruction::GetLocal(id) => {
                    let value = self.current_frame().stack[id].clone();
                    self.push(value);
                }
                Instruction::GetAbsolute(id) => {
                    let value = self.stack.first().unwrap().stack[id].clone();
                    self.push(value);
                }
                Instruction::StringEquals => {
                    let s1 = &pool.strings()[self.pop().into_string()];
                    let s2 = &pool.strings()[self.pop().into_string()];

                    self.push(Value::Bool(s1 == s2));
                }
                Instruction::Jump(address) => {
                    ip = address;
                }
                Instruction::JumpIfFalse(address) => {
                    if !self.pop().into_bool() {
                        ip = address;
                    }
                }
                Instruction::SetBase => {
                    self.current_frame_mut().base = self.current_frame().stack.len();
                }
                Instruction::Truncate => {
                    let return_value = self.pop();
                    let base = self.current_frame().base;
                    self.current_frame_mut().stack.truncate(base);
                    self.push(return_value);
                }
                Instruction::Call => {
                    match self.pop() {
                        Value::Lambda(lambda) => {
                            let (address, captures) = lambda.destruct();
                            let argument = self.pop();

                            self.stack.push(Frame::new());
                            self.push(argument);
                            self.current_frame_mut().closure = captures;

                            // FIX THIS
                            let value = self.run(&pool.lambdas()[address], pool, false);
                            self.push(value);
                        }
                        Value::Constructor(constructor) => {
                            let (order, arity, captures) = constructor.destruct();
                            let argument = self.pop();

                            let value = if arity <= 1 {
                                let mut values = (*captures).clone();
                                values.push(argument);
                                Value::Structure(StructureValue::new(order, values))
                            } else {
                                let mut values = (*captures).clone();
                                values.push(argument);
                                Value::Constructor(ConstructorValue::new(order, arity - 1, values))
                            };

                            self.push(value);
                        }
                        _ => unreachable!(),
                    }
                }
                Instruction::Return => {
                    let return_value = self.pop();
                    self.stack.pop().unwrap();
                    self.push(return_value);
                }
                Instruction::TagEquals(tag) => {
                    let structure = self.pop().into_structure();
                    self.push(Value::Bool(structure.order() == tag));
                }
                Instruction::GetArgument(nth) => {
                    let structure = self.pop().into_structure();
                    self.push(structure.values()[nth].clone())
                }
                Instruction::LogicalAnd => {
                    let b = self.pop().into_bool();
                    let a = self.pop().into_bool();

                    self.push(Value::Bool(a && b));
                }
                Instruction::Bool(bool) => {
                    self.push(Value::Bool(bool));
                }
            }

            // println!("{:?}", self.stack);
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
    base: usize,
}

impl Frame {
    fn new() -> Self {
        Self::with_closure(vec![])
    }

    fn with_closure(closure: Vec<Value>) -> Self {
        Self {
            stack: Vec::new(),
            closure: Rc::new(closure),
            base: 0,
        }
    }
}

impl std::fmt::Debug for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.stack)
    }
}
