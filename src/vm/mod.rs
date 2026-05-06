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
        self.stack.clear();
    }

    pub fn run(
        &mut self,
        instructions: &[Instruction],
        pool: &ConstantPool,
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
                Instruction::Constructor(name_offset, order, arity) => {
                    self.push(Value::Constructor(ConstructorValue::new(
                        name_offset,
                        order,
                        arity,
                        Vec::with_capacity(arity),
                    )));
                }
                Instruction::Structure(name_offset, order) => {
                    self.push(Value::Structure(StructureValue::new(
                        name_offset,
                        order,
                        None,
                    )));
                }
                Instruction::MakeLambda(address, captures) => {
                    let mut closure = Vec::with_capacity(captures.len());

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
                    let s1 = self.pop().into_string();
                    let s2 = self.pop().into_string();

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
                Instruction::PopScope(n) => {
                    let return_value = self.pop();
                    let len = self.current_frame().stack.len() - n;
                    self.current_frame_mut().stack.truncate(len);
                    self.push(return_value);
                }
                Instruction::Call => {
                    let operand = self.pop();
                    let argument = self.pop();

                    match operand {
                        Value::Lambda(lambda) => {
                            let (address, captures) = lambda.destruct();

                            self.stack.push(Frame::with_closure(captures));
                            self.push(argument);

                            let value = self.run(&pool.lambdas()[address], pool);
                            self.push(value);
                        }
                        Value::Constructor(constructor) => {
                            let (name_offset, order, arity, captures) = constructor.destruct();

                            let value = if arity <= 1 {
                                let mut values = (*captures).clone();
                                values.push(argument);
                                Value::Structure(StructureValue::new(name_offset, order, Some(values)))
                            } else {
                                let mut values = (*captures).clone();
                                values.push(argument);
                                Value::Constructor(ConstructorValue::new(
                                    name_offset,
                                    order,
                                    arity - 1,
                                    values,
                                ))
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
        Self::with_closure(Rc::default())
    }

    fn with_closure(closure: Rc<Vec<Value>>) -> Self {
        Self {
            stack: Vec::new(),
            closure,
        }
    }
}
