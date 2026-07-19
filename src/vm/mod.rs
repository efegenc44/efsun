pub mod value;

use std::{io::Write, rc::Rc};

use crate::{
    compilation::{ConstantPool, instruction::Instruction},
    resolution::bound::Capture,
    vm::value::PartialApplicationValue,
};

use value::{ConstructorValue, LambdaValue, StructureValue, Value};

/// Stack-Based Virtual Machine
pub struct VM {
    /// Stack of values
    stack: Vec<Value>,
    /// Stack frame base register, points to a stack location
    base: usize,
    /// Stack frame closure register
    closure: Rc<Vec<Value>>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            base: 0,
            closure: Rc::new(Vec::new()),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    pub fn reset_state(&mut self) {
        self.stack.clear();
    }

    pub fn run(&mut self, instructions: &[Instruction], pool: &ConstantPool, debug: bool) -> Value {
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
                    self.push(Value::Constructor(ConstructorValue {
                        name_offset,
                        order,
                        arity,
                        captures: Rc::new(Vec::with_capacity(arity)),
                    }));
                }
                Instruction::Structure(name_offset, order) => {
                    self.push(Value::Structure(StructureValue {
                        name_offset,
                        order,
                        values: None,
                    }));
                }
                Instruction::MakeLambda(address, arity, captures) => {
                    let mut closure = Vec::with_capacity(captures.len());

                    for capture in captures {
                        let value = match capture {
                            Capture::Local(id) => self.stack[self.base + id.value()].clone(),
                            Capture::Outer(id) => self.closure.as_ref()[id.value()].clone(),
                        };

                        closure.push(value);
                    }

                    self.push(Value::Lambda(LambdaValue {
                        address,
                        arity,
                        captures: Rc::new(closure),
                    }));
                }
                Instruction::GetCapture(id) => {
                    let value = self.closure.as_ref()[id].clone();
                    self.push(value);
                }
                Instruction::GetLocal(id) => {
                    let value = self.stack[self.base + id].clone();
                    self.push(value);
                }
                Instruction::GetAbsolute(id) => {
                    let value = self.stack[id].clone();
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
                    let len = self.stack.len() - n;
                    self.stack.truncate(len);
                    self.push(return_value);
                }
                Instruction::SetBase(n) => {
                    self.base = self.stack.len() - n;
                }
                Instruction::PushBase => {
                    self.push(Value::StackPointer(self.base));
                    self.push(Value::Closure(self.closure.clone()));
                }
                Instruction::Call(n) => {
                    let operand = self.pop();

                    match operand {
                        Value::Lambda(lambda) => {
                            if lambda.arity != n {
                                let mut arguments = vec![];
                                for _ in 0..n {
                                    arguments.push(self.pop())
                                }

                                let value = PartialApplicationValue {
                                    address: lambda.address,
                                    remaining: lambda.arity - n,
                                    parital: Rc::new(arguments),
                                    captures: lambda.captures,
                                };

                                self.push(Value::PartialApplication(value));
                            } else {
                                self.closure = lambda.captures;

                                let value = self.run(&pool.lambdas()[lambda.address], pool, debug);
                                self.push(value);
                            }
                        }
                        Value::Constructor(constructor) => {
                            let mut arguments = vec![];
                            for _ in 0..n {
                                arguments.push(self.pop())
                            }

                            let mut values = (*constructor.captures).clone();
                            values.extend(arguments);

                            let value = if constructor.arity == n {
                                Value::Structure(StructureValue {
                                    name_offset: constructor.name_offset,
                                    order: constructor.order,
                                    values: Some(Rc::new(values)),
                                })
                            } else {
                                Value::Constructor(ConstructorValue {
                                    name_offset: constructor.name_offset,
                                    order: constructor.order,
                                    arity: constructor.arity - n,
                                    captures: Rc::new(values),
                                })
                            };

                            // TODO: Infer constructor application and lambda application
                            //   then handle them seperately
                            self.stack.truncate(self.base);
                            self.closure = self.pop().into_closure();
                            self.base = self.pop().into_stack_pointer();

                            self.push(value);
                        }
                        Value::PartialApplication(lambda) => {
                            let mut arguments = vec![];
                            for _ in 0..n {
                                arguments.push(self.pop())
                            }

                            let mut values = (*lambda.parital).clone();
                            values.extend(arguments);

                            let value = if lambda.remaining == n {
                                self.stack.extend(values);
                                self.closure = lambda.captures;

                                self.run(&pool.lambdas()[lambda.address], pool, debug)
                            } else {
                                Value::PartialApplication(PartialApplicationValue {
                                    address: lambda.address,
                                    remaining: lambda.remaining - n,
                                    parital: Rc::new(values),
                                    captures: lambda.captures,
                                })
                            };

                            self.push(value);
                        }
                        _ => unreachable!(),
                    }
                }
                Instruction::Return => {
                    let return_value = self.pop();
                    self.stack.truncate(self.base);
                    self.closure = self.pop().into_closure();
                    self.base = self.pop().into_stack_pointer();
                    self.push(return_value);
                }
                Instruction::TagEquals(tag) => {
                    let structure = self.pop().into_structure();
                    self.push(Value::Bool(structure.order == tag));
                }
                Instruction::GetArgument(nth) => {
                    let structure = self.pop().into_structure();
                    self.push(structure.values.unwrap()[nth].clone())
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

            if debug {
                print!("| ");
                for v in &self.stack {
                    print!("{} ", v.display(pool.strings()));
                }
                println!();
                print!("base pointer: {}", self.base);
                std::io::stdout().flush().unwrap();
                std::io::stdin().read_line(&mut String::new()).unwrap();
            }
        }

        self.pop()
    }
}
