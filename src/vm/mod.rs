pub mod value;

use std::{cell::RefCell, cmp::Ordering, io::Write, println, rc::Rc};

use crate::{
    compilation::{ConstantPool, instruction::Instruction},
    resolution::bound::Capture,
    vm::value::{ClosurePointer, PartialApplicationValue},
};

use value::{LambdaValue, StructureValue, Value};

/// Stack-Based Virtual Machine
pub struct VM {
    /// Stack of values
    stack: Vec<Value>,
    /// Stack frame base register, points to a stack location
    base: usize,
    /// Stack frame closure register
    closure: ClosurePointer,
    /// Count of remaining arguments for higher order functions
    remaining_arguments: usize,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            base: 0,
            closure: None,
            remaining_arguments: 0,
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

            match instruction.clone() {
                Instruction::Unit => {
                    self.push(Value::Unit);
                }
                Instruction::String(offset) => {
                    self.push(Value::String(offset));
                }
                Instruction::MakeStructure(name_offset, order, arity) => {
                    let mut values = Vec::with_capacity(arity);
                    for _ in 0..arity {
                        values.push(self.pop());
                    }

                    self.push(Value::Structure(StructureValue {
                        name_offset,
                        order,
                        values: Rc::new(values),
                    }));
                }
                Instruction::MakeLambda(address, arity) => {
                    self.push(Value::Lambda(LambdaValue {
                        address,
                        arity,
                        captures: None,
                    }));
                }
                Instruction::CaptureIntoLambda(captures, self_capture) => {
                    let mut closure = Vec::with_capacity(captures.len());
                    for capture in captures {
                        let value = match capture {
                            Capture::Local(id) => self.stack[self.base + id.value()].clone(),
                            Capture::Outer(id) => {
                                self.closure.as_ref().unwrap().borrow()[id.value()].clone()
                            }
                        };

                        closure.push(value);
                    }

                    let closure = Rc::new(RefCell::new(closure));

                    // Backpatching a capture loop in self-captured lambda's captures (if it's even captured)
                    if let Some(self_capture) = self_capture {
                        let Value::Lambda(capture) = &mut closure.borrow_mut()[self_capture] else {
                            unreachable!();
                        };

                        capture.captures = Some(closure.clone());
                    }

                    let mut lambda = self.pop().into_lambda();
                    lambda.captures = Some(closure.clone());
                    self.push(Value::Lambda(lambda));
                }
                Instruction::GetCapture(id) => {
                    let value = self.closure.as_ref().unwrap().borrow()[id].clone();
                    self.push(value);
                }
                Instruction::GetLocal(id) => {
                    let value = self.stack[self.base + id].clone();
                    self.push(value);
                }
                Instruction::CopyIntoLocal(id) => {
                    self.stack[self.base + id] = self.stack.last().unwrap().clone();
                }
                Instruction::TruncateFrame(n) => {
                    self.stack.truncate(self.base + n);
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
                Instruction::PushFrame(address) => {
                    self.push(Value::StackPointer(self.base));
                    self.push(Value::Closure(self.closure.clone()));
                    self.push(Value::InstructionPointer(address));
                }
                // TODO: Uniform lambda representation and less-doing instructions
                Instruction::Call(n) => {
                    let operand = self.pop();

                    match operand {
                        Value::Lambda(lambda) => {
                            match lambda.arity.cmp(&n) {
                                Ordering::Equal => {
                                    self.closure = lambda.captures;
                                    ip = lambda.address;
                                }
                                Ordering::Greater => {
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

                                    let return_value = Value::PartialApplication(value);
                                    self.stack.truncate(self.base);
                                    ip = self.pop().into_instruction_pointer();
                                    self.closure = self.pop().into_closure();
                                    self.base = self.pop().into_stack_pointer();
                                    self.push(return_value);
                                }
                                Ordering::Less => {
                                    let step = n - lambda.arity;
                                    self.base += step;
                                    self.remaining_arguments = step;

                                    self.closure = lambda.captures;
                                    ip = lambda.address;
                                }
                            }
                        }
                        Value::PartialApplication(lambda) => {
                            let mut arguments = vec![];
                            for _ in 0..n {
                                arguments.push(self.pop())
                            }

                            let mut values = (*lambda.parital).clone();
                            values.extend(arguments);

                            match lambda.remaining.cmp(&n) {
                                Ordering::Equal => {
                                    self.stack.extend(values.into_iter().rev());
                                    self.closure = lambda.captures;
                                    ip = lambda.address;
                                }
                                Ordering::Greater => {
                                    self.push(Value::PartialApplication(PartialApplicationValue {
                                        address: lambda.address,
                                        remaining: lambda.remaining - n,
                                        parital: Rc::new(values),
                                        captures: lambda.captures,
                                    }));

                                    let return_value = self.pop();
                                    self.stack.truncate(self.base);
                                    ip = self.pop().into_instruction_pointer();
                                    self.closure = self.pop().into_closure();
                                    self.base = self.pop().into_stack_pointer();
                                    self.push(return_value);
                                }
                                Ordering::Less => {
                                    let step = n - lambda.remaining;
                                    self.base += step;
                                    self.remaining_arguments = step;

                                    self.stack.extend(values.into_iter().rev());

                                    self.closure = lambda.captures;
                                    ip = lambda.address;
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                Instruction::Return => {
                    if self.remaining_arguments > 0 {
                        let lambda = self.pop();

                        self.stack.truncate(self.base);

                        let (arity, captures, address) = match lambda {
                            Value::Lambda(lambda) => {
                                (lambda.arity, lambda.captures, lambda.address)
                            }
                            Value::PartialApplication(lambda) => {
                                self.stack.extend((lambda.parital.iter().rev().cloned()).clone());
                                (lambda.remaining, lambda.captures, lambda.address)
                            }
                            _ => unreachable!()
                        };

                        if arity > self.remaining_arguments {
                            let remaining = arity - self.remaining_arguments;
                            self.base -= self.remaining_arguments;

                            let mut partial = vec![];
                            for _ in 0..self.remaining_arguments {
                                partial.push(self.pop());
                            }

                            self.remaining_arguments = 0;

                            self.push(Value::PartialApplication(PartialApplicationValue {
                                address,
                                remaining,
                                parital: Rc::new(partial),
                                captures: captures,
                            }));

                            let return_value = self.pop();
                            self.stack.truncate(self.base);
                            ip = self.pop().into_instruction_pointer();
                            self.closure = self.pop().into_closure();
                            self.base = self.pop().into_stack_pointer();
                            self.push(return_value);
                        } else {
                            self.base -= arity;
                            self.remaining_arguments -= arity;

                            self.closure = captures;
                            ip = address;
                        }
                    } else {
                        let return_value = self.pop();
                        self.stack.truncate(self.base);
                        let address = self.pop().into_instruction_pointer();
                        ip = address;
                        self.closure = self.pop().into_closure();
                        self.base = self.pop().into_stack_pointer();
                        self.push(return_value);
                    }
                }
                Instruction::PopBase => {
                    let return_value = self.pop();
                    self.stack.truncate(self.base);
                    self.push(return_value);
                }
                Instruction::TagEquals(tag) => {
                    let structure = self.pop().into_structure();
                    self.push(Value::Bool(structure.order == tag));
                }
                Instruction::GetArgument(nth) => {
                    let structure = self.pop().into_structure();
                    self.push(structure.values[nth].clone())
                }
                Instruction::LogicalAnd => {
                    let b = self.pop().into_bool();
                    let a = self.pop().into_bool();

                    self.push(Value::Bool(a && b));
                }
                Instruction::Bool(bool) => {
                    self.push(Value::Bool(bool));
                }
                Instruction::Halt => {
                    break;
                }
            }

            if debug {
                print!("{instruction} ");
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
