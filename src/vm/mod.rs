pub mod value;

use std::rc::Rc;

use crate::{
    compilation::{ConstantPool, instruction::Instruction},
    resolution::{Renamed, bound::Capture},
    parse::expression::Pattern,
    interner::Interner
};

use value::{
    Value, LambdaValue, ConstructorValue, StructureValue
};

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

    pub fn run(&mut self, instructions: &[Instruction], pool: &ConstantPool, is_main: bool, interner: &Interner) -> Value {
        let mut ip = 0;

        while ip < instructions.len() {
            let instruction = instructions[ip].clone();
            ip += 1;

            match instruction {
                Instruction::String(offset) => {
                    self.push(Value::String(offset));
                }
                Instruction::Constructor(order, arity) => {
                    self.push(Value::Constructor(ConstructorValue::new(order, arity, vec![])));
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
                Instruction::StructurePatternMatch(pattern) => {
                    let structure = self.pop().into_structure();
                    let mut result = true;
                    for (value, pattern) in structure.values().iter().zip(pattern.arguments()) {
                        if !self.pattern_match(value, pattern.data(), pool, interner) {
                            result = false;
                            break;
                        }
                    }

                    self.push(Value::Bool(result));
                },
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
                    match self.pop() {
                        Value::Lambda(lambda) => {
                            let (address, captures) = lambda.destruct();
                            let argument = self.pop();

                            self.stack.push(Frame::new());
                            self.push(argument);
                            self.current_frame_mut().closure = captures;

                            // FIX THIS
                            let value = self.run(&pool.lambdas()[address], pool, false, interner);
                            self.push(value);
                        },
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
                        },
                        _ => unreachable!()
                    }
                },
                Instruction::Return => {
                    let return_value = self.pop();
                    self.stack.pop().unwrap();
                    self.push(return_value);
                },
                Instruction::PatternLocals(pattern) => {
                    let value = self.pop();
                    self.pattern_locals(value, &pattern);
                },
            }
        }

        if self.stack.len() != 1 && is_main {
            println!("{:?}", self.current_frame().stack);
            panic!();
        }

        self.pop()
    }

    fn pattern_match(&self, value: &Value, pattern: &Pattern<Renamed>, pool: &ConstantPool, interner: &Interner) -> bool {
        match (value, pattern) {
            (Value::Structure(structure_value), Pattern::Structure(structure_pattern)) => {
                if structure_value.order() != structure_pattern.order() {
                    return false;
                }

                for (value, pattern) in structure_value.values().iter().zip(structure_pattern.arguments()) {
                    if !self.pattern_match(value, pattern.data(), pool, interner) {
                        return false;
                    }
                }

                true
            },
            (Value::String(id1), Pattern::String(id2)) => pool.strings()[*id1] == interner.lookup(*id2),
            (_, Pattern::Any(_)) => true,
            _ => false,
        }
    }

    fn pattern_locals(&mut self, value: Value, pattern: &Pattern<Renamed>) {
        match (value, pattern) {
            (Value::Structure(structure_value), Pattern::Structure(structure_pattern)) => {
                let (_, arguments) = structure_value.destruct();
                for (value, pattern) in arguments.iter().zip(structure_pattern.arguments()) {
                    self.pattern_locals(value.clone(), pattern.data());
                }
            },
            (value, Pattern::Any(_)) => self.push(value),
            _ => ()
        }
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
