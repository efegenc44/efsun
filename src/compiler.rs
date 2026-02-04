use std::fmt::Display;

use crate::{
    expression::{
        Expression, LambdaExpression, LetExpression, Resolved,
        ApplicationExpression, IdentifierExpression
    },
    resolver::{Bound, Capture},
    interner::Interner,
};

pub struct Compiler<'interner> {
    output: Vec<Instruction>,
    interner: &'interner Interner
}

impl<'interner> Compiler<'interner> {
    pub fn new(interner: &'interner Interner) -> Self {
        Self {
            output: Vec::new(),
            interner
        }
    }

    fn write(&mut self, instruction: Instruction) {
        self.output.push(instruction);
    }

    fn ip(&self) -> usize {
        self.output.len()
    }

    pub fn compile(&mut self, expression: &Expression<Resolved>) -> &[Instruction] {
        self.expression(expression);
        &self.output
    }

    fn expression(&mut self, expression: &Expression<Resolved>) {
        match expression {
            Expression::String(string) => self.write(Instruction::MakeString(self.interner.lookup(*string).to_string())),
            Expression::Identifier(identifier) => self.identifier(identifier),
            Expression::Application(application) => self.application(application),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Let(letin) => self.letin(letin),
        }
    }

    fn identifier(&mut self, identifier: &IdentifierExpression<Resolved>) {
        match identifier.bound() {
            Bound::Local(id) => self.write(Instruction::GetLocal(id.value())),
            Bound::Capture(id) => self.write(Instruction::GetCapture(id.value())),
        }
    }

    fn application(&mut self, application: &ApplicationExpression<Resolved>) {
        self.expression(application.argument().data());
        self.expression(application.function().data());
        self.write(Instruction::Call);
    }

    fn lambda(&mut self, lambda: &LambdaExpression<Resolved>) {
        self.write(Instruction::Jump(0));
        let start = self.ip();
        self.expression(lambda.expression().data());
        self.write(Instruction::Return);
        let end = self.ip();
        self.write(Instruction::MakeLambda(start, lambda.captures().to_vec()));

        self.output[start - 1] = Instruction::Jump(end);
    }

    fn letin(&mut self, letin: &LetExpression<Resolved>) {
        self.write(Instruction::CapturingEnter(letin.captures().to_vec()));
        self.expression(letin.variable_expression().data());
        self.expression(letin.return_expression().data());
        self.write(Instruction::Leave);
    }
}

#[derive(Clone)]
pub enum Instruction {
    MakeString(String),
    MakeLambda(usize, Vec<Capture>),
    GetCapture(usize),
    GetLocal(usize),
    Jump(usize),
    CapturingEnter(Vec<Capture>),
    Leave,
    Call,
    Return,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MakeString(string) => write!(f, "MAKE_STRING \"{string}\""),
            Self::MakeLambda(address, captures) => {
                write!(f, "MAKE_LAMBDA {address:#x}")?;
                for capture in captures {
                    write!(f, "\n        CAPTURE_")?;
                    match capture {
                        Capture::Local(id) => write!(f, "LOCAL {}", id.value())?,
                        Capture::Outer(id) => write!(f, "OUTER {}", id.value())?,
                    }
                }

                Ok(())
            },
            Self::GetCapture(id) => write!(f, "GET_CAPTURE {id}"),
            Self::GetLocal(id) => write!(f, "GET_LOCAL {id}"),
            Self::Jump(address) => write!(f, "JUMP {address:#x}"),
            Self::CapturingEnter(captures) => {
                write!(f, "ENTER")?;
                for capture in captures {
                    write!(f, "\n        CAPTURE_")?;
                    match capture {
                        Capture::Local(id) => write!(f, "LOCAL {}", id.value())?,
                        Capture::Outer(id) => write!(f, "OUTER {}", id.value())?,
                    }
                }

                Ok(())
            }
            Self::Leave => write!(f, "LEAVE"),
            Self::Call => write!(f, "CALL"),
            Self::Return => write!(f, "RETURN"),
        }
    }
}

#[allow(unused)]
pub fn display_instructions(instructions: &[Instruction]) {
    for (index, instruction) in instructions.iter().enumerate() {
        println!("{index:#>05x} | {instruction}");
    }
}