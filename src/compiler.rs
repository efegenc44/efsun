use std::fmt::Display;

use crate::{
    expression::{
        ApplicationExpression, Expression, IdentifierExpression, LambdaExpression, LetExpression, Resolved
    },
    interner::{Interner, InternId},
    resolver::{Bound, Capture},
};

pub struct Compiler<'interner> {
    code: Vec<Instruction>,
    interns: Vec<InternId>,
    strings: Vec<String>,
    interner: &'interner Interner
}

impl<'interner> Compiler<'interner> {
    pub fn new(interner: &'interner Interner) -> Self {
        Self {
            code: Vec::new(),
            interns: Vec::new(),
            strings: Vec::new(),
            interner
        }
    }

    fn write(&mut self, instruction: Instruction) {
        self.code.push(instruction);
    }

    fn ip(&self) -> usize {
        self.code.len()
    }

    pub fn compile(mut self, expression: &Expression<Resolved>) -> (Vec<Instruction>, Vec<String>) {
        self.expression(expression);
        (self.code, self.strings)
    }

    fn expression(&mut self, expression: &Expression<Resolved>) {
        match expression {
            Expression::String(id) => self.string(*id),
            Expression::Identifier(identifier) => self.identifier(identifier),
            Expression::Application(application) => self.application(application),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Let(letin) => self.letin(letin),
        }
    }

    fn string(&mut self, intern_id: InternId) {
        let offset = match self.interns.iter().position(|id| *id == intern_id) {
            Some(offset) => offset,
            None => {
                let string = self.interner.lookup(intern_id).to_string();
                let offset = self.strings.len();
                self.strings.push(string);
                self.interns.push(intern_id);
                offset
            },
        };

        self.write(Instruction::String(offset));
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

        self.code[start - 1] = Instruction::Jump(end);
    }

    fn letin(&mut self, letin: &LetExpression<Resolved>) {
        self.expression(letin.variable_expression().data());
        self.write(Instruction::CapturingEnter(letin.captures().to_vec()));
        self.expression(letin.return_expression().data());
        self.write(Instruction::Leave);
    }
}

#[derive(Clone)]
pub enum Instruction {
    String(usize),
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
            Self::String(offset) => write!(f, "STRING {offset}"),
            Self::MakeLambda(address, captures) => {
                write!(f, "MAKE_LAMBDA {address:#x}")?;
                for capture in captures {
                    write!(f, "\n            CAPTURE_")?;
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
                    write!(f, "\n            CAPTURE_")?;
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
pub fn display_instructions(instructions: &[Instruction], strings: &[String]) {
    println!("  ====== CODE ======");
    for (index, instruction) in instructions.iter().enumerate() {
        println!("    {index:#>05x} | {instruction}");
    }

    println!("  ====== STRINGS ======");
    for (index, string) in strings.iter().enumerate() {
        println!("    {index:#>05x} | {string}");
    }
}