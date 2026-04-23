use std::fmt::Display;

use crate::{
    compilation::ConstantPool,
    resolution::bound::{Capture, Path},
};

#[derive(Clone)]
pub enum PreInstruction {
    Placeholder(Placeholder),
    Instruction(Instruction),
}

impl From<Instruction> for PreInstruction {
    fn from(value: Instruction) -> Self {
        Self::Instruction(value)
    }
}

impl From<Placeholder> for PreInstruction {
    fn from(value: Placeholder) -> Self {
        Self::Placeholder(value)
    }
}

#[derive(Clone)]
pub enum Placeholder {
    GetAbsolute(Path),
    SkipIfFalse(usize),
    Skip(usize),
    Jump(usize),
}

#[derive(Clone)]
pub enum Instruction {
    Unit,
    String(usize),
    Bool(bool),
    Constructor(usize, usize, usize),
    MakeLambda(usize, Vec<Capture>),
    GetCapture(usize),
    GetLocal(usize),
    GetAbsolute(usize),
    GetArgument(usize),
    StringEquals,
    TagEquals(usize),
    LogicalAnd,
    SetBase,
    Truncate,
    Call,
    Return,
    Jump(usize),
    JumpIfFalse(usize),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "UNIT"),
            Self::String(offset) => write!(f, "STRING {offset}"),
            Self::Bool(bool) => write!(f, "BOOL {bool}"),
            Self::Constructor(name_offset, order, arity) => {
                write!(f, "CONSTRUCTOR {name_offset} {order} {arity}")
            }
            Self::MakeLambda(address, captures) => {
                write!(f, "MAKE_LAMBDA {address}")?;
                for capture in captures {
                    write!(f, "\n            CAPTURE_")?;
                    match capture {
                        Capture::Local(id) => write!(f, "LOCAL {}", id.value())?,
                        Capture::Outer(id) => write!(f, "OUTER {}", id.value())?,
                    }
                }

                Ok(())
            }
            Self::GetCapture(id) => write!(f, "GET_CAPTURE {id}"),
            Self::GetLocal(id) => write!(f, "GET_LOCAL {id}"),
            Self::GetAbsolute(id) => write!(f, "GET_ABSOLUTE {id}"),
            Self::GetArgument(nth) => write!(f, "GET_ARGUMENT {nth}"),
            Self::StringEquals => write!(f, "STRING_EQUALS"),
            Self::TagEquals(tag) => write!(f, "TAG_EQUALS {tag}"),
            Self::LogicalAnd => write!(f, "AND"),
            Self::Jump(address) => write!(f, "JUMP {address:X}"),
            Self::JumpIfFalse(address) => write!(f, "JUMP_IF_FALSE {address:X}"),
            Self::SetBase => write!(f, "SET_BASE"),
            Self::Truncate => write!(f, "TRUNCATE"),
            Self::Call => write!(f, "CALL"),
            Self::Return => write!(f, "RETURN"),
        }
    }
}

#[allow(unused)]
pub fn display_instructions(instructions: &[Instruction], pool: &ConstantPool) {
    println!("  ====== CODE ======");
    for (index, instruction) in instructions.iter().enumerate() {
        println!("    {index:#>05x} | {instruction}");
    }

    if !pool.strings.is_empty() {
        println!("  ====== STRINGS ======");
        for (index, string) in pool.strings.iter().enumerate() {
            println!("    {index:#>05x} | \"{string}\"");
        }
    }

    if !pool.lambdas.is_empty() {
        println!("  ====== LAMBDAS ======");
        for (index, lambda) in pool.lambdas.iter().enumerate() {
            println!("    <lambda {index}>:");
            for (index, instruction) in lambda.iter().enumerate() {
                println!("    {index:#>05x} | {instruction}");
            }
        }
    }
}
