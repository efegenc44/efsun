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
    MakeStructure(usize, usize, usize),
    MakeLambda(usize, usize, Vec<Capture>),
    GetCapture(usize),
    GetLocal(usize),
    GetAbsolute(usize),
    GetArgument(usize),
    StringEquals,
    TagEquals(usize),
    LogicalAnd,
    PopScope(usize),
    PushBase,
    SetBase(usize),
    Call(usize),
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
            Self::MakeStructure(name_offset, tag, arity) => {
                write!(f, "MAKE_STRUCTURE {name_offset} {tag} {arity}")
            }
            Self::PushBase => {
                write!(f, "PUSH_BASE")
            }
            Self::SetBase(n) => {
                write!(f, "SET_BASE {n}")
            }
            Self::MakeLambda(address, arity, captures) => {
                write!(f, "MAKE_LAMBDA {address} {arity}")?;
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
            Self::PopScope(n) => write!(f, "POP_SCOPE {n}"),
            Self::Call(n) => write!(f, "CALL {n}"),
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
