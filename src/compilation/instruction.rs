use std::fmt::Display;

use crate::{
    compilation::ConstantPool,
    parse::expression::{Pattern, StructurePattern},
    resolution::{Renamed, bound::Capture}
};

#[derive(Clone)]
pub enum Instruction {
    String(usize),
    Constructor(usize, usize),
    MakeLambda(usize, Vec<Capture>),
    GetCapture(usize),
    GetLocal(usize),
    GetAbsolute(usize),
    Jump(usize),
    StringEquals,
    StructurePatternMatch(StructurePattern<Renamed>),
    PatternLocals(Pattern<Renamed>),
    SkipIfFalse(usize),
    Skip(usize),
    EnterFrame,
    ExitFrame,
    Call,
    Return,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(offset) => write!(f, "STRING {offset}"),
            Self::Constructor(order, arity) => write!(f, "CONSTRUCTOR {order} {arity}"),
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
            },
            Self::GetCapture(id) => write!(f, "GET_CAPTURE {id}"),
            Self::GetLocal(id) => write!(f, "GET_LOCAL {id}"),
            Self::GetAbsolute(id) => write!(f, "GET_ABSOLUTE {id}"),
            Self::Jump(label) => write!(f, "JUMP {label}"),
            Self::StringEquals => write!(f, "STRING_EQUALS"),
            Self::StructurePatternMatch(_) => write!(f, "STRUCTURE_EQUALS"),
            Self::PatternLocals(_) => write!(f, "PATTERN_LOCALS"),
            Self::SkipIfFalse(count) => write!(f, "SKIP_IF_FALSE {count}"),
            Self::Skip(count) => write!(f, "SKIP {count}"),
            Self::EnterFrame => write!(f, "ENTER_FRAME"),
            Self::ExitFrame => write!(f, "EXIT_FRAME"),
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
            println!("    {index:#>05x} | {string}");
        }
    }

    if !pool.structure.is_empty() {
        println!("  ====== LAMBDAS ======");
        for (index, lambda) in pool.structure.iter().enumerate() {
            println!("    <lambda {index}>:");
            for (index, instruction) in lambda.iter().enumerate() {
                println!("    {index:#>05x} | {instruction}");
            }
        }
    }
}