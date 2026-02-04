mod interner;
mod lexer;
mod token;
mod location;
mod parser;
mod expression;
mod resolver;
mod checker;
mod typ;
mod error;
mod compiler;
mod vm;
mod cli;
mod run;

fn main() {
    cli::execute();
}
