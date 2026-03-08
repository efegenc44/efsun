#![allow(unused)]

mod check;
mod compilation;
mod parse;
mod resolution;

mod cli;
mod error;
mod interner;
mod location;
mod run;
mod vm;

fn main() {
    cli::execute();
}
