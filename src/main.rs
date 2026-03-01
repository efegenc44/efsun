#![allow(unused)]

mod parse;
mod check;
mod compilation;
mod resolution;

mod interner;
mod location;
mod error;
mod vm;
mod cli;
mod run;

fn main() {
    cli::execute();
}
