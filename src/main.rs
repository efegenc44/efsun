mod parse;
mod check;
mod compile;
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
