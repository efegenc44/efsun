mod parse;
mod check;
mod compile;

mod interner;
mod location;
mod resolver;
mod error;
mod vm;
mod cli;
mod run;

fn main() {
    cli::execute();
}
