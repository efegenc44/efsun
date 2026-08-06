mod check;
mod compilation;
mod data_table;
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
