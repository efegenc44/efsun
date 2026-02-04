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
mod evaluator;
mod value;
mod compiler;
mod vm;

use std::{env, fs, io::{Write, stdin, stdout}};

use crate::compiler::display_instructions;

fn main() {
    let args: Vec<String> = env::args().collect();
    match &args[1..] {
        [] => repl(),
        [file_path] => from_file(file_path),
        _ => panic!()
    }
}

fn repl() {
    let mut interner = interner::Interner::new();
    // let mut evaluator = evaluator::Evaluator::new();

    loop {
        let mut input = String::new();
        print!("> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut input).unwrap();
        let input = input.trim();

        if input.is_empty() {
            continue;
        }

        if input == "q" {
            break;
        }

        let lexer = lexer::Lexer::new(input, &mut interner);
        let mut parser = parser::Parser::new(lexer);
        let mut resolver = resolver::Resolver::new();
        let mut checker = checker::TypeChecker::new();

        let expression = match parser.expression() {
            Ok(expression) => expression,
            Err(error) => {
                error.report("interactive", input, &interner);
                continue;
            },
        };

        let expression = match resolver.expression(expression) {
            Ok(expression) => expression,
            Err(error) => {
                error.report("interactive", input, &interner);
                continue;
            },
        };

        // expression.data().print(&interner, 0);

        let t = match checker.infer(&expression) {
            Ok(t) => t,
            Err(error) => {
                error.report("interactive", input, &interner);
                continue;
            },
        };

        // let value = evaluator.expression(expression.data());

        let mut compiler = compiler::Compiler::new(&interner);
        let mut vm = vm::VM::new();

        let instructions = compiler.compile(expression.data());
        display_instructions(instructions);

        let result = vm.run(instructions);
        println!("= {result} : {t}");
    }
}

fn from_file(file_path: &str) {
    let source = fs::read_to_string(file_path).unwrap();
    let mut interner = interner::Interner::new();
    let lexer = lexer::Lexer::new(&source, &mut interner);
    let mut parser = parser::Parser::new(lexer);
    let mut resolver = resolver::Resolver::new();
    let mut checker = checker::TypeChecker::new();
    let mut evaluator = evaluator::Evaluator::new();

    let expression = match parser.expression() {
        Ok(expression) => expression,
        Err(error) => {
            error.report(file_path, &source, &interner);
            return;
        },
    };

    let expression = match resolver.expression(expression) {
        Ok(expression) => expression,
        Err(error) => {
            error.report(file_path, &source, &interner);
            return;
        },
    };

    expression.data().print(&interner, 0);

    let t = match checker.infer(&expression) {
        Ok(t) => t,
        Err(error) => {
            error.report(file_path, &source, &interner);
            return;
        },
    };

    let value = evaluator.expression(expression.data());

    println!("= {value} : {t}");
}