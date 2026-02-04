use std::{fs, io::{self, Write}};

use crate::{
    checker::TypeChecker,
    compiler::Compiler,
    interner::Interner,
    lexer::Lexer,
    parser::Parser,
    resolver::Resolver,
    typ::MonoType,
    vm::{VM, Value},
    error::Result
};

fn expression(source: &str, vm: &mut VM, interner: &mut Interner) -> Result<(Value, MonoType)> {
    let lexer = Lexer::new(source, interner);
    let mut parser = Parser::new(lexer);
    let mut resolver = Resolver::new();
    let mut checker = TypeChecker::new();

    let expression = parser.expression()?;
    let expression = resolver.expression(expression)?;
    let t = checker.infer(&expression)?;

    let mut compiler = Compiler::new(interner);
    let instructions = compiler.compile(expression.data());
    let result = vm.run(instructions);

    Ok((result, t))
}

pub fn repl() {
    let mut interner = Interner::new();
    let mut vm = VM::new();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();

        if input.is_empty() {
            continue;
        }

        if input == "q" {
            break;
        }

        match expression(input, &mut vm, &mut interner) {
            Ok((result, t)) => println!("= {result} : {t}"),
            Err(error) => error.report("<interactive>", input, &interner),
        }
    }
}

pub fn from_file(file_path: &str) {
    let source = fs::read_to_string(file_path).unwrap();
    let mut interner = Interner::new();
    let mut vm = VM::new();

    match expression(&source, &mut vm, &mut interner) {
        Ok((result, t)) => println!("= {result} : {t}"),
        Err(error) => error.report(file_path, &source, &interner),
    }
}