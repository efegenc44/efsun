use std::{fs, io::{self, Write}};

use crate::{
    check::{TypeChecker, typ::MonoType},
    compile::{Compiler, instruction::display_instructions},
    interner::Interner,
    parse::{Parser, lex::Lexer},
    resolver::{ExpressionResolver, ANFResolver},
    vm::{VM, Value},
    error::Result,
    compile::anf::ANFConverter,
};

fn expression(source: &str, vm: &mut VM, interner: &mut Interner) -> Result<(Value, MonoType, Vec<String>)> {
    let lexer = Lexer::new(source, interner);
    let mut parser = Parser::new(lexer);
    let mut expression_resolver = ExpressionResolver::new();
    let mut anf_resolver = ANFResolver::new();
    let mut checker = TypeChecker::new();

    let expression = parser.expression()?;

    let resolved = expression_resolver.expression(expression.clone())?;
    let t = checker.infer(&resolved)?;

    let converter = ANFConverter::new();
    let anf = converter.convert(expression.destruct().0);
    let anf = anf_resolver.expression(anf);

    // anf.print(0, interner);

    let compiler = Compiler::new(interner);
    let (instructions, strings) = compiler.compile(&anf);

    display_instructions(&instructions, &strings);

    let result = vm.run(&instructions);

    Ok((result, t, strings))
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
            Ok((result, t, strings)) => println!("= {} : {t}", result.display(&strings)),
            Err(error) => error.report("<interactive>", input, &interner),
        }
    }
}

pub fn from_file(file_path: &str) {
    let source = fs::read_to_string(file_path).unwrap();
    let mut interner = Interner::new();
    let mut vm = VM::new();

    match expression(&source, &mut vm, &mut interner) {
        Ok((result, t, strings)) => println!("= {} : {t}", result.display(&strings)),
        Err(error) => error.report(file_path, &source, &interner),
    }
}