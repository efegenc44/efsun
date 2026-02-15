use std::{fs, io::{self, Write}};

use crate::{
    check::{TypeChecker, typ::MonoType},
    compilation::{Compiler, instruction::display_instructions},
    interner::Interner,
    parse::Parser,
    resolution::{ExpressionResolver, ANFResolver},
    vm::{VM, value::Value},
    error::Result,
    compilation::anf::ANFTransformer,
};

fn expression(source: &str, vm: &mut VM, interner: &mut Interner) -> Result<(Value, MonoType, Vec<String>)> {
    let mut parser = Parser::from_source(source, interner);
    let mut expression_resolver = ExpressionResolver::new();


    let mut anf_resolver = ANFResolver::new();
    let mut checker = TypeChecker::new();

    let expression = parser.expression()?;

    expression_resolver.interactive_module(interner);
    let resolved = expression_resolver.expression(expression.clone())?;
    resolved.data().print(interner, 0);

    let t = checker.infer(&resolved)?;

    let converter = ANFTransformer::new();
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