use std::{fs, io::{self, Write}};

use crate::{
    check::{TypeChecker, typ::MonoType},
    compilation::{Compiler, instruction::display_instructions, ConstantPool},
    interner::Interner,
    parse::Parser,
    resolution::{ExpressionResolver, ANFResolver},
    vm::{VM, value::Value},
    error::Result,
    compilation::anf::ANFTransformer,
};

fn expression(source: &str, vm: &mut VM, interner: &mut Interner) -> Result<(Value, MonoType, ConstantPool)> {
    let mut parser = Parser::from_source(source, interner);
    let mut expression_resolver = ExpressionResolver::new();
    let mut anf_resolver = ANFResolver::new();
    let mut checker = TypeChecker::new();

    let expression = parser.expression()?;

    expression_resolver.interactive_module(interner);
    let resolved = expression_resolver.expression(expression)?;
    resolved.data().print(interner, 0);

    let t = checker.infer(&resolved)?;

    let converter = ANFTransformer::new();
    let anf = converter.convert(resolved.destruct().0);
    let anf = anf_resolver.expression(anf);

    // anf.print(0, interner);

    let compiler = Compiler::new(interner);
    let (instructions, pool) = compiler.compile(&anf);

    display_instructions(&instructions, &pool);

    let result = vm.run(&instructions, &pool, true);

    Ok((result, t, pool))
}

fn program(sources: Vec<String>, vm: &mut VM, interner: &mut Interner) -> Result<(Value, ConstantPool)> {
    let mut resolver = ExpressionResolver::new();
    let mut checker = TypeChecker::new();

    let mut modules = vec![];
    for source in sources {
        let mut parser = Parser::from_source(&source, interner);
        modules.push(parser.module()?);
    }

    let resolved_definitions = resolver.program(modules.clone())?;
    checker.program(&resolved_definitions)?;

    let anf_transformer = ANFTransformer::new();
    let mut anf_resolver = ANFResolver::new();

    let mut anf_modules = vec![];
    for module in resolved_definitions {
        anf_modules.push(anf_transformer.module(module));
    }

    let anf_definitions = anf_resolver.program(anf_modules)?;

    let compiler = Compiler::new(interner);

    let (instructions, pool) = compiler.program(&anf_definitions);

    display_instructions(&instructions, &pool);

    let result = vm.run(&instructions, &pool, true);

    Ok((result, pool))
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
            Ok((result, t, pool)) => println!("= {} : {t}", result.display(pool.strings())),
            Err(error) => error.report("<interactive>", input, &interner),
        }

        vm.reset_state();
    }
}

pub fn from_file(file_paths: Vec<String>) {
    let file_path = file_paths[0].clone();

    let mut sources = vec![];
    for file_path in file_paths {
        let source = fs::read_to_string(file_path).unwrap();
        sources.push(source);
    }

    let source = sources[0].clone();

    let mut interner = Interner::new();
    let mut vm = VM::new();

    match program(sources, &mut vm, &mut interner) {
        Ok((result, pool)) => println!("= {}", result.display(pool.strings())),
        Err(error) => error.report(&file_path, &source, &interner),
    }
}