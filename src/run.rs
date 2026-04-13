use std::{
    collections::HashMap,
    fs,
    io::{self, Write},
};

use crate::{
    check::{
        TypeChecker,
        typ::{MonoType, Type},
    },
    compilation::{Compiler, ConstantPool, anf, instruction::display_instructions},
    error::Result,
    interner::{Interner, WithInterner},
    parse::{Parser, definition},
    resolution::{ANFResolver, Resolver, renamer::Renamer},
    vm::{VM, value::Value},
};

fn expression(
    source: &str,
    vm: &mut VM,
    interner: &mut Interner,
) -> Result<(Value, MonoType, ConstantPool)> {
    let expression = Parser::from_source("<interactive>", source, interner).expression_repl()?;
    let resolved = Resolver::new()
        .interactive_environment(interner)
        .expression(expression)?;
    let t = TypeChecker::new()
        .interactive_environment()
        .infer(&resolved)?;
    let renamed = Renamer::new()
        .interactive_environment()
        .expression(resolved);
    let anf = anf::Transformer::new().transform(renamed.into_data());
    let resolved_anf = ANFResolver::new().interactive_environment().expression(anf);
    let (code, pool) = Compiler::new(interner).compile(&resolved_anf);

    display_instructions(&code, &pool);

    let result = vm.run(&code, &pool, true);

    Ok((result, t, pool))
}

fn program(
    sources: &HashMap<String, String>,
    vm: &mut VM,
    interner: &mut Interner,
) -> Result<(Value, Type, ConstantPool)> {
    let modules = sources
        .iter()
        .map(|(source_name, source)| Parser::from_source(source_name, source, interner).module())
        .collect::<Result<Vec<_>>>()?;
    let program = definition::program::Observation { modules }.into();
    let resolved = Resolver::new().program(program)?;
    let t = TypeChecker::new().program(&resolved, interner)?;
    let renamed = Renamer::new().program(resolved);
    let anf = anf::Transformer::new().program(renamed);
    let resolved_anf = ANFResolver::new().program(anf);
    let (code, pool) = Compiler::new(interner).program(&resolved_anf);
    let result = vm.run(&code, &pool, true);

    Ok((result, t, pool))
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
            Ok((result, t, pool)) => println!(
                "= {} : {}",
                result.display(pool.strings()),
                WithInterner::new(&t, &interner)
            ),
            Err(error) => error.report(input, &interner),
        }

        vm.reset_state();
    }
}

pub fn from_file(file_paths: Vec<String>) {
    let mut sources = HashMap::new();

    for file_path in file_paths {
        let source = fs::read_to_string(&file_path).unwrap();
        sources.insert(file_path, source);
    }

    let mut interner = Interner::new();
    let mut vm = VM::new();

    match program(&sources, &mut vm, &mut interner) {
        Ok((result, t, pool)) => println!(
            "= {} : {}",
            result.display(pool.strings()),
            WithInterner::new(&t, &interner)
        ),
        Err(error) => error.report(&sources[error.source_name()], &interner),
    }
}
