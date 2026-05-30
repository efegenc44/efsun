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
    compilation::{Compiler, ConstantPool, anf},
    error::Result,
    interner::{Interner, WithInterner},
    metadata::{IndexState, Metadata},
    parse::{Parser, definition},
    resolution::{ANFResolver, Resolver, renamer::Renamer},
    vm::{VM, value::Value},
};

fn expression(
    source: &str,
    vm: &mut VM,
    interner: &mut Interner,
) -> Result<(Value, MonoType, ConstantPool)> {
    let expression = Parser::from_source("<interactive>", source, IndexState::default(), interner)
        .expression_repl()?;

    let mut metadata = Metadata::default();

    Resolver::new(&mut metadata)
        .interactive_environment(interner)
        .expression(&expression)?;
    let t = TypeChecker::new(&metadata).infer(&expression)?;
    Renamer::new(&mut metadata).expression(&expression);
    let anf = anf::Transformer::new(&metadata).transform(expression.into_data());
    ANFResolver::new(&mut metadata).expression(&anf);
    let (code, pool) = Compiler::new(interner, &metadata).compile(&anf);
    let result = vm.run(&code, &pool, false);

    Ok((result, t, pool))
}

fn program(
    sources: &HashMap<String, String>,
    vm: &mut VM,
    interner: &mut Interner,
) -> Result<(Value, Type, ConstantPool)> {
    let mut modules = vec![];
    let mut current_indicies = IndexState::default();
    for (source_name, source) in sources {
        let mut parser = Parser::from_source(source_name, source, current_indicies, interner);
        modules.push(parser.module()?);
        current_indicies = parser.indicies();
    }

    let program = definition::Program::new(modules);

    let mut metadata = Metadata::default();

    Resolver::new(&mut metadata).program(&program)?;
    let t = TypeChecker::new(&metadata).program(&program, interner)?;
    Renamer::new(&mut metadata).program(&program);
    let anf = anf::Transformer::new(&metadata).program(program);
    ANFResolver::new(&mut metadata).program(&anf);
    let (code, pool) = Compiler::new(interner, &metadata).program(&anf);
    let result = vm.run(&code, &pool, false);

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
            Err(error) => {
                vm.reset_state();
                error.report(input, &interner)
            }
        }
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
