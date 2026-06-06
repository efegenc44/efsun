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
    metadata::{Indicies, Metadata},
    parse::{Parser, definition},
    resolution::{ANFResolver, Resolver, renamer::Renamer},
    vm::{VM, value::Value},
};

fn expression(
    source: &str,
    vm: &mut VM,
    interner: &mut Interner,
) -> Result<(Value, MonoType, ConstantPool)> {
    let indicies = Indicies::default();

    let mut parser = Parser::from_source("<interactive>", source, indicies, interner);
    let expression = parser.expression_repl()?;
    let indicies = parser.indicies();

    let metadata = Metadata::new();

    let mut resolver = Resolver::new(metadata).interactive_environment(interner);
    resolver.expression(&expression)?;
    let metadata = resolver.finish();

    let t = TypeChecker::new(&metadata).infer(&expression)?;

    let mut renamer = Renamer::new(metadata);
    renamer.expression(&expression);
    let metadata = renamer.finish();

    let transformer = anf::Transformer::new(&metadata, indicies);
    let anf = transformer.transform(expression.into_data());

    let mut resolver = ANFResolver::new(metadata);
    resolver.expression(&anf);
    let metadata = resolver.finish();

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
    let mut current_indicies = Indicies::default();
    for (source_name, source) in sources {
        let mut parser = Parser::from_source(source_name, source, current_indicies, interner);
        modules.push(parser.module()?);
        current_indicies = parser.indicies();
    }

    let program = definition::Program::new(modules);

    let metadata = Metadata::new();

    let mut resolver = Resolver::new(metadata);
    resolver.program(&program)?;
    let metadata = resolver.finish();

    let t = TypeChecker::new(&metadata).program(&program, interner)?;

    let mut renamer = Renamer::new(metadata);
    renamer.program(&program);
    let metadata = renamer.finish();

    let transformer = anf::Transformer::new(&metadata, current_indicies);
    let anf = program.into_anf(&transformer);

    let mut resolver = ANFResolver::new(metadata);
    resolver.program(&anf);
    let metadata = resolver.finish();

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
