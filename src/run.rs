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
    parse::{Parser, ProgramParser},
    resolution::{ANFResolver, Resolver, renamer::Renamer},
    vm::{VM, value::Value},
};

fn expression(
    source: &str,
    vm: &mut VM,
    interner: &mut Interner,
) -> Result<(Value, MonoType, ConstantPool)> {
    let (expression, indicies) =
        Parser::from_source("<interactive>", source, Indicies::default(), interner)
            .parse(Parser::expression_repl)?;
    let metadata = Resolver::new(Metadata::new())
        .set_interactive_module(interner)
        .finish(Resolver::expression, &expression)?;
    let t = TypeChecker::new(&metadata).infer(&expression)?;
    let metadata = Renamer::new(metadata).finish(Renamer::expression, &expression);
    let anf_expression = anf::Transformer::new(&metadata, indicies).transform(expression.data);
    let metadata = ANFResolver::new(metadata).finish(ANFResolver::expression, &anf_expression);
    let (code, pool) = Compiler::new(interner, &metadata).compile(&anf_expression);
    let result = vm.run(&code, &pool, false);

    Ok((result, t, pool))
}

fn program(
    sources: &HashMap<String, String>,
    vm: &mut VM,
    interner: &mut Interner,
) -> Result<(Value, Type, ConstantPool)> {
    let (program, indicies) = ProgramParser::new(sources, interner, Indicies::default()).parse()?;
    let metadata = Resolver::new(Metadata::new()).finish(Resolver::program, &program)?;
    let t = TypeChecker::new(&metadata).program(&program, interner)?;
    let metadata = Renamer::new(metadata).finish(Renamer::program, &program);
    let anf_program = anf::Transformer::new(&metadata, indicies).program(program);
    let metadata = ANFResolver::new(metadata).finish(ANFResolver::program, &anf_program);
    let (code, pool) = Compiler::new(interner, &metadata).program(&anf_program);
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
                WithInterner {
                    data: &t,
                    interner: &interner
                }
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
            WithInterner {
                data: &t,
                interner: &interner
            }
        ),
        Err(error) => error.report(&sources[&error.source_name], &interner),
    }
}
