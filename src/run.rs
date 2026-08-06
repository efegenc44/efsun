use std::{
    collections::HashMap,
    fs,
    io::{self, Write},
};

use crate::{
    check::{TypeChecker, typ::Type},
    compilation::{Compiler, ConstantPool, anf},
    error::Result,
    interner::{Interner, WithInterner},
    parse::{Parser, ProgramParser},
    resolution::{ANFResolver, Resolver, renamer::Renamer},
    vm::{VM, value::Value},
};

fn expression(
    source: &str,
    vm: &mut VM,
    interner: &mut Interner,
) -> Result<(Value, Type, ConstantPool)> {
    let expression = Parser::from_source("<interactive>", source, interner).expression_repl()?;
    let resolution_data = Resolver::new()
        .set_interactive_module(interner)
        .expression_repl(&expression)?;
    let (t, type_check_data) = TypeChecker::new(&resolution_data).infer_repl(&expression)?;
    let rename_data = Renamer::new(&resolution_data).expression_repl(&expression);
    let anf_expression =
        anf::Transformer::new(&resolution_data, &rename_data).transform(expression.data);
    let anf_resolution_data = ANFResolver::new(&rename_data).expression_repl(&anf_expression);
    let (code, pool) = Compiler::new(
        interner,
        &resolution_data,
        &type_check_data,
        &anf_resolution_data,
    )
    .expression_repl(&anf_expression);
    let result = vm.run(&code, &pool, false);

    Ok((result, t, pool))
}

fn program(
    sources: &HashMap<String, String>,
    vm: &mut VM,
    interner: &mut Interner,
) -> Result<(Value, Type, ConstantPool)> {
    let program = ProgramParser::new(sources, interner).parse()?;
    let resolution_data = Resolver::new().program(&program)?;
    let (t, type_check_data) = TypeChecker::new(&resolution_data).program(&program, interner)?;
    let rename_data = Renamer::new(&resolution_data).program(&program);
    let anf_program = anf::Transformer::new(&resolution_data, &rename_data).program(program);
    let anf_resolution_data = ANFResolver::new(&rename_data).program(&anf_program);
    let (code, pool) = Compiler::new(
        interner,
        &resolution_data,
        &type_check_data,
        &anf_resolution_data,
    )
    .program(&anf_program);
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
            Ok((result, t, pool)) => {
                let t = WithInterner {
                    data: &t,
                    interner: &interner,
                };

                let result = result.display(pool.strings());

                println!("= {} : {}", result, t)
            }
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
        Ok((result, t, pool)) => {
            let t = WithInterner {
                data: &t,
                interner: &interner,
            };

            let result = result.display(pool.strings());

            println!("= {} : {}", result, t)
        }
        Err(error) => error.report(&sources[&error.source_name], &interner),
    }
}
