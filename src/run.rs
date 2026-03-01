use std::{collections::HashMap, fs, io::{self, Write}};

use crate::{
    check::{TypeChecker, typ::{MonoType, Type}},
    compilation::{Compiler, ConstantPool, instruction::display_instructions},
    interner::Interner,
    parse::Parser,
    resolution::{ExpressionResolver, ANFResolver, renamer::Renamer},
    vm::{VM, value::Value},
    error::Result,
    compilation::anf::ANFTransformer,
};

fn expression(source: &str, vm: &mut VM, interner: &mut Interner) -> Result<(Value, MonoType, ConstantPool)> {
    let expression   = Parser::from_source("<interactive>".to_string(), source, interner).expression()?;
    let resolved     = ExpressionResolver::interactive(interner).expression(expression)?;
    let t            = TypeChecker::new().infer(&resolved)?;
    let renamed      = Renamer::new().expression(resolved);
    let anf          = ANFTransformer::new().transform(renamed.as_data());
    let resolved_anf = ANFResolver::new().expression(anf);
    let (code, pool) = Compiler::new(interner).compile(&resolved_anf);

    display_instructions(&code, &pool);

    let result       = vm.run(&code, &pool, true, interner);

    Ok((result, t, pool))
}

fn program(sources: &HashMap<String, String>, vm: &mut VM, interner: &mut Interner) -> Result<(Value, Type, ConstantPool)> {
    let modules = sources
        .iter()
        .map(|(source_name, source)| Parser::from_source(source_name.clone(), source, interner).module())
        .collect::<Result<_>>()?;
    let resolved     = ExpressionResolver::new().program(modules)?;
    let t            = TypeChecker::new().program(&resolved, interner)?;

    println!(": {t}");


    let renamed      = Renamer::new().program(resolved);
    let anf          = ANFTransformer::new().program(renamed);
    let resolved_anf = ANFResolver::new().program(anf);
    panic!();
    let (code, pool) = Compiler::new(interner).program(&resolved_anf);
    let result       = vm.run(&code, &pool, true, interner);

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
            Ok((result, t, pool)) => println!("= {} : {t}", result.display(pool.strings())),
            Err((error, source_name)) => error.report(&source_name, input, &interner),
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
        Ok((result, t, pool)) => println!("= {} : {t}", result.display(pool.strings())),
        Err((error, source_name)) => error.report(&source_name, &sources[&source_name], &interner),
    }
}