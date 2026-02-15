use std::env;

use crate::run;

pub struct Command {
    name: &'static str,
    arguments: &'static str,
    description: &'static str,
    action: fn(Vec<String>)
}

impl Command {
    const fn new(
        name: &'static str,
        arguments: &'static str,
        description: &'static str,
        action: fn(Vec<String>)
    ) -> Self {
        Self { name, arguments, description, action }
    }
}

const COMMANDS: &[Command] = &[
    Command::new("help", "[COMMAND]", "Displays this message", |args| {
        match args.as_slice() {
            [] => help(),
            [command] => {
                if let Some(command) = COMMANDS.iter().find(|c| c.name == command) {
                    help_commmand(command);
                } else {
                    error(&format!("Unknown command `{command}`"));
                }
            }
            _ => error("Provided too many arguments"),
        }
    }),
    Command::new("interactive", "", "Enter the the interactive efsun session", |_| {
        run::repl();
    }),
    Command::new("execute", "FILE*", "Execute an efsun program from a file", |args| {
        match args.as_slice() {
            [] => error("No file provided"),
            _ => run::from_file(args),
        }
    })
];

fn help() {
    println!();
    println!("    Usage: efsun COMMAND [ARGUMENT*]");
    println!();
    println!("    Available commands:");
    for command in COMMANDS {
        println!("      {:<15} {}", command.name, command.description)
    }
    println!();
}

fn help_commmand(command: &Command) {
    println!();
    println!("    Usage: efsun {} {}", command.name, command.arguments);
    println!();
    println!("      {}", command.description);
    println!();
}

fn error(message: &str) {
    println!();
    println!("    Error: {message}");
    help();
}

pub fn execute() {
    let mut args = env::args().skip(1);
    let Some(command) = args.next() else {
        error("No command provided");
        return;
    };

    if let Some(command) = COMMANDS.iter().find(|c| c.name == command) {
        (command.action)(args.collect());
    } else {
        error(&format!("Unknown command `{command}`"));
    }
}