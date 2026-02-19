pub mod instruction;
pub mod anf;

use std::collections::HashMap;

use crate::{
    interner::{InternId, Interner},
    resolution::{Resolved, bound::{Bound, Path}},
};

use anf::{ANF, Atom, ANFDefinition};

use instruction::Instruction;

pub struct Compiler<'interner> {
    interns: Vec<InternId>,
    strings: Vec<String>,
    lambdas: Vec<Vec<Instruction>>,
    name_exprs: HashMap<Path, ANF<Resolved>>,
    names: Vec<(Path, Vec<Instruction>)>,
    interner: &'interner Interner
}

impl<'interner> Compiler<'interner> {
    pub fn new(interner: &'interner Interner) -> Self {
        Self {
            interns: Vec::new(),
            strings: Vec::new(),
            lambdas: Vec::new(),
            name_exprs: HashMap::new(),
            names: Vec::new(),
            interner
        }
    }

    pub fn program(mut self, modules: &[Vec<ANFDefinition<Resolved>>]) -> (Vec<Instruction>, Vec<String>, Vec<Vec<Instruction>>) {
        for module in modules {
            self.collect_names(module);
        }

        for module in modules {
            self.module(module);
        }

        let parts = vec!["Main", "main"]
            .iter()
            .map(|s| self.interner.intern_id(s))
            .collect::<Vec<_>>();

        let path = Path::from_parts(parts);

        let id = self.names.iter().position(|p| &p.0 == &path).unwrap();

        let mut instructions = self
            .names
            .into_iter()
            .map(|(_, code)| code)
            .flatten()
            .collect::<Vec<_>>();

        // TODO: Remove this instruction
        instructions.push(Instruction::String(0));
        instructions.push(Instruction::GetAbsolute(id));
        // TODO: Enforce main symbol to have an arrow type
        instructions.push(Instruction::Call);

        (instructions, self.strings, self.lambdas)
    }

    fn collect_names(&mut self, definitions: &[ANFDefinition<Resolved>]) {
        for definition in definitions {
            #[allow(irrefutable_let_patterns)]
            if let ANFDefinition::Name(name) = definition {
                self.name_exprs.insert(name.path().clone(), name.expression().clone());
            }
        }
    }

    pub fn module(&mut self, definitions: &[ANFDefinition<Resolved>]) {
        for definition in definitions {
            match definition {
                ANFDefinition::Name(name) => {
                    if let Some((_, _)) = self.names.iter().find(|p| &p.0 == name.path()) {
                    } else {
                        let code = self.expression(name.expression());
                        self.names.push((name.path().clone(), code));
                    }
                },
            }
        }
    }

    pub fn compile(mut self, expression: &ANF<Resolved>) -> (Vec<Instruction>, Vec<String>, Vec<Vec<Instruction>>) {
        (self.expression(expression), self.strings, self.lambdas)
    }

    #[must_use]
    fn expression(&mut self, expression: &ANF<Resolved>) -> Vec<Instruction> {
        match expression {
            ANF::Let(letin) => self.letin(letin),
            ANF::Application(application) => self.application(application),
            ANF::Atom(atom) => self.atom(atom),
        }
    }

    #[must_use]
    fn atom(&mut self, atom: &Atom<Resolved>) -> Vec<Instruction> {
        match atom {
            Atom::String(id) => self.string(*id),
            Atom::Path(identifier) => self.identifier(identifier),
            Atom::Lambda(lambda) => self.lambda(lambda),
        }
    }

    fn string(&mut self, intern_id: InternId) -> Vec<Instruction> {
        let offset = match self.interns.iter().position(|id| *id == intern_id) {
            Some(offset) => offset,
            None => {
                let string = self.interner.lookup(intern_id).to_string();
                let offset = self.strings.len();
                self.strings.push(string);
                self.interns.push(intern_id);
                offset
            },
        };

        vec![Instruction::String(offset)]
    }

    fn identifier(&mut self, identifier: &anf::PathExpression<Resolved>) -> Vec<Instruction> {
        match identifier.bound() {
            Bound::Local(id) => vec![Instruction::GetLocal(id.value())],
            Bound::Capture(id) => vec![Instruction::GetCapture(id.value())],
            Bound::Absolute(path) => {
                let mut instructions = vec![];

                let id = match self.names.iter().position(|p| &p.0 == path) {
                    Some(id) => id,
                    None => {
                        let expr = self.name_exprs[path].clone();
                        let code = self.expression(&expr);
                        instructions.extend(code.clone());
                        let id = self.names.len();
                        self.names.push((path.clone(), code));
                        id
                    },
                };

                vec![Instruction::GetAbsolute(id)]
            },
        }
    }

    fn application(&mut self, application: &anf::ApplicationExpression<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(application.argument()));
        instructions.extend(self.atom(application.function()));
        instructions.push(Instruction::Call);
        instructions.extend(self.expression(application.expression()));

        instructions
    }

    fn lambda(&mut self, lambda: &anf::LambdaExpression<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        instructions.extend(self.expression(lambda.expression()));
        instructions.push(Instruction::Return);

        let id = self.lambdas.len();
        self.lambdas.push(instructions);

        vec![Instruction::MakeLambda(id, lambda.captures().to_vec())]
    }

    fn letin(&mut self, letin: &anf::LetExpression<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(letin.variable_expression()));
        instructions.extend(self.expression(letin.return_expression()));

        instructions
    }
}
