pub mod instruction;
pub mod anf;

use crate::{
    interner::{InternId, Interner},
    resolution::{Resolved, bound::{Bound, Path}},
};

use anf::{ANF, Atom, ANFDefinition};

use instruction::Instruction;

pub struct Compiler<'interner> {
    code: Vec<Instruction>,
    interns: Vec<InternId>,
    strings: Vec<String>,
    names: Vec<Path>,
    interner: &'interner Interner
}

impl<'interner> Compiler<'interner> {
    pub fn new(interner: &'interner Interner) -> Self {
        Self {
            code: Vec::new(),
            interns: Vec::new(),
            strings: Vec::new(),
            names: Vec::new(),
            interner
        }
    }

    fn write(&mut self, instruction: Instruction) {
        self.code.push(instruction);
    }

    fn ip(&self) -> usize {
        self.code.len()
    }

    pub fn program(mut self, modules: &[Vec<ANFDefinition<Resolved>>]) -> (Vec<Instruction>, Vec<String>) {
        for module in modules {
            for definition in module {
                if let ANFDefinition::Name(name) = definition {
                    self.names.push(name.path().clone());
                }
            }
        }

        for module in modules {
            self.module(module);
        }

        let parts = vec!["Main", "main"]
            .iter()
            .map(|s| self.interner.intern_id(s))
            .collect::<Vec<_>>();

        let path = Path::from_parts(parts);

        let id = self.names.iter().position(|p| p == &path).unwrap();
        // TODO: Remove this instruction
        self.write(Instruction::String(0));
        self.write(Instruction::GetAbsolute(id));
        // TODO: Enforce main symbol to have an arrow type
        self.write(Instruction::Call);

        (self.code, self.strings)
    }

    pub fn module(&mut self, definitions: &[ANFDefinition<Resolved>]) {
        // for definition in definitions {
        //     if let ANFDefinition::Name(name) = definition {
        //         self.names.push(name.path().clone());
        //     }
        // }

        for definition in definitions {
            match definition {
                ANFDefinition::Module(_) => (),
                ANFDefinition::Name(name) => {
                    self.expression(name.expression());
                },
                ANFDefinition::Import(_) => (),
            }
        }
    }

    pub fn compile(mut self, expression: &ANF<Resolved>) -> (Vec<Instruction>, Vec<String>) {
        self.expression(expression);
        (self.code, self.strings)
    }

    fn expression(&mut self, expression: &ANF<Resolved>) {
        match expression {
            ANF::Let(letin) => self.letin(letin),
            ANF::Application(application) => self.application(application),
            ANF::Atom(atom) => self.atom(atom),
        }
    }

    fn atom(&mut self, atom: &Atom<Resolved>) {
        match atom {
            Atom::String(id) => self.string(*id),
            Atom::Path(identifier) => self.identifier(identifier),
            Atom::Lambda(lambda) => self.lambda(lambda),
        }
    }

    fn string(&mut self, intern_id: InternId) {
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

        self.write(Instruction::String(offset));
    }

    fn identifier(&mut self, identifier: &anf::PathExpression<Resolved>) {
        match identifier.bound() {
            Bound::Local(id) => self.write(Instruction::GetLocal(id.value())),
            Bound::Capture(id) => self.write(Instruction::GetCapture(id.value())),
            Bound::Absolute(path) => {
                let id = self.names.iter().position(|p| p == path).unwrap();
                self.write(Instruction::GetAbsolute(id));
            },
        }
    }

    fn application(&mut self, application: &anf::ApplicationExpression<Resolved>) {
        self.atom(application.argument());
        self.atom(application.function());
        self.write(Instruction::Call);
        self.expression(application.expression());
    }

    fn lambda(&mut self, lambda: &anf::LambdaExpression<Resolved>) {
        self.write(Instruction::Jump(0));
        let start = self.ip();
        self.expression(lambda.expression());
        self.write(Instruction::Return);
        let end = self.ip();
        self.write(Instruction::MakeLambda(start, lambda.captures().to_vec()));

        self.code[start - 1] = Instruction::Jump(end);
    }

    fn letin(&mut self, letin: &anf::LetExpression<Resolved>) {
        self.atom(letin.variable_expression());
        self.expression(letin.return_expression());
    }
}
