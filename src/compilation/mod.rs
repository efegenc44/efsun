pub mod instruction;
pub mod anf;

use std::collections::HashMap;

use crate::{
    interner::{InternId, Interner},
    resolution::{Resolved, bound::{Bound, Path}},
    parse::expression::Pattern,
};

use anf::{ANF, Atom, ANFDefinition};

use instruction::Instruction;

pub struct Compiler<'interner, 'anf> {
    interns: Vec<InternId>,
    constant_pool: ConstantPool,
    name_anfs: HashMap<&'anf Path, &'anf ANF<Resolved>>,
    names: Vec<(Path, Vec<Instruction>)>,
    interner: &'interner Interner
}

impl<'interner, 'anf> Compiler<'interner, 'anf> {
    pub fn new(interner: &'interner Interner) -> Self {
        Self {
            interns: Vec::new(),
            constant_pool: ConstantPool::new(),
            name_anfs: HashMap::new(),
            names: Vec::new(),
            interner
        }
    }

    pub fn program(mut self, modules: &'anf [Vec<ANFDefinition<Resolved>>]) -> (Vec<Instruction>, ConstantPool) {
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
        // TODO: Error when main function is not present
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

        (instructions, self.constant_pool)
    }

    fn collect_names(&mut self, definitions: &'anf [ANFDefinition<Resolved>]) {
        for definition in definitions {
            if let ANFDefinition::Name(name) = definition {
                // self.names.push((name.path().clone(), vec![]));
                self.name_anfs.insert(name.path(), name.expression());
            }

            if let ANFDefinition::Structure(structure) = definition {
                for (order, (path, arity)) in structure.constructors().iter().enumerate() {
                    let instructions = vec![Instruction::Constructor(order, *arity)];
                    self.names.push((path.clone(), instructions));
                }
            }
        }
    }

    pub fn module(&mut self, definitions: &[ANFDefinition<Resolved>]) {
        for definition in definitions {
            match definition {
                ANFDefinition::Name(name) => {
                    if self.names.iter().find(|p| &p.0 == name.path()).is_none() {
                        let code = self.expression(name.expression());
                        self.names.push((name.path().clone(), code));
                    }
                },
                _ => ()
            }
        }
    }

    pub fn compile(mut self, expression: &ANF<Resolved>) -> (Vec<Instruction>, ConstantPool) {
        (self.expression(expression), self.constant_pool)
    }

    #[must_use]
    fn expression(&mut self, expression: &ANF<Resolved>) -> Vec<Instruction> {
        match expression {
            ANF::Let(letin) => self.letin(letin),
            ANF::Application(application) => self.application(application),
            ANF::Match(matchlet) => self.matchlet(matchlet),
            ANF::Join(join) => self.join(join),
            ANF::Jump(jump) => self.jump(jump),
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
                let offset = self.constant_pool.add_string(string);
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
                let id = match self.names.iter().position(|p| &p.0 == path) {
                    Some(id) => id,
                    None => {
                        let id = self.names.len();
                        self.names.push((path.clone(), vec![]));
                        let code = self.expression(self.name_anfs[path]);
                        self.names.get_mut(id).unwrap().1 = code;
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

    fn matchlet(&mut self, matchlet: &anf::MatchExpression<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(matchlet.variable_expression()));

        for branch in matchlet.branches() {
            match branch.pattern() {
                Pattern::Any(_) => {
                    instructions.extend(self.atom(branch.matched()));
                    instructions.extend(self.expression(branch.expression()));
                },
                Pattern::String(string) => {
                    instructions.extend(self.atom(branch.matched()));
                    instructions.extend(self.string(*string));
                    instructions.push(Instruction::StringEquals);
                    let expression = self.expression(branch.expression());
                    instructions.push(Instruction::SkipIfFalse(expression.len()));
                    instructions.extend(expression);
                },
                Pattern::Structure(structure) => {
                    instructions.extend(self.atom(branch.matched()));
                    instructions.push(Instruction::StructurePatternMatch(structure.clone()));
                    let mut ins = vec![];
                    ins.extend(self.atom(branch.matched()));
                    ins.push(Instruction::PatternLocals(Pattern::Structure(structure.clone())));
                    ins.extend(self.expression(branch.expression()));
                    instructions.push(Instruction::SkipIfFalse(ins.len()));
                    instructions.extend(ins);
                },
            }
        }

        instructions
    }

    fn join(&mut self, join: &anf::Join<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        let mut join_instructions = self.expression(join.join());

        let len = join_instructions.len();
        for (index, instruction) in join_instructions.iter_mut().enumerate() {
            if let Instruction::Jump(label) = instruction {
                if *label == join.label() {
                    *instruction = Instruction::Skip(len - (index + 1))
                }
            }
        }

        instructions.push(Instruction::SetBase);
        instructions.extend(join_instructions);
        instructions.push(Instruction::Truncate);
        instructions.extend(self.expression(join.expression()));

        instructions
    }

    fn jump(&mut self, jump: &anf::Jump<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(jump.expression()));
        instructions.push(Instruction::Jump(jump.to()));

        instructions
    }

    fn lambda(&mut self, lambda: &anf::LambdaExpression<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        instructions.extend(self.expression(lambda.expression()));
        instructions.push(Instruction::Return);

        let id = self.constant_pool.add_lambda(instructions);

        vec![Instruction::MakeLambda(id, lambda.captures().to_vec())]
    }

    fn letin(&mut self, letin: &anf::LetExpression<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(letin.variable_expression()));
        instructions.extend(self.expression(letin.return_expression()));

        instructions
    }
}

pub struct ConstantPool {
    strings: Vec<String>,
    structure: Vec<Vec<Instruction>>,
}

impl ConstantPool {
    fn new() -> Self {
        Self {
            strings: Vec::new(),
            structure: Vec::new(),
        }
    }

    pub fn strings(&self) -> &[String] {
        &self.strings
    }

    pub fn lambdas(&self) -> &[Vec<Instruction>] {
        &self.structure
    }

    fn add_string(&mut self, string: String) -> usize {
        let id = self.strings.len();
        self.strings.push(string);
        id
    }

    fn add_lambda(&mut self, lambda: Vec<Instruction>) -> usize {
        let id = self.structure.len();
        self.structure.push(lambda);
        id
    }
}
