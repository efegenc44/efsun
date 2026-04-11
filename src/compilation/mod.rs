pub mod anf;
pub mod instruction;

use std::collections::HashMap;

use crate::{
    interner::{InternId, Interner},
    parse::pattern,
    resolution::bound::{Bound, Path},
    state::Resolved,
};

use instruction::Instruction;

pub struct Compiler<'interner, 'anf> {
    interns: Vec<InternId>,
    constant_pool: ConstantPool,
    name_anfs: HashMap<&'anf Path, &'anf anf::Expression<Resolved>>,
    names: HashMap<&'anf Path, Vec<Instruction>>,
    globals: Vec<&'anf Path>,
    in_lambda: bool,
    interner: &'interner Interner,
}

impl<'interner, 'anf> Compiler<'interner, 'anf> {
    pub fn new(interner: &'interner Interner) -> Self {
        Self {
            interns: Vec::new(),
            constant_pool: ConstantPool::new(),
            name_anfs: HashMap::new(),
            names: HashMap::new(),
            globals: Vec::new(),
            in_lambda: false,
            interner,
        }
    }

    pub fn program(
        mut self,
        program: &'anf anf::Program<Resolved>,
    ) -> (Vec<Instruction>, ConstantPool) {
        for module in program.modules() {
            self.collect_names(module);
        }

        for module in program.modules() {
            self.module(module);
        }

        let parts = ["Main", "main"]
            .iter()
            .map(|s| self.interner.intern_id(s))
            .collect::<Vec<_>>();

        let path = Path::from_parts(parts);
        // TODO: Error when main function is not present
        let id = self.globals.iter().position(|p| *p == &path).unwrap();

        let mut instructions = self
            .globals
            .iter()
            .flat_map(|path| self.names[path].clone())
            .collect::<Vec<_>>();

        // Patch global name instructions
        for instruction in instructions.iter_mut() {
            if let Instruction::GetAbsolutePath(path) = instruction {
                let order = self.globals.iter().position(|p| *p == path).unwrap();
                *instruction = Instruction::GetAbsolute(order);
            }
        }

        for lambda in self.constant_pool.lambdas_mut() {
            for instruction in lambda.iter_mut() {
                if let Instruction::GetAbsolutePath(path) = instruction {
                    let order = self.globals.iter().position(|p| *p == path).unwrap();
                    *instruction = Instruction::GetAbsolute(order);
                }
            }
        }

        // TODO: Remove this instruction
        instructions.push(Instruction::String(0));
        instructions.push(Instruction::GetAbsolute(id));
        // TODO: Enforce main symbol to have an arrow type
        instructions.push(Instruction::Call);

        (instructions, self.constant_pool)
    }

    fn collect_names(&mut self, module: &'anf anf::Module<Resolved>) {
        for definition in module.definitions() {
            if let anf::Definition::Name(name) = definition {
                // self.names.insert(name.path(), vec![]);
                self.name_anfs.insert(name.path(), name.expression());
            }

            if let anf::Definition::Structure(structure) = definition {
                for (order, constructor) in structure.constructors().iter().enumerate() {
                    let instructions = vec![Instruction::Constructor(order, constructor.arity())];
                    self.names.insert(constructor.path(), instructions);
                    self.globals.push(constructor.path());
                }
            }
        }
    }

    pub fn module(&mut self, module: &'anf anf::Module<Resolved>) {
        for definition in module.definitions() {
            if let anf::Definition::Name(name) = definition
                && !self.names.contains_key(name.path())
            {
                self.names.insert(name.path(), vec![]);
                let code = self.expression(name.expression());
                self.names.insert(name.path(), code);
                self.globals.push(name.path());
            }
        }
    }

    pub fn compile(
        mut self,
        expression: &'anf anf::Expression<Resolved>,
    ) -> (Vec<Instruction>, ConstantPool) {
        (self.expression(expression), self.constant_pool)
    }

    #[must_use]
    fn expression(&mut self, expression: &'anf anf::Expression<Resolved>) -> Vec<Instruction> {
        match expression {
            anf::Expression::LetIn(letin) => self.letin(letin),
            anf::Expression::Application(application) => self.application(application),
            anf::Expression::Match(matchlet) => self.matchlet(matchlet),
            anf::Expression::Join(join) => self.join(join),
            anf::Expression::Jump(jump) => self.jump(jump),
            anf::Expression::Atom(atom) => self.atom(atom),
        }
    }

    #[must_use]
    fn atom(&mut self, atom: &'anf anf::Atom<Resolved>) -> Vec<Instruction> {
        match atom {
            anf::Atom::String(id) => self.string(*id),
            anf::Atom::Path(identifier) => self.identifier(identifier),
            anf::Atom::Lambda(lambda) => self.lambda(lambda),
        }
    }

    fn string(&mut self, intern_id: InternId) -> Vec<Instruction> {
        let offset = match self.interns.iter().position(|id| *id == intern_id) {
            Some(offset) => offset,
            None => {
                let string = self.interner.lookup(&intern_id).to_string();
                let offset = self.constant_pool.add_string(string);
                self.interns.push(intern_id);
                offset
            }
        };

        vec![Instruction::String(offset)]
    }

    fn identifier(&mut self, identifier: &'anf anf::atom::Path<Resolved>) -> Vec<Instruction> {
        match identifier.bound() {
            Bound::Local(id) => vec![Instruction::GetLocal(id.value())],
            Bound::Capture(id) => vec![Instruction::GetCapture(id.value())],
            Bound::Absolute(path) => {
                if !self.in_lambda && !self.names.contains_key(path) {
                    self.names.insert(path, vec![]);
                    let code = self.expression(self.name_anfs[path]);
                    self.names.insert(path, code);
                    self.globals.push(path);
                }

                vec![Instruction::GetAbsolutePath(path.clone())]
            }
        }
    }

    fn application(
        &mut self,
        application: &'anf anf::expression::Application<Resolved>,
    ) -> Vec<Instruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(application.argument()));
        instructions.extend(self.atom(application.function()));
        instructions.push(Instruction::Call);
        instructions.extend(self.expression(application.expression()));

        instructions
    }

    fn matchlet(&mut self, matchlet: &'anf anf::expression::MatchAs<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        for branch in matchlet.branches() {
            instructions.extend(self.atom(matchlet.expression()));

            match branch.pattern() {
                pattern::Pattern::Any(_) => {
                    instructions.extend(self.expression(branch.expression()));
                }
                pattern::Pattern::String(string) => {
                    instructions.extend(self.string(*string));
                    instructions.push(Instruction::StringEquals);
                    let expression = self.expression(branch.expression());
                    instructions.push(Instruction::SkipIfFalse(expression.len()));
                    instructions.extend(expression);
                }
                pattern::Pattern::Structure(structure) => {
                    instructions.push(Instruction::StructurePatternMatch(structure.clone()));
                    let mut ins = vec![];
                    ins.extend(self.atom(matchlet.expression()));
                    ins.push(Instruction::PatternLocals(pattern::Pattern::Structure(
                        structure.clone(),
                    )));
                    ins.extend(self.expression(branch.expression()));
                    instructions.push(Instruction::SkipIfFalse(ins.len()));
                    instructions.extend(ins);
                }
            }
        }

        instructions
    }

    fn join(&mut self, join: &'anf anf::expression::Join<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        let mut join_instructions = self.expression(join.join());

        let len = join_instructions.len();
        for (index, instruction) in join_instructions.iter_mut().enumerate() {
            if let Instruction::Jump(label) = instruction
                && *label == join.label()
            {
                *instruction = Instruction::Skip(len - (index + 1))
            }
        }

        instructions.push(Instruction::SetBase);
        instructions.extend(join_instructions);
        instructions.push(Instruction::Truncate);
        instructions.extend(self.expression(join.expression()));

        instructions
    }

    fn jump(&mut self, jump: &'anf anf::expression::Jump<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(jump.expression()));
        instructions.push(Instruction::Jump(jump.to()));

        instructions
    }

    fn lambda(&mut self, lambda: &'anf anf::atom::Lambda<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        let old_in_lambda = self.in_lambda;
        self.in_lambda = true;
        instructions.extend(self.expression(lambda.expression()));
        instructions.push(Instruction::Return);
        self.in_lambda = old_in_lambda;

        let id = self.constant_pool.add_lambda(instructions);

        vec![Instruction::MakeLambda(id, lambda.captures().to_vec())]
    }

    fn letin(&mut self, letin: &'anf anf::expression::LetIn<Resolved>) -> Vec<Instruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(letin.variable_expression()));
        instructions.extend(self.expression(letin.return_expression()));

        instructions
    }
}

pub struct ConstantPool {
    strings: Vec<String>,
    lambdas: Vec<Vec<Instruction>>,
}

impl ConstantPool {
    fn new() -> Self {
        Self {
            strings: Vec::new(),
            lambdas: Vec::new(),
        }
    }

    pub fn strings(&self) -> &[String] {
        &self.strings
    }

    pub fn lambdas(&self) -> &[Vec<Instruction>] {
        &self.lambdas
    }

    pub fn lambdas_mut(&mut self) -> &mut Vec<Vec<Instruction>> {
        &mut self.lambdas
    }

    fn add_string(&mut self, string: String) -> usize {
        let id = self.strings.len();
        self.strings.push(string);
        id
    }

    fn add_lambda(&mut self, lambda: Vec<Instruction>) -> usize {
        let id = self.lambdas.len();
        self.lambdas.push(lambda);
        id
    }
}
