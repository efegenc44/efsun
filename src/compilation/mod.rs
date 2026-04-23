pub mod anf;
pub mod instruction;

use core::slice;
use std::collections::HashMap;

use crate::{
    compilation::instruction::{Placeholder, PreInstruction},
    interner::{InternId, Interner},
    parse::pattern::Pattern,
    resolution::bound::{Bound, Path},
    state::{Renamed, Resolved},
};

use instruction::Instruction;

macro_rules! seperate {
    ($self:expr, $e:expr) => {{
        let old = $self.swap_out(Some(Vec::new()));
        $e;
        $self.swap_out(old).unwrap()
    }};
}

/// Compiles ANF to high-level VM instructions
pub struct Compiler<'interner, 'anf> {
    /// Local interned string ordering because in the global
    ///   ordering strings does not have to be sequantial
    ///   because of identifiers
    interns: Vec<InternId>,
    /// Constant string values
    strings: Vec<String>,
    /// Lambdas with pre instructions
    lambdas: Vec<Vec<PreInstruction>>,
    /// Map from global names to their ANF expression
    ///   Used for compiling cyclic references
    name_anfs: HashMap<&'anf Path, &'anf anf::Expression<Resolved>>,
    /// Global names and their instructions
    names: HashMap<&'anf Path, Vec<PreInstruction>>,
    /// Compilation order for global names
    ///   Order is determined at compile time because language
    ///   does not require strict lexical definition order
    globals: Globals<'anf>,
    /// True if currently compiling a lambda
    in_lambda: bool,
    /// Current output to emit into
    out: Option<Vec<PreInstruction>>,
    /// Interner to retrieve strings
    interner: &'interner Interner,
}

impl<'interner, 'anf> Compiler<'interner, 'anf> {
    pub fn new(interner: &'interner Interner) -> Self {
        Self {
            interns: Vec::new(),
            strings: Vec::new(),
            lambdas: Vec::new(),
            name_anfs: HashMap::new(),
            names: HashMap::new(),
            globals: Globals::new(),
            in_lambda: false,
            out: None,
            interner,
        }
    }

    fn replace_in_lambda(&mut self, value: bool) -> bool {
        let old = self.in_lambda;
        self.in_lambda = value;
        old
    }

    fn emit(&mut self, pre_instruction: PreInstruction) {
        self.out.as_mut().unwrap().push(pre_instruction);
    }

    fn extend<E>(&mut self, pre_instructions: E)
    where
        E: Iterator<Item = PreInstruction>,
    {
        self.out.as_mut().unwrap().extend(pre_instructions);
    }

    fn swap_out(&mut self, new_out: Option<Vec<PreInstruction>>) -> Option<Vec<PreInstruction>> {
        std::mem::replace(&mut self.out, new_out)
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

        let id = self.globals.order(&path);

        let pre_instructions = self
            .globals
            .iter()
            .flat_map(|path| self.names.remove(path).unwrap())
            .collect::<Vec<_>>();

        let mut instructions = Self::patch_instructions(&self.globals, pre_instructions);

        let lambdas = self
            .lambdas
            .into_iter()
            .map(|lambda| Self::patch_instructions(&self.globals, lambda))
            .collect::<Vec<_>>();

        instructions.push(Instruction::Unit);
        instructions.push(Instruction::GetAbsolute(id));
        // TODO: Enforce main symbol to have an arrow type
        instructions.push(Instruction::Call);

        (instructions, ConstantPool::new(self.strings, lambdas))
    }

    fn patch_instructions(
        globals: &Globals<'anf>,
        pre_instructions: Vec<PreInstruction>,
    ) -> Vec<Instruction> {
        let mut instructions = Vec::with_capacity(pre_instructions.capacity());

        for (index, pre_instruction) in pre_instructions.into_iter().enumerate() {
            let instruction = match pre_instruction {
                PreInstruction::Placeholder(Placeholder::GetAbsolute(path)) => {
                    Instruction::GetAbsolute(globals.order(&path))
                }
                PreInstruction::Placeholder(Placeholder::Skip(n)) => {
                    Instruction::Jump(index + 1 + n)
                }
                PreInstruction::Placeholder(Placeholder::SkipIfFalse(n)) => {
                    Instruction::JumpIfFalse(index + 1 + n)
                }
                PreInstruction::Placeholder(Placeholder::Jump(_)) => {
                    // Jump is a 2-deep placeholder, meaning it will first
                    //   be patched into Skip, then Skip will be patched
                    //   into Instruction::Jump. This unreachable state can
                    //   be removed with one more enum type, but it is a bit
                    //   unnecessary since right now there is only Jump
                    panic!("This case should be handeld at join()");
                }
                PreInstruction::Instruction(instruction) => instruction,
            };

            instructions.push(instruction)
        }

        instructions
    }

    fn collect_names(&mut self, module: &'anf anf::Module<Resolved>) {
        for definition in module.definitions() {
            if let anf::Definition::Name(name) = definition {
                self.name_anfs.insert(name.path(), name.expression());
            }

            if let anf::Definition::Structure(structure) = definition {
                for (order, constructor) in structure.constructors().iter().enumerate() {
                    let code = vec![Instruction::Constructor(order, constructor.arity()).into()];
                    self.names.insert(constructor.path(), code);
                    self.globals.push(constructor.path());
                }
            }
        }
    }

    pub fn module(&mut self, module: &'anf anf::Module<Resolved>) {
        for definition in module.definitions() {
            if let anf::Definition::Name(name) = definition
                && !self.globals.pushed(name.path())
            {
                let code = seperate!(self, self.expression(name.expression()));
                self.names.insert(name.path(), code);
                self.globals.push(name.path());
            }
        }
    }

    pub fn compile(
        mut self,
        expression: &'anf anf::Expression<Resolved>,
    ) -> (Vec<Instruction>, ConstantPool) {
        let code = seperate!(self, self.expression(expression));

        let lambdas = self
            .lambdas
            .into_iter()
            .map(|lambda| Self::patch_instructions(&self.globals, lambda))
            .collect::<Vec<_>>();

        (
            Self::patch_instructions(&self.globals, code),
            ConstantPool::new(self.strings, lambdas),
        )
    }

    fn expression(&mut self, expression: &'anf anf::Expression<Resolved>) {
        match expression {
            anf::Expression::LetIn(letin) => self.letin(letin),
            anf::Expression::Application(application) => self.application(application),
            anf::Expression::Match(matchlet) => self.matchas(matchlet),
            anf::Expression::Join(join) => self.join(join),
            anf::Expression::Jump(jump) => self.jump(jump),
            anf::Expression::Atom(atom) => self.atom(atom),
        }
    }

    fn atom(&mut self, atom: &'anf anf::Atom<Resolved>) {
        match atom {
            anf::Atom::String(id) => self.string(*id),
            anf::Atom::Path(path) => self.path(path),
            anf::Atom::Lambda(lambda) => self.lambda(lambda),
        }
    }

    fn string(&mut self, intern_id: InternId) {
        let offset = match self.interns.iter().position(|id| *id == intern_id) {
            Some(offset) => offset,
            None => {
                let string = self.interner.lookup(&intern_id).to_string();
                let offset = self.strings.len();
                self.strings.push(string);
                self.interns.push(intern_id);
                offset
            }
        };

        self.emit(Instruction::String(offset).into())
    }

    fn path(&mut self, identifier: &'anf anf::atom::Path<Resolved>) {
        let instruction = match identifier.bound() {
            Bound::Local(id) => Instruction::GetLocal(id.value()).into(),
            Bound::Capture(id) => Instruction::GetCapture(id.value()).into(),
            Bound::Absolute(path) => {
                if !self.in_lambda && !self.globals.pushed(path) {
                    let code = seperate!(self, self.expression(self.name_anfs[path]));
                    self.names.insert(path, code);
                    self.globals.push(path);
                }

                Placeholder::GetAbsolute(path.clone()).into()
            }
        };

        self.emit(instruction)
    }

    fn application(&mut self, application: &'anf anf::expression::Application<Resolved>) {
        self.atom(application.argument());
        self.atom(application.function());
        self.emit(Instruction::Call.into());
        self.expression(application.expression());
    }

    fn matchas(&mut self, matchas: &'anf anf::expression::MatchAs<Resolved>) {
        let mut matched = seperate!(self, self.atom(matchas.expression()));

        for branch in matchas.branches() {
            self.pattern_equality(&mut matched, branch.pattern());

            let local_code = seperate!(self, self.pattern_locals(&mut matched, branch.pattern()));
            let branch_code = seperate!(self, self.expression(branch.expression()));

            self.emit(Placeholder::SkipIfFalse(local_code.len() + branch_code.len()).into());
            self.extend(local_code.into_iter());
            self.extend(branch_code.into_iter());
        }
    }

    fn pattern_equality(&mut self, matched: &mut Vec<PreInstruction>, pattern: &Pattern<Renamed>) {
        match pattern {
            Pattern::Any(_) => self.emit(Instruction::Bool(true).into()),
            Pattern::Structure(structure) => {
                self.extend(matched.iter().cloned());
                self.emit(Instruction::TagEquals(structure.order()).into());

                for (index, argument) in structure.arguments().iter().enumerate() {
                    matched.push(Instruction::GetArgument(index).into());
                    self.pattern_equality(matched, argument.data());
                    self.emit(Instruction::LogicalAnd.into());
                    matched.pop();
                }
            }
            Pattern::String(string) => {
                self.extend(matched.iter().cloned());
                self.string(*string);
                self.emit(Instruction::StringEquals.into());
            }
        }
    }

    fn pattern_locals(&mut self, matched: &mut Vec<PreInstruction>, pattern: &Pattern<Renamed>) {
        match pattern {
            Pattern::Any(_) => self.extend(matched.iter().cloned()),
            Pattern::Structure(structure) => {
                for (index, argument) in structure.arguments().iter().enumerate() {
                    matched.push(Instruction::GetArgument(index).into());
                    self.pattern_locals(matched, argument.data());
                    matched.pop();
                }
            }
            Pattern::String(_) => (),
        }
    }

    fn join(&mut self, join: &'anf anf::expression::Join<Resolved>) {
        let mut join_instructions = seperate!(self, self.expression(join.join()));

        let len = join_instructions.len();
        for (index, instruction) in join_instructions.iter_mut().enumerate() {
            if let PreInstruction::Placeholder(Placeholder::Jump(label)) = instruction
                && *label == join.label()
            {
                *instruction = Placeholder::Skip(len - (index + 1)).into()
            }
        }

        self.emit(Instruction::SetBase.into());
        self.extend(join_instructions.into_iter());
        self.emit(Instruction::Truncate.into());
        self.expression(join.expression());
    }

    fn jump(&mut self, jump: &'anf anf::expression::Jump<Resolved>) {
        self.atom(jump.expression());
        self.emit(Placeholder::Jump(jump.to()).into());
    }

    fn lambda(&mut self, lambda: &'anf anf::atom::Lambda<Resolved>) {
        let old = self.replace_in_lambda(true);
        let lambda_code = seperate!(self, {
            self.expression(lambda.expression());
            self.emit(Instruction::Return.into());
        });
        self.replace_in_lambda(old);

        let id = self.lambdas.len();
        self.lambdas.push(lambda_code);

        self.emit(Instruction::MakeLambda(id, lambda.captures().to_vec()).into())
    }

    fn letin(&mut self, letin: &'anf anf::expression::LetIn<Resolved>) {
        self.atom(letin.variable_expression());
        self.expression(letin.return_expression());
    }
}

pub struct ConstantPool {
    strings: Vec<String>,
    lambdas: Vec<Vec<Instruction>>,
}

impl ConstantPool {
    fn new(strings: Vec<String>, lambdas: Vec<Vec<Instruction>>) -> Self {
        Self { strings, lambdas }
    }

    pub fn strings(&self) -> &[String] {
        &self.strings
    }

    pub fn lambdas(&self) -> &[Vec<Instruction>] {
        &self.lambdas
    }
}

struct Globals<'anf> {
    array: Vec<&'anf Path>,
    table: HashMap<&'anf Path, usize>,
}

impl<'anf> Globals<'anf> {
    fn new() -> Self {
        Self {
            array: Vec::new(),
            table: HashMap::new(),
        }
    }

    fn push(&mut self, path: &'anf Path) -> usize {
        let order = self.array.len();
        self.array.push(path);
        self.table.insert(path, order);
        order
    }

    fn order(&self, path: &'anf Path) -> usize {
        *self.table.get(path).unwrap()
    }

    fn pushed(&self, path: &'anf Path) -> bool {
        self.table.contains_key(path)
    }

    fn iter(&self) -> slice::Iter<'_, &'anf Path> {
        self.array.iter()
    }
}
