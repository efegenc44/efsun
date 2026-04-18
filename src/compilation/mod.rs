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

/// Compiles ANF to high-level VM instructions
pub struct Compiler<'interner, 'anf> {
    /// Local interned string ordering because in the global
    ///   ordering strings does not have to be sequantial
    ///   because of identifiers
    interns: Vec<InternId>,
    /// Pool for constant values
    constant_pool: ConstantPool<PreInstruction>,
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
    /// Interner to retrieve strings
    interner: &'interner Interner,
}

impl<'interner, 'anf> Compiler<'interner, 'anf> {
    pub fn new(interner: &'interner Interner) -> Self {
        Self {
            interns: Vec::new(),
            constant_pool: ConstantPool::new(),
            name_anfs: HashMap::new(),
            names: HashMap::new(),
            globals: Globals::new(),
            in_lambda: false,
            interner,
        }
    }

    fn replace_in_lambda(&mut self, value: bool) -> bool {
        let old = self.in_lambda;
        self.in_lambda = value;
        old
    }

    pub fn program(
        mut self,
        program: &'anf anf::Program<Resolved>,
    ) -> (Vec<Instruction>, ConstantPool<Instruction>) {
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
            .flat_map(|path| self.names[path].clone())
            .collect::<Vec<_>>();

        let mut instructions = Self::patch_instructions(&self.globals, pre_instructions);

        let lambdas = self
            .constant_pool
            .lambdas
            .into_iter()
            .map(|lambda| Self::patch_instructions(&self.globals, lambda))
            .collect::<Vec<_>>();

        let pool = ConstantPool {
            strings: self.constant_pool.strings,
            lambdas,
        };

        instructions.push(Instruction::Unit);
        instructions.push(Instruction::GetAbsolute(id));
        // TODO: Enforce main symbol to have an arrow type
        instructions.push(Instruction::Call);

        (instructions, pool)
    }

    fn patch_instructions(
        globals: &Globals<'anf>,
        pre_instructions: Vec<PreInstruction>,
    ) -> Vec<Instruction> {
        let mut instructions = vec![];

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
                let code = self.expression(name.expression());
                self.names.insert(name.path(), code);
                self.globals.push(name.path());
            }
        }
    }

    pub fn compile(
        mut self,
        expression: &'anf anf::Expression<Resolved>,
    ) -> (Vec<Instruction>, ConstantPool<Instruction>) {
        let code = self.expression(expression);
        (
            Self::patch_instructions(&self.globals, code),
            self.constant_pool.patch_lambdas(&self.globals),
        )
    }

    #[must_use]
    fn expression(&mut self, expression: &'anf anf::Expression<Resolved>) -> Vec<PreInstruction> {
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
    fn atom(&mut self, atom: &'anf anf::Atom<Resolved>) -> Vec<PreInstruction> {
        match atom {
            anf::Atom::String(id) => self.string(*id),
            anf::Atom::Path(path) => self.path(path),
            anf::Atom::Lambda(lambda) => self.lambda(lambda),
        }
    }

    fn string(&mut self, intern_id: InternId) -> Vec<PreInstruction> {
        let offset = match self.interns.iter().position(|id| *id == intern_id) {
            Some(offset) => offset,
            None => {
                let string = self.interner.lookup(&intern_id).to_string();
                let offset = self.constant_pool.add_string(string);
                self.interns.push(intern_id);
                offset
            }
        };

        vec![Instruction::String(offset).into()]
    }

    fn path(&mut self, identifier: &'anf anf::atom::Path<Resolved>) -> Vec<PreInstruction> {
        let instruction = match identifier.bound() {
            Bound::Local(id) => Instruction::GetLocal(id.value()).into(),
            Bound::Capture(id) => Instruction::GetCapture(id.value()).into(),
            Bound::Absolute(path) => {
                if !self.in_lambda && !self.globals.pushed(path) {
                    let code = self.expression(self.name_anfs[path]);
                    self.names.insert(path, code);
                    self.globals.push(path);
                }

                Placeholder::GetAbsolute(path.clone()).into()
            }
        };

        vec![instruction]
    }

    fn application(
        &mut self,
        application: &'anf anf::expression::Application<Resolved>,
    ) -> Vec<PreInstruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(application.argument()));
        instructions.extend(self.atom(application.function()));
        instructions.push(Instruction::Call.into());
        instructions.extend(self.expression(application.expression()));

        instructions
    }

    fn matchlet(
        &mut self,
        matchlet: &'anf anf::expression::MatchAs<Resolved>,
    ) -> Vec<PreInstruction> {
        let mut instructions = vec![];

        let matched = self.atom(matchlet.expression());

        for branch in matchlet.branches() {
            instructions.extend(self.pattern_equality(&matched, branch.pattern()));

            let mut branch_code = self.pattern_locals(&matched, branch.pattern());
            branch_code.extend(self.expression(branch.expression()));

            instructions.push(Placeholder::SkipIfFalse(branch_code.len()).into());
            instructions.extend(branch_code);
        }

        instructions
    }

    fn pattern_equality(
        &mut self,
        matched: &[PreInstruction],
        pattern: &Pattern<Renamed>,
    ) -> Vec<PreInstruction> {
        let mut instructions = vec![];

        match pattern {
            Pattern::Any(_) => instructions.push(Instruction::Bool(true).into()),
            Pattern::Structure(structure) => {
                instructions.extend(matched.iter().cloned());
                instructions.push(Instruction::TagEquals(structure.order()).into());

                for (index, argument) in structure.arguments().iter().enumerate() {
                    let mut argument_code = matched.to_vec();
                    argument_code.push(Instruction::GetArgument(index).into());
                    instructions.extend(self.pattern_equality(&argument_code, argument.data()));
                    instructions.push(Instruction::LogicalAnd.into());
                }
            }
            Pattern::String(string) => {
                instructions.extend(matched.iter().cloned());
                instructions.extend(self.string(*string));
                instructions.push(Instruction::StringEquals.into());
            }
        }

        instructions
    }

    fn pattern_locals(
        &mut self,
        matched: &[PreInstruction],
        pattern: &Pattern<Renamed>,
    ) -> Vec<PreInstruction> {
        let mut instructions = vec![];

        match pattern {
            Pattern::Any(_) => instructions.extend(matched.iter().cloned()),
            Pattern::Structure(structure) => {
                for (index, argument) in structure.arguments().iter().enumerate() {
                    let mut argument_code = matched.to_vec();
                    argument_code.push(Instruction::GetArgument(index).into());
                    instructions.extend(self.pattern_locals(&argument_code, argument.data()));
                }
            }
            Pattern::String(_) => (),
        }

        instructions
    }

    fn join(&mut self, join: &'anf anf::expression::Join<Resolved>) -> Vec<PreInstruction> {
        let mut instructions = vec![];

        let mut join_instructions = self.expression(join.join());

        let len = join_instructions.len();
        for (index, instruction) in join_instructions.iter_mut().enumerate() {
            if let PreInstruction::Placeholder(Placeholder::Jump(label)) = instruction
                && *label == join.label()
            {
                *instruction = Placeholder::Skip(len - (index + 1)).into()
            }
        }

        instructions.push(Instruction::SetBase.into());
        instructions.extend(join_instructions);
        instructions.push(Instruction::Truncate.into());
        instructions.extend(self.expression(join.expression()));

        instructions
    }

    fn jump(&mut self, jump: &'anf anf::expression::Jump<Resolved>) -> Vec<PreInstruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(jump.expression()));
        instructions.push(Placeholder::Jump(jump.to()).into());

        instructions
    }

    fn lambda(&mut self, lambda: &'anf anf::atom::Lambda<Resolved>) -> Vec<PreInstruction> {
        let mut instructions = vec![];

        let old = self.replace_in_lambda(true);
        instructions.extend(self.expression(lambda.expression()));
        instructions.push(Instruction::Return.into());
        self.replace_in_lambda(old);

        let id = self.constant_pool.add_lambda(instructions);

        vec![Instruction::MakeLambda(id, lambda.captures().to_vec()).into()]
    }

    fn letin(&mut self, letin: &'anf anf::expression::LetIn<Resolved>) -> Vec<PreInstruction> {
        let mut instructions = vec![];

        instructions.extend(self.atom(letin.variable_expression()));
        instructions.extend(self.expression(letin.return_expression()));

        instructions
    }
}

pub struct ConstantPool<I> {
    strings: Vec<String>,
    lambdas: Vec<Vec<I>>,
}

impl<I> ConstantPool<I> {
    fn new() -> Self {
        Self {
            strings: Vec::new(),
            lambdas: Vec::new(),
        }
    }

    pub fn strings(&self) -> &[String] {
        &self.strings
    }

    pub fn lambdas(&self) -> &[Vec<I>] {
        &self.lambdas
    }

    fn add_string(&mut self, string: String) -> usize {
        let id = self.strings.len();
        self.strings.push(string);
        id
    }

    fn add_lambda(&mut self, lambda: Vec<I>) -> usize {
        let id = self.lambdas.len();
        self.lambdas.push(lambda);
        id
    }
}

impl ConstantPool<PreInstruction> {
    fn patch_lambdas(self, globals: &Globals<'_>) -> ConstantPool<Instruction> {
        let lambdas = self
            .lambdas
            .into_iter()
            .map(|lambda| Compiler::patch_instructions(globals, lambda))
            .collect();

        ConstantPool {
            strings: self.strings,
            lambdas,
        }
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
