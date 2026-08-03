pub mod anf;
pub mod instruction;

use core::slice;
use std::collections::{HashMap, HashSet};

use crate::{
    compilation::instruction::{Placeholder, PreInstruction},
    interner::{InternId, Interner},
    metadata::{CheckFlag, Metadata},
    parse::pattern::Pattern,
    resolution::{
        ANFResolved, Edge, Graph,
        bound::{Bound, Path},
    },
};

use instruction::Instruction;

macro_rules! seperate {
    ($self:expr, $e:expr) => {{
        let old = $self.swap_out(Some(Vec::new()));
        $e;
        $self.swap_out(old).unwrap()
    }};
}

macro_rules! scoped_expression {
    ($n:expr, $self:expr, $e:expr) => {
        let local_count = $n;
        $self.local_count += local_count;
        $self.expression($e);
        $self.local_count -= local_count;
    };
}

/// Compiles ANF to high-level VM instructions
pub struct Compiler<'interner, 'anf, 'metadata, 'dependencies, 'cycles> {
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
    name_definition_anfs: HashMap<&'metadata Path, &'anf anf::definition::Name>,
    /// Global names and their instructions
    names: HashMap<&'metadata Path, Vec<PreInstruction>>,
    /// Compilation order for global names
    ///   Order is determined at compile time because language
    ///   does not require strict lexical definition order
    globals: Globals<'metadata>,
    /// Current output to emit into
    out: Option<Vec<PreInstruction>>,
    /// Interner to retrieve strings
    interner: &'interner Interner,
    /// Number of local variables on the stack
    ///   Used for popping the scope of the branching path when jumping
    ///   to a join point
    local_count: usize,
    /// Dependency graph
    dependencies: &'dependencies Graph<Path>,
    /// Allowed cycles due to lambdas
    allowed_cycles: &'cycles HashSet<Edge<Path>>,
    /// Crossed allowed cycles
    crossed_cycles: HashSet<Edge<&'dependencies Path>>,
    /// Metadata
    metadata: &'metadata Metadata<ANFResolved>,
}

impl<'interner, 'anf, 'metadata, 'dependencies, 'cycles>
    Compiler<'interner, 'anf, 'metadata, 'dependencies, 'cycles>
where
    'metadata: 'anf,
    'metadata: 'dependencies,
{
    pub fn new(
        interner: &'interner Interner,
        metadata: &'metadata Metadata<ANFResolved>,
        dependencies: &'dependencies Graph<Path>,
        allowed_cycles: &'cycles HashSet<Edge<Path>>,
    ) -> Self {
        Self {
            interns: Vec::new(),
            strings: Vec::new(),
            lambdas: Vec::new(),
            name_definition_anfs: HashMap::new(),
            names: HashMap::new(),
            globals: Globals::new(),
            out: None,
            interner,
            local_count: 0,
            dependencies,
            allowed_cycles,
            crossed_cycles: HashSet::new(),
            metadata,
        }
    }

    fn reset_local_counter(&mut self) -> usize {
        let old = self.local_count;
        self.local_count = 0;
        old
    }

    fn restore_local_counter(&mut self, local_count: usize) {
        assert!(
            self.local_count == 0,
            "An expression should not have left-open scope"
        );

        self.local_count = local_count;
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

    pub fn program(mut self, program: &'anf anf::Program) -> (Vec<Instruction>, ConstantPool) {
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

        let mut pre_instructions = self
            .globals
            .iter()
            .flat_map(|path| self.names.remove(path).unwrap())
            .collect::<Vec<_>>();

        // Return the `main` symbol
        pre_instructions.push(Instruction::GetAbsolute(id).into());
        pre_instructions.push(Instruction::Halt.into());

        let (pre_instructions, lambda_addresses) =
            Self::merge_lambdas(pre_instructions, self.lambdas);

        let instructions =
            Self::patch_instructions(&self.globals, &lambda_addresses, pre_instructions);

        (instructions, ConstantPool::new(self.strings))
    }

    fn merge_lambdas(
        mut pre_instructions: Vec<PreInstruction>,
        lambdas: Vec<Vec<PreInstruction>>,
    ) -> (Vec<PreInstruction>, Vec<usize>) {
        let mut lambda_addresses = Vec::with_capacity(lambdas.len());
        for lambda in lambdas {
            lambda_addresses.push(pre_instructions.len());
            pre_instructions.extend(lambda);
        }

        (pre_instructions, lambda_addresses)
    }

    fn patch_instructions(
        globals: &Globals<'anf>,
        lambda_addresses: &[usize],
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
                    unreachable!("This case should be handeld at join()");
                }
                PreInstruction::Placeholder(Placeholder::MakeLambda(id, arity)) => {
                    Instruction::MakeLambda(lambda_addresses[id], arity)
                }
                PreInstruction::Placeholder(Placeholder::PushFrame(offset)) => {
                    Instruction::PushFrame(index + 1 + offset)
                }
                PreInstruction::Instruction(instruction) => instruction,
            };

            instructions.push(instruction)
        }

        instructions
    }

    fn collect_names(&mut self, module: &'anf anf::Module) {
        for definition in module.definitions() {
            if let anf::Definition::Name(name) = definition {
                let path = &self.metadata[name.path_id];
                self.name_definition_anfs.insert(path, name);
            }

            if let anf::Definition::Structure(structure) = definition {
                for (order, constructor) in structure.constructors.iter().enumerate() {
                    let name_offset = self.string_offset(constructor.name);

                    let instruction = if constructor.arity == 0 {
                        Instruction::MakeStructure(name_offset, order, constructor.arity).into()
                    } else {
                        let id = self.lambdas.len();
                        self.lambdas.push(vec![
                            Instruction::MakeStructure(name_offset, order, constructor.arity)
                                .into(),
                            Instruction::Return.into(),
                        ]);

                        Placeholder::MakeLambda(id, constructor.arity).into()
                    };

                    let path = &self.metadata[constructor.path_id];

                    self.names.insert(path, vec![instruction]);
                    self.globals.push(path);
                }
            }
        }
    }

    pub fn module(&mut self, module: &'anf anf::Module) {
        for definition in module.definitions() {
            if let anf::Definition::Name(name) = definition {
                self.name_definition(name);
            }
        }
    }

    fn name_definition(&mut self, name_definition: &'anf anf::definition::Name) {
        let path = &self.metadata[name_definition.path_id];

        self.visit_dependencies(path);

        if !self.globals.pushed(path) {
            let code = seperate!(self, {
                self.emit(Instruction::SetBase(0).into());
                self.expression(&name_definition.expression);
                self.emit(Instruction::PopBase.into());
            });

            self.names.insert(path, code);
            self.globals.push(path);
        }
    }

    fn visit_dependencies(&mut self, path: &'metadata Path) {
        for (dependency, _) in &self.dependencies[path] {
            // TODO: Path interning
            if self
                .allowed_cycles
                .contains(&(path.clone(), dependency.clone()))
            {
                let not_crossed = self.crossed_cycles.insert((path, dependency));
                if not_crossed {
                    self.name_definition(self.name_definition_anfs[dependency]);
                }
            } else {
                self.name_definition(self.name_definition_anfs[dependency]);
            }
        }
    }

    pub fn compile(
        mut self,
        expression: &'anf anf::Expression,
    ) -> (Vec<Instruction>, ConstantPool) {
        let code = seperate!(self, self.expression(expression));
        let (code, lambda_addresses) = Self::merge_lambdas(code, self.lambdas);

        (
            Self::patch_instructions(&self.globals, &lambda_addresses, code),
            ConstantPool::new(self.strings),
        )
    }

    fn expression(&mut self, expression: &'anf anf::Expression) {
        match expression {
            anf::Expression::LetIn(letin) => self.letin(letin),
            anf::Expression::Application(application) => self.application(application),
            anf::Expression::MatchAs(matchlet) => self.matchas(matchlet),
            anf::Expression::Join(join) => self.join(join),
            anf::Expression::Jump(jump) => self.jump(jump),
            anf::Expression::Atom(atom) => self.atom(atom),
        }
    }

    fn atom(&mut self, atom: &'anf anf::Atom) {
        match atom {
            anf::Atom::String(id) => self.string(*id),
            anf::Atom::Path(path) => self.path(path),
            anf::Atom::Lambda(lambda) => self.lambda(lambda),
        }
    }

    fn string_offset(&mut self, intern_id: InternId) -> usize {
        match self.interns.iter().position(|id| *id == intern_id) {
            Some(offset) => offset,
            None => {
                let string = self.interner.lookup(&intern_id).to_string();
                let offset = self.strings.len();
                self.strings.push(string);
                self.interns.push(intern_id);
                offset
            }
        }
    }

    fn string(&mut self, intern_id: InternId) {
        let offset = self.string_offset(intern_id);
        self.emit(Instruction::String(offset).into())
    }

    fn path(&mut self, path: &'anf anf::atom::Path) {
        let bound = &self.metadata[path.anf_bound_id];

        let instruction = match bound {
            Bound::Local(id) => Instruction::GetLocal(id.value()).into(),
            Bound::Capture(id) => Instruction::GetCapture(id.value()).into(),
            Bound::Absolute(path) => Placeholder::GetAbsolute(path.clone()).into(),
        };

        self.emit(instruction)
    }

    fn application(&mut self, application: &'anf anf::expression::Application) {
        let is_tail_call = self.metadata.check(application.tail_call_id);

        let code = seperate!(self, {
            // NOTE: Reverse is to preserve left associative application
            //   semantics that is forced by previous pipline steps
            for (i, argument) in application.arguments.iter().rev().enumerate() {
                self.atom(argument);
                if is_tail_call {
                    // NOTE: CopyIntoLocal does not pop so we will always have enough
                    //   stack size if tail call needs larger frame
                    self.emit(Instruction::CopyIntoLocal(i).into());
                }
            }

            if is_tail_call {
                // NOTE: General tail call elimitination may require adjusting the frame size
                //   and by copying (not popping) new arguments into old frame, stack size
                //   never becomes less but always _at least one_ more. This trick eliminates
                //   the need for a runtime check to either allocate or truncate stack frame
                //   because only truncation is possible
                self.emit(Instruction::TruncateFrame(application.arguments.len()).into());
            }

            self.atom(&application.function);
            if !is_tail_call {
                self.emit(Instruction::SetBase(application.arguments.len() + 1).into());
            }
            self.emit(Instruction::Call(application.arguments.len()).into());
        });

        if !is_tail_call {
            self.emit(Placeholder::PushFrame(code.len()).into());
        }

        self.extend(code.into_iter());

        // TODO: Generalize optimization for immediate return of last produced local
        if !is_tail_call {
            scoped_expression!(1, self, &application.expression);
        }
    }

    fn matchas(&mut self, matchas: &'anf anf::expression::MatchAs) {
        let mut matched = seperate!(self, self.atom(&matchas.expression));

        for branch in &matchas.branches {
            self.pattern_equality(&mut matched, &branch.pattern);

            let local_code = seperate!(self, self.pattern_locals(&mut matched, &branch.pattern));

            let old = self.reset_local_counter();
            let branch_code = seperate!(
                self,
                scoped_expression!(branch.pattern.local_count(), self, &branch.expression)
            );
            self.restore_local_counter(old);

            self.emit(Placeholder::SkipIfFalse(local_code.len() + branch_code.len()).into());
            self.extend(local_code.into_iter());
            self.extend(branch_code.into_iter());
        }
    }

    fn pattern_equality(&mut self, matched: &mut Vec<PreInstruction>, pattern: &Pattern) {
        match pattern {
            Pattern::Any(_) => self.emit(Instruction::Bool(true).into()),
            Pattern::Structure(structure) => {
                self.extend(matched.iter().cloned());

                let tag = self.metadata[structure.structure_pattern_id].tag;

                self.emit(Instruction::TagEquals(tag).into());

                for (index, argument) in structure.arguments.iter().enumerate() {
                    matched.push(Instruction::GetArgument(index).into());
                    self.pattern_equality(matched, &argument.data);
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

    fn pattern_locals(&mut self, matched: &mut Vec<PreInstruction>, pattern: &Pattern) {
        match pattern {
            Pattern::Any(_) => self.extend(matched.iter().cloned()),
            Pattern::Structure(structure) => {
                for (index, argument) in structure.arguments.iter().enumerate() {
                    matched.push(Instruction::GetArgument(index).into());
                    self.pattern_locals(matched, &argument.data);
                    matched.pop();
                }
            }
            Pattern::String(_) => (),
        }
    }

    fn join(&mut self, join: &'anf anf::expression::Join) {
        let mut join_instructions = seperate!(self, self.expression(&join.join));

        let len = join_instructions.len();
        for (index, instruction) in join_instructions.iter_mut().enumerate() {
            if let PreInstruction::Placeholder(Placeholder::Jump(label)) = instruction
                && *label == join.label
            {
                *instruction = Placeholder::Skip(len - (index + 1)).into()
            }
        }

        self.extend(join_instructions.into_iter());
        scoped_expression!(1, self, &join.expression);
    }

    fn jump(&mut self, jump: &'anf anf::expression::Jump) {
        self.atom(&jump.expression);
        if self.local_count > 0 {
            self.emit(Instruction::PopScope(self.local_count).into());
        }
        self.emit(Placeholder::Jump(jump.to).into());
    }

    fn lambda(&mut self, lambda: &'anf anf::atom::Lambda) {
        let lambda_code = seperate!(self, {
            self.expression(&lambda.expression);
            self.emit(Instruction::Return.into());
        });

        let id = self.lambdas.len();
        self.lambdas.push(lambda_code);

        let artiy = lambda.variables.len();
        let capture = &self.metadata[lambda.anf_capture_id];

        self.emit(Placeholder::MakeLambda(id, artiy).into());

        if !capture.is_empty() {
            self.emit(Instruction::CaptureIntoLambda(capture.to_vec(), lambda.self_capture).into())
        }
    }

    fn letin(&mut self, letin: &'anf anf::expression::LetIn) {
        self.atom(&letin.variable_expression);
        scoped_expression!(1, self, &letin.return_expression);
    }
}

pub struct ConstantPool {
    strings: Vec<String>,
}

impl ConstantPool {
    fn new(strings: Vec<String>) -> Self {
        Self { strings }
    }

    pub fn strings(&self) -> &[String] {
        &self.strings
    }
}

struct Globals<'path> {
    array: Vec<&'path Path>,
    table: HashMap<&'path Path, usize>,
}

impl<'path> Globals<'path> {
    fn new() -> Self {
        Self {
            array: Vec::new(),
            table: HashMap::new(),
        }
    }

    fn push(&mut self, path: &'path Path) -> usize {
        let order = self.array.len();
        self.array.push(path);
        self.table.insert(path, order);
        order
    }

    fn order(&self, path: &'path Path) -> usize {
        *self.table.get(path).unwrap()
    }

    fn pushed(&self, path: &'path Path) -> bool {
        self.table.contains_key(path)
    }

    fn iter(&self) -> slice::Iter<'_, &'path Path> {
        self.array.iter()
    }
}
