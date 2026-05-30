use std::collections::HashMap;

use crate::{
    interner::InternId,
    resolution::{
        bound::{Bound, Capture, Path},
        renamer::UniqueName,
    },
};

/// AST and ANF Node Metadata
#[derive(Default)]
pub struct Metadata {
    bounds: HashMap<usize, Bound>,
    captures: HashMap<usize, Vec<Capture>>,
    structure_patterns: HashMap<usize, StructurePattern>,
    paths: HashMap<usize, Path>,
    unique_names: HashMap<usize, Option<UniqueName>>,
    anf_bounds: HashMap<usize, Bound>,
    anf_captures: HashMap<usize, Vec<Capture>>,
}

impl Metadata {
    pub fn set_bound(&mut self, id: usize, bound: Bound) {
        self.bounds.insert(id, bound);
    }

    pub fn get_bound(&self, id: usize) -> &Bound {
        &self.bounds[&id]
    }

    pub fn set_capture(&mut self, id: usize, capture: Vec<Capture>) {
        self.captures.insert(id, capture);
    }

    pub fn get_capture(&self, id: usize) -> &[Capture] {
        &self.captures[&id]
    }

    pub fn set_structure_pattern(&mut self, id: usize, structure_pattern: StructurePattern) {
        self.structure_patterns.insert(id, structure_pattern);
    }

    pub fn get_structure_pattern(&self, id: usize) -> &StructurePattern {
        &self.structure_patterns[&id]
    }

    pub fn set_path(&mut self, id: usize, path: Path) {
        self.paths.insert(id, path);
    }

    pub fn get_path(&self, id: usize) -> &Path {
        &self.paths[&id]
    }

    pub fn set_unique_name(&mut self, id: usize, unique_name: Option<UniqueName>) {
        self.unique_names.insert(id, unique_name);
    }

    pub fn get_unique_name(&self, id: usize) -> &Option<UniqueName> {
        &self.unique_names[&id]
    }

    pub fn set_anf_bound(&mut self, id: usize, bound: Bound) {
        self.anf_bounds.insert(id, bound);
    }

    pub fn get_anf_bound(&self, id: usize) -> &Bound {
        &self.anf_bounds[&id]
    }

    pub fn set_anf_capture(&mut self, id: usize, capture: Vec<Capture>) {
        self.anf_captures.insert(id, capture);
    }

    pub fn get_anf_capture(&self, id: usize) -> &[Capture] {
        &self.anf_captures[&id]
    }
}

pub struct StructurePattern {
    type_path: Path,
    constructor_name: InternId,
    tag: usize,
}

impl StructurePattern {
    pub fn new(type_path: Path, constructor_name: InternId, tag: usize) -> Self {
        Self {
            type_path,
            constructor_name,
            tag,
        }
    }

    pub fn type_path(&self) -> &Path {
        &self.type_path
    }

    pub fn constructor_name(&self) -> InternId {
        self.constructor_name
    }

    pub fn tag(&self) -> usize {
        self.tag
    }
}

#[derive(Clone, Copy, Default)]
pub struct IndexState {
    bound_id_counter: usize,
    capture_id_counter: usize,
    structure_pattern_id_counter: usize,
    path_id_counter: usize,
    unique_name_counter: usize,
}

impl IndexState {
    pub fn new_bound_id(&mut self) -> usize {
        let id = self.bound_id_counter;
        self.bound_id_counter += 1;
        id
    }

    pub fn new_capture_id(&mut self) -> usize {
        let id = self.capture_id_counter;
        self.capture_id_counter += 1;
        id
    }

    pub fn new_structure_pattern_id(&mut self) -> usize {
        let id = self.structure_pattern_id_counter;
        self.structure_pattern_id_counter += 1;
        id
    }

    pub fn new_path_id(&mut self) -> usize {
        let id = self.path_id_counter;
        self.path_id_counter += 1;
        id
    }

    pub fn new_unique_name_id(&mut self) -> usize {
        let id = self.unique_name_counter;
        self.unique_name_counter += 1;
        id
    }
}
