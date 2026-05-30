use crate::{interner::InternId, location::Located};

use super::Pattern;

#[derive(Clone)]
pub struct Structure {
    parts: Located<Vec<InternId>>,
    arguments: Vec<Located<Pattern>>,
    structure_pattern_id: usize,
}

impl Structure {
    pub fn new(
        parts: Located<Vec<InternId>>,
        arguments: Vec<Located<Pattern>>,
        structure_pattern_id: usize,
    ) -> Self {
        Self {
            parts,
            arguments,
            structure_pattern_id,
        }
    }

    pub fn arguments(&self) -> &[Located<Pattern>] {
        &self.arguments
    }

    pub fn parts(&self) -> &Located<Vec<InternId>> {
        &self.parts
    }

    pub fn structure_pattern_id(&self) -> usize {
        self.structure_pattern_id
    }
}
