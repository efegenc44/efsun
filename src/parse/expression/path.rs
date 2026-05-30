use crate::{interner::InternId, location::Located};

pub struct Path {
    pub parts: Located<Vec<InternId>>,
    pub bound_id: usize,
    pub unique_name_id: usize,
}

impl Path {
    pub fn new(parts: Located<Vec<InternId>>, bound_id: usize, unique_name_id: usize) -> Self {
        Self {
            parts,
            bound_id,
            unique_name_id,
        }
    }

    pub fn parts(&self) -> &Located<Vec<InternId>> {
        &self.parts
    }

    pub fn bound_id(&self) -> usize {
        self.bound_id
    }

    pub fn unique_name_id(&self) -> usize {
        self.unique_name_id
    }
}
