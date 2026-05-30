use crate::{interner::InternId, location::Located};

pub struct Path {
    parts: Located<Vec<InternId>>,
    bound_id: usize,
}

impl Path {
    pub fn new(parts: Located<Vec<InternId>>, bound_id: usize) -> Self {
        Self { parts, bound_id }
    }

    pub fn parts(&self) -> &Located<Vec<InternId>> {
        &self.parts
    }

    pub fn bound_id(&self) -> usize {
        self.bound_id
    }
}
