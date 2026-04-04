use crate::{interner::InternId, location::Located};

pub struct ModulePath {
    parts: Located<Vec<InternId>>,
}

impl ModulePath {
    pub fn new(parts: Located<Vec<InternId>>) -> Self {
        Self { parts }
    }

    pub fn parts(&self) -> &Located<Vec<InternId>> {
        &self.parts
    }
}
