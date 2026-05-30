use crate::interner::InternId;

#[derive(Clone)]
pub struct Any {
    identifier: InternId,
    unique_name_id: usize,
}

impl Any {
    pub fn new(identifier: InternId, unique_name_id: usize) -> Self {
        Self {
            identifier,
            unique_name_id,
        }
    }

    pub fn identifier(&self) -> InternId {
        self.identifier
    }

    pub fn unique_name_id(&self) -> usize {
        self.unique_name_id
    }
}
