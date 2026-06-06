use crate::{interner::InternId, metadata::UniqueNameMetadataId};

#[derive(Clone)]
pub struct Any {
    identifier: InternId,
    unique_name_id: UniqueNameMetadataId,
}

impl Any {
    pub fn new(identifier: InternId, unique_name_id: UniqueNameMetadataId) -> Self {
        Self {
            identifier,
            unique_name_id,
        }
    }

    pub fn identifier(&self) -> InternId {
        self.identifier
    }

    pub fn unique_name_id(&self) -> UniqueNameMetadataId {
        self.unique_name_id
    }
}
