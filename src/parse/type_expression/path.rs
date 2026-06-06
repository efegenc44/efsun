use crate::{interner::InternId, location::Located, metadata::BoundMetadataId};

pub struct Path {
    parts: Located<Vec<InternId>>,
    bound_id: BoundMetadataId,
}

impl Path {
    pub fn new(parts: Located<Vec<InternId>>, bound_id: BoundMetadataId) -> Self {
        Self { parts, bound_id }
    }

    pub fn parts(&self) -> &Located<Vec<InternId>> {
        &self.parts
    }

    pub fn bound_id(&self) -> BoundMetadataId {
        self.bound_id
    }
}
