use crate::{compilation::anf, metadata::BoundMetadataId, resolution::bound::Bound};

pub struct Path {
    path: anf::Path,
    bound: Option<Bound>,
    anf_bound_id: BoundMetadataId,
}

impl Path {
    pub fn new(path: anf::Path, bound: Option<Bound>, anf_bound_id: BoundMetadataId) -> Self {
        Self {
            path,
            bound,
            anf_bound_id,
        }
    }

    pub fn path(&self) -> &anf::Path {
        &self.path
    }

    pub fn anf_bound_id(&self) -> BoundMetadataId {
        self.anf_bound_id
    }

    pub fn bound(&self) -> Option<&Bound> {
        self.bound.as_ref()
    }
}
