use crate::{
    compilation::anf,
    interner::InternId,
    metadata::{BoundMetadataId, CaptureMetadataId},
    resolution::{bound::Bound, renamer::UniqueName},
};

pub enum Atom {
    String(InternId),
    Path(Path),
    Lambda(Lambda),
}

pub struct Path {
    pub path: anf::Path,
    pub bound: Option<Bound>,
    pub anf_bound_id: BoundMetadataId,
}

pub struct Lambda {
    pub variables: Vec<UniqueName>,
    pub expression: Box<anf::Expression>,
    pub anf_capture_id: CaptureMetadataId,
}
