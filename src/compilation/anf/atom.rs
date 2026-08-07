use crate::{
    compilation::anf,
    data_table::{ANFBoundDataId, ANFCaptureDataId, SelfCaptureDataId},
    interner::InternId,
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
    pub anf_bound_id: ANFBoundDataId,
}

pub struct Lambda {
    pub variables: Vec<UniqueName>,
    pub expression: Box<anf::Expression>,
    pub anf_capture_id: ANFCaptureDataId,
    pub self_capture_id: SelfCaptureDataId,
}
