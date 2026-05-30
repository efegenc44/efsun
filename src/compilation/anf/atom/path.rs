use crate::{compilation::anf, resolution::bound::Bound};

pub struct Path {
    path: anf::Path,
    bound: Option<Bound>,
    anf_bound_id: usize,
}

impl Path {
    pub fn new(path: anf::Path, bound: Option<Bound>, anf_bound_id: usize) -> Self {
        Self {
            path,
            bound,
            anf_bound_id,
        }
    }

    pub fn path(&self) -> &anf::Path {
        &self.path
    }

    pub fn anf_bound_id(&self) -> usize {
        self.anf_bound_id
    }

    pub fn bound(&self) -> Option<&Bound> {
        self.bound.as_ref()
    }
}
