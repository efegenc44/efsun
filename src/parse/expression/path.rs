use crate::{
    compilation::anf::{self, Atom, atom},
    interner::InternId,
    location::Located,
    resolution::bound::Bound,
};

pub struct Path {
    parts: Located<Vec<InternId>>,
    bound_id: usize,
    unique_name_id: usize,
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

    pub fn into_anf(self, transformer: &anf::Transformer, k: anf::Continuation) -> anf::Expression {
        let Self {
            parts,
            bound_id,
            unique_name_id,
        } = self;

        let bound = transformer.metadata().get_bound(bound_id);
        let bound = match bound {
            Bound::Absolute(_) => Some(bound.clone()),
            Bound::Local(_) | Bound::Capture(_) => None,
        };

        let unique_name = transformer.metadata().get_unique_name(unique_name_id);
        let path = match unique_name {
            Some(unique_name) => anf::Path::Local(*unique_name),
            None => anf::Path::Absolute(parts.into_data()),
        };

        let path = atom::Path::new(path, bound, transformer.new_anf_bound_id());
        k(Atom::Path(path))
    }
}
