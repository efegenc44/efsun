use crate::{
    compilation::anf::{self, Atom, atom},
    interner::InternId,
    location::Located,
    metadata::{BoundMetadataId, Generator, UniqueNameMetadataId},
    resolution::bound::Bound,
};

pub struct Path {
    parts: Located<Vec<InternId>>,
    bound_id: BoundMetadataId,
    unique_name_id: UniqueNameMetadataId,
}

impl Path {
    pub fn new(
        parts: Located<Vec<InternId>>,
        bound_id: BoundMetadataId,
        unique_name_id: UniqueNameMetadataId,
    ) -> Self {
        Self {
            parts,
            bound_id,
            unique_name_id,
        }
    }

    pub fn parts(&self) -> &Located<Vec<InternId>> {
        &self.parts
    }

    pub fn bound_id(&self) -> BoundMetadataId {
        self.bound_id
    }

    pub fn unique_name_id(&self) -> UniqueNameMetadataId {
        self.unique_name_id
    }

    pub fn into_anf(self, transformer: &anf::Transformer, k: anf::Continuation) -> anf::Expression {
        let Self {
            parts,
            bound_id,
            unique_name_id,
        } = self;

        let bound = &transformer.metadata()[bound_id];
        let bound = match bound {
            Bound::Absolute(_) => Some(bound.clone()),
            Bound::Local(_) | Bound::Capture(_) => None,
        };

        let unique_name = &transformer.metadata()[unique_name_id];
        let path = match unique_name {
            Some(unique_name) => anf::Path::Local(*unique_name),
            None => anf::Path::Absolute(parts.into_data()),
        };

        let path = atom::Path::new(path, bound, transformer.indicies_mut().get());
        k(Atom::Path(path))
    }
}
