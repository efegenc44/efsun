use std::marker::PhantomData;

use crate::{
    interner::InternId,
    location::Located,
    resolution::bound::Bound,
    state::{Renamed, Resolved, Unresolved},
};

pub struct Path<State> {
    parts: Located<Vec<InternId>>,
    bound: Option<Bound>,
    state: PhantomData<State>,
}

pub struct UnresolvedObservation {
    pub parts: Located<Vec<InternId>>,
}

impl From<UnresolvedObservation> for Path<Unresolved> {
    fn from(value: UnresolvedObservation) -> Self {
        Self {
            parts: value.parts,
            bound: None,
            state: PhantomData,
        }
    }
}

pub struct ResolvedObservation {
    pub parts: Located<Vec<InternId>>,
    pub bound: Bound,
}

impl From<ResolvedObservation> for Path<Resolved> {
    fn from(value: ResolvedObservation) -> Self {
        Self {
            parts: value.parts,
            bound: Some(value.bound),
            state: PhantomData,
        }
    }
}

pub struct RenamedObservation {
    pub parts: Located<Vec<InternId>>,
    pub bound: Bound,
}

impl From<RenamedObservation> for Path<Renamed> {
    fn from(value: RenamedObservation) -> Self {
        Self {
            parts: value.parts,
            bound: Some(value.bound),
            state: PhantomData,
        }
    }
}

impl<State> Path<State> {
    pub fn parts(&self) -> &Located<Vec<InternId>> {
        &self.parts
    }

    pub fn try_bound(&self) -> Option<&Bound> {
        self.bound.as_ref()
    }
}

impl Path<Unresolved> {
    pub fn observe(self) -> UnresolvedObservation {
        UnresolvedObservation { parts: self.parts }
    }
}

impl Path<Resolved> {
    pub fn bound(&self) -> &Bound {
        self.bound.as_ref().unwrap()
    }
}
