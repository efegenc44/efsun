use std::marker::PhantomData;

use crate::{
    interner::InternId,
    location::Located,
    resolution::{bound::Bound, renamer::UniqueName},
    state::{Renamed, Resolved, Unresolved},
};

pub struct Path<State> {
    parts: Located<Vec<InternId>>,
    bound: Option<Bound>,
    unique_name: Option<UniqueName>,
    state: PhantomData<State>,
}

pub struct UnresolvedObservation {
    pub parts: Located<Vec<InternId>>,
}

impl From<UnresolvedObservation> for Path<Unresolved> {
    fn from(val: UnresolvedObservation) -> Self {
        Path {
            parts: val.parts,
            bound: None,
            unique_name: None,
            state: PhantomData,
        }
    }
}

pub struct ResolvedObservation {
    pub parts: Located<Vec<InternId>>,
    pub bound: Bound,
}

impl From<ResolvedObservation> for Path<Resolved> {
    fn from(val: ResolvedObservation) -> Self {
        Path {
            parts: val.parts,
            bound: Some(val.bound),
            unique_name: None,
            state: PhantomData,
        }
    }
}

pub struct RenamedObservation {
    pub parts: Located<Vec<InternId>>,
    pub bound: Bound,
    pub unique_name: Option<UniqueName>,
}

impl From<RenamedObservation> for Path<Renamed> {
    fn from(val: RenamedObservation) -> Self {
        Path {
            parts: val.parts,
            bound: Some(val.bound),
            unique_name: val.unique_name,
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

    pub fn observe(self) -> ResolvedObservation {
        ResolvedObservation {
            parts: self.parts,
            bound: self.bound.unwrap(),
        }
    }
}

impl Path<Renamed> {
    pub fn observe(self) -> RenamedObservation {
        RenamedObservation {
            parts: self.parts,
            bound: self.bound.unwrap(),
            unique_name: self.unique_name,
        }
    }
}
