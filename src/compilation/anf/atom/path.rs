use std::marker::PhantomData;

use crate::{
    compilation::anf::ANFPath,
    resolution::{Resolved, Unresolved, bound::Bound},
};

pub struct Path<State> {
    path: ANFPath,
    bound: Option<Bound>,
    state: PhantomData<State>,
}

pub struct UnresolvedObservation {
    pub path: ANFPath,
    pub bound: Option<Bound>,
}

impl From<UnresolvedObservation> for Path<Unresolved> {
    fn from(value: UnresolvedObservation) -> Self {
        Self {
            path: value.path,
            bound: value.bound,
            state: PhantomData,
        }
    }
}

pub struct ResolvedObservation {
    pub path: ANFPath,
    pub bound: Bound,
}

impl From<ResolvedObservation> for Path<Resolved> {
    fn from(value: ResolvedObservation) -> Self {
        Self {
            path: value.path,
            bound: Some(value.bound),
            state: PhantomData,
        }
    }
}

impl<State> Path<State> {
    pub fn path(&self) -> &ANFPath {
        &self.path
    }
}

impl Path<Unresolved> {
    pub fn observe(self) -> UnresolvedObservation {
        UnresolvedObservation {
            path: self.path,
            bound: self.bound,
        }
    }
}

impl Path<Resolved> {
    pub fn bound(&self) -> &Bound {
        self.bound.as_ref().unwrap()
    }
}
