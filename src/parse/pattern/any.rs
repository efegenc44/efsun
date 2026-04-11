use std::marker::PhantomData;

use crate::{interner::InternId, resolution::{Renamed, ResolvedState, renamer::UniqueName}};

#[derive(Clone)]
pub struct Any<State> {
    identifier: InternId,
    unique_name: Option<UniqueName>,
    state: PhantomData<State>,
}

pub struct Observation {
    pub identifier: InternId
}

impl<S: ResolvedState> From<Observation> for Any<S> {
    fn from(value: Observation) -> Self {
        Self {
            identifier: value.identifier,
            unique_name: None,
            state: PhantomData,
        }
    }
}

pub struct RenamedObservation {
    pub identifier: InternId,
    pub unique_name: UniqueName,
}

impl From<RenamedObservation> for Any<Renamed> {
    fn from(value: RenamedObservation) -> Self {
        Self {
            identifier: value.identifier,
            unique_name: Some(value.unique_name),
            state: PhantomData,
        }
    }
}

impl<S: ResolvedState> Any<S> {
    pub fn observe(self) -> Observation {
        Observation { identifier: self.identifier }
    }
}

impl Any<Renamed> {
    pub fn unique_name(&self) -> UniqueName {
        self.unique_name.unwrap()
    }
}
