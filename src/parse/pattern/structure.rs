use std::marker::PhantomData;

use crate::{
    interner::InternId,
    location::Located,
    resolution::bound::Path,
    state::{AfterUnresolved, Unresolved},
};

use super::Pattern;

#[derive(Clone)]
pub struct Structure<State> {
    parts: Located<Vec<InternId>>,
    arguments: Vec<Located<Pattern<State>>>,
    type_path: Option<Path>,
    constructor_name: Option<InternId>,
    order: Option<usize>,
    state: PhantomData<State>,
}

pub struct UnresolvedObservation {
    pub parts: Located<Vec<InternId>>,
    pub arguments: Vec<Located<Pattern<Unresolved>>>,
}

impl From<UnresolvedObservation> for Structure<Unresolved> {
    fn from(val: UnresolvedObservation) -> Self {
        Structure {
            parts: val.parts,
            arguments: val.arguments,
            type_path: None,
            constructor_name: None,
            order: None,
            state: PhantomData,
        }
    }
}

pub struct Observation<State: AfterUnresolved> {
    pub parts: Located<Vec<InternId>>,
    pub arguments: Vec<Located<Pattern<State>>>,
    pub type_path: Path,
    pub constructor_name: InternId,
    pub order: usize,
}

impl<S: AfterUnresolved> From<Observation<S>> for Structure<S> {
    fn from(val: Observation<S>) -> Self {
        Structure {
            parts: val.parts,
            arguments: val.arguments,
            type_path: Some(val.type_path),
            constructor_name: Some(val.constructor_name),
            order: Some(val.order),
            state: PhantomData,
        }
    }
}

impl<State> Structure<State> {
    pub fn arguments(&self) -> &[Located<Pattern<State>>] {
        &self.arguments
    }
}

impl Structure<Unresolved> {
    pub fn observe(self) -> UnresolvedObservation {
        UnresolvedObservation {
            parts: self.parts,
            arguments: self.arguments,
        }
    }
}

impl<S: AfterUnresolved> Structure<S> {
    pub fn type_path(&self) -> &Path {
        self.type_path.as_ref().unwrap()
    }

    pub fn constructor_name(&self) -> InternId {
        *self.constructor_name.as_ref().unwrap()
    }

    pub fn order(&self) -> usize {
        self.order.unwrap()
    }

    pub fn observe(self) -> Observation<S> {
        Observation {
            parts: self.parts,
            arguments: self.arguments,
            type_path: self.type_path.unwrap(),
            constructor_name: self.constructor_name.unwrap(),
            order: self.order.unwrap(),
        }
    }
}
