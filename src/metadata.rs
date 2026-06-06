use std::{collections::HashMap, marker::PhantomData, ops::Index};

use crate::{
    interner::InternId,
    resolution::{
        bound::{Bound, Capture, Path},
        renamer::UniqueName,
    },
};

/// Proof of construction
pub struct Unresolved(());

/// Utility trait for indice polymorphism
pub trait Setter<I, V> {
    fn set(&mut self, id: I, value: V);
}

/// Utility trait for indice polymorphism
pub trait Generator<T> {
    fn get(&mut self) -> T;
}

macro_rules! metadata {
    ( $($i:ident -> $t:ty),* ) => {
        $(
            #[derive(Clone, Copy, PartialEq, Eq, Hash)]
            pub struct $i(usize);
        )*

        #[allow(non_snake_case)]
        #[derive(Clone, Copy, Default)]
        pub struct Indicies {
            $($i: usize),*
        }

        $(
            impl Generator<$i> for Indicies {
                fn get(&mut self) -> $i {
                    let id = self.$i;
                    self.$i += 1;
                    $i(id)
                }
            }
        )*

        #[allow(non_snake_case)]
        pub struct Metadata<State> {
            $($i: HashMap<$i, $t>),*,

            /// Used as a proof mechanism for compilation pipeline
            ///   This doesn't prevent using a tree node with unrelated
            ///   metadata but metadata is intented to be a singleton
            ///   anyway
            state: PhantomData<State>,
        }

        impl Metadata<Unresolved> {
            pub fn new() -> Self {
                Metadata {
                    $($i: Default::default()),*,
                    state: PhantomData::<Unresolved>
                }
            }
        }

        impl<State> Metadata<State> {
            pub fn transition<NewState>(self, _proof: NewState) -> Metadata<NewState> {
                Metadata {
                    $($i: self.$i),*,
                    state: PhantomData::<NewState>
                }
            }
        }

        $(
            impl<State> Index<$i> for Metadata<State> {
                type Output = $t;

                fn index(&self, index: $i) -> &Self::Output {
                    &self.$i[&index]
                }
            }

            impl<State> Setter<$i, $t> for Metadata<State> {
                fn set(&mut self, id: $i, value: $t) {
                    self.$i.insert(id, value);
                }
            }
        )*
    };
}

metadata! {
    BoundMetadataId -> Bound,
    CaptureMetadataId -> Vec<Capture>,
    StructurePatternMetadataId -> StructurePattern,
    PathMetadataId -> Path,
    UniqueNameMetadataId -> Option<UniqueName>,
    ANFBoundMetadataId -> Bound
}

pub struct StructurePattern {
    type_path: Path,
    constructor_name: InternId,
    tag: usize,
}

impl StructurePattern {
    pub fn new(type_path: Path, constructor_name: InternId, tag: usize) -> Self {
        Self {
            type_path,
            constructor_name,
            tag,
        }
    }

    pub fn type_path(&self) -> &Path {
        &self.type_path
    }

    pub fn constructor_name(&self) -> InternId {
        self.constructor_name
    }

    pub fn tag(&self) -> usize {
        self.tag
    }
}
