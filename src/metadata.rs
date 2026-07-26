use std::{
    collections::{HashMap, HashSet},
    marker::PhantomData,
    ops::Index,
};

use crate::resolution::{
    bound::{Bound, Capture, Path},
    renamer::UniqueName,
};

/// Proof of construction
pub struct Unresolved(());

/// Utility trait for indice polymorphism
pub trait Setter<I, V> {
    fn set(&mut self, id: I, value: V);
}

pub trait SetterFlag<I> {
    fn set_flag(&mut self, id: I);
}

pub trait CheckFlag<I> {
    fn check(&self, id: I) -> bool;
}

/// Utility trait for indice polymorphism
pub trait Generator<T> {
    fn get(&mut self) -> T;
}

macro_rules! metadata {
    ( $($i:ident -> $t:ty),* | $($j:ident),* ) => {
        $(
            #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
            pub struct $i(usize);
        )*

        $(
            #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
            pub struct $j(usize);
        )*

        #[allow(non_snake_case)]
        #[derive(Clone, Copy, Default)]
        pub struct Indicies {
            $($i: usize),*,
            $($j: usize),*
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

        $(
            impl Generator<$j> for Indicies {
                fn get(&mut self) -> $j {
                    let id = self.$j;
                    self.$j += 1;
                    $j(id)
                }
            }
        )*

        #[allow(non_snake_case)]
        pub struct Metadata<State> {
            $($i: HashMap<$i, $t>),*,
            $($j: HashSet<$j>),*,

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
                    $($j: Default::default()),*,
                    state: PhantomData::<Unresolved>
                }
            }
        }

        impl<State> Metadata<State> {
            pub fn transition<NewState>(self, _proof: NewState) -> Metadata<NewState> {
                Metadata {
                    $($i: self.$i),*,
                    $($j: self.$j),*,
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

        $(
            impl<State> CheckFlag<$j> for Metadata<State> {
                fn check(&self, id: $j) -> bool {
                    self.$j.contains(&id)
                }
            }

            impl<State> SetterFlag<$j> for Metadata<State> {
                fn set_flag(&mut self, id: $j) {
                    self.$j.insert(id);
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
    UniqueNameMetadataId -> Option<UniqueName>
    |
    TailCallMetadataId
}

pub struct StructurePattern {
    pub type_path: Path,
    pub tag: usize,
}
