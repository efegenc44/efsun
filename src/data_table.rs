use std::{collections::HashMap, hash::Hash};

/// Utility trait for indice polymorphism
pub trait Generator<T> {
    fn get(&mut self) -> T;
}

macro_rules! indicies {
    ($name:ident : $( $indice:ident ),* $(,)?) => {
        $(
            #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
            pub struct $indice(usize);
        )*

        #[allow(non_snake_case)]
        #[derive(Default)]
        pub struct $name {
            $( $indice : usize ),*
        }

        $(
            impl Generator<$indice> for $name {
                fn get(&mut self) -> $indice {
                    let id = self.$indice;
                    self.$indice += 1;
                    $indice(id)
                }
            }
        )*
    };
}

indicies! {
    Indicies:
        BoundDataId,
        CaptureDataId,
        StructurePatternDataId,
        PathDataId,
        UniqueNameDataId,
        SelfCaptureDataId,
        TailCallDataId,
        PathUniqueNameDataId,
}

indicies! {
    ANFIndicies:
        ANFBoundDataId,
        ANFCaptureDataId,
}

pub struct DataTable<I, V> {
    table: HashMap<I, V>,
}

impl<I, V> DataTable<I, V>
where
    I: Hash + Eq,
{
    pub fn set(&mut self, key: I, value: V) {
        self.table.insert(key, value);
    }

    pub fn get(&self, key: &I) -> &V {
        &self.table[key]
    }
}

impl<I, V> Default for DataTable<I, V> {
    fn default() -> Self {
        Self {
            table: Default::default(),
        }
    }
}

pub struct OptionalDataTable<I, V> {
    table: HashMap<I, V>,
}

impl<I, V> OptionalDataTable<I, V>
where
    I: Hash + Eq,
{
    pub fn set(&mut self, key: I, value: V) {
        self.table.insert(key, value);
    }

    pub fn get(&self, key: &I) -> Option<&V> {
        self.table.get(key)
    }
}

impl<I, V> Default for OptionalDataTable<I, V> {
    fn default() -> Self {
        Self {
            table: Default::default(),
        }
    }
}
