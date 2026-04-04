pub mod structure;

use crate::interner::InternId;

pub type Structure<State> = structure::Structure<State>;

#[derive(Clone)]
pub enum Pattern<State> {
    Any(InternId),
    Structure(Structure<State>),
    String(InternId),
}
