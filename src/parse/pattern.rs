use crate::{
    data_table::{StructurePatternDataId, UniqueNameDataId},
    interner::InternId,
    location::Located,
};

#[derive(Clone, Debug)]
pub enum Pattern {
    Any(Any),
    Structure(Structure),
    String(InternId),
}

#[derive(Clone, Debug)]
pub struct Any {
    pub identifier: InternId,
    pub unique_name_id: UniqueNameDataId,
}

#[derive(Clone, Debug)]
pub struct Structure {
    pub parts: Located<Vec<InternId>>,
    pub arguments: Vec<Located<Pattern>>,
    pub structure_pattern_id: StructurePatternDataId,
}

impl Pattern {
    pub fn local_count(&self) -> usize {
        match self {
            Self::Any(_) => 1,
            Self::Structure(structure) => structure
                .arguments
                .iter()
                .fold(0, |acc, x| acc + x.data.local_count()),
            Self::String(_) => 0,
        }
    }
}
