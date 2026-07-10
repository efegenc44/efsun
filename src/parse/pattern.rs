use std::fmt::Display;

use crate::{
    interner::{InternId, Interner},
    location::Located,
    metadata::{StructurePatternMetadataId, UniqueNameMetadataId},
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
    pub unique_name_id: UniqueNameMetadataId,
}

#[derive(Clone, Debug)]
pub struct Structure {
    pub parts: Located<Vec<InternId>>,
    pub arguments: Vec<Located<Pattern>>,
    pub structure_pattern_id: StructurePatternMetadataId,
}

impl Pattern {
    #[allow(unused)]
    pub fn print(&self, depth: usize, interner: &Interner) {
        fn indent(display: impl Display, depth: usize) {
            println!("{:level$}{display}", "", level = depth * 2);
        }

        match self {
            Self::Any(any) => {
                todo!()
                // let name = if let Some(unique_name) = any.try_unique_name() {
                //     &unique_name.to_string()
                // } else {
                //     interner.lookup(&any.identifier())
                // };

                // indent(format!("Any: {}", name), depth)
            }
            Self::Structure(structure) => {
                indent("Structure Pattern", depth);
                for argument in &structure.arguments {
                    argument.data.print(depth + 1, interner);
                }
            }
            Self::String(string) => indent(format!("\"{}\"", interner.lookup(string)), depth),
        }
    }

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
