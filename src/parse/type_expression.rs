use crate::{interner::InternId, location::Located, metadata::BoundMetadataId};

pub enum TypeExpression {
    Path(Path),
    Application(Application),
}

pub struct Path {
    pub parts: Located<Vec<InternId>>,
    pub bound_id: BoundMetadataId,
}

pub struct Application {
    pub function: Box<Located<TypeExpression>>,
    pub arguments: Vec<Located<TypeExpression>>,
}
