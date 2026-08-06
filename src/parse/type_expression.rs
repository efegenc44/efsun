use crate::{data_table::BoundDataId, interner::InternId, location::Located};

pub enum TypeExpression {
    Path(Path),
    Application(Application),
}

pub struct Path {
    pub parts: Located<Vec<InternId>>,
    pub bound_id: BoundDataId,
}

pub struct Application {
    pub function: Box<Located<TypeExpression>>,
    pub arguments: Vec<Located<TypeExpression>>,
}
