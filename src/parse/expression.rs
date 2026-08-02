use crate::{
    interner::InternId,
    location::Located,
    metadata::{
        BoundMetadataId, CaptureMetadataId, PathUniqueNameMetadataId, SelfCaptureMetadataId,
        TailCallMetadataId, UniqueNameMetadataId,
    },
    parse::pattern::Pattern,
};

pub enum Expression {
    String(InternId),
    Path(Path),
    Application(Application),
    Lambda(Lambda),
    LetIn(LetIn),
    MatchAs(MatchAs),
}

pub struct Path {
    pub parts: Located<Vec<InternId>>,
    pub bound_id: BoundMetadataId,
    pub unique_name_id: PathUniqueNameMetadataId,
}

pub struct Application {
    pub function: Box<Located<Expression>>,
    pub argument: Box<Located<Expression>>,
    pub tail_call_id: TailCallMetadataId,
}

pub struct Lambda {
    pub variable: Located<InternId>,
    pub expression: Box<Located<Expression>>,
    pub capture_id: CaptureMetadataId,
    pub unique_name_id: UniqueNameMetadataId,
    pub self_capture_id: SelfCaptureMetadataId,
}

pub struct LetIn {
    pub variable: Located<InternId>,
    pub variable_expression: Box<Located<Expression>>,
    pub return_expression: Box<Located<Expression>>,
    pub unique_name_id: UniqueNameMetadataId,
}

pub struct MatchAs {
    pub expression: Box<Located<Expression>>,
    pub branches: Vec<Located<Branch>>,
}

pub struct Branch {
    pub pattern: Located<Pattern>,
    pub expression: Located<Expression>,
}
