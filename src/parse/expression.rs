use crate::{
    data_table::{
        BoundDataId, CaptureDataId, PathUniqueNameDataId, SelfCaptureDataId,
        TailCallDataId, UniqueNameDataId,
    },
    interner::InternId,
    location::Located,
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
    pub bound_id: BoundDataId,
    pub unique_name_id: PathUniqueNameDataId,
}

pub struct Application {
    pub function: Box<Located<Expression>>,
    pub argument: Box<Located<Expression>>,
    pub tail_call_id: TailCallDataId,
}

pub struct Lambda {
    pub variable: Located<InternId>,
    pub expression: Box<Located<Expression>>,
    pub capture_id: CaptureDataId,
    pub unique_name_id: UniqueNameDataId,
    pub self_capture_id: SelfCaptureDataId,
}

pub struct LetIn {
    pub variable: Located<InternId>,
    pub variable_expression: Box<Located<Expression>>,
    pub return_expression: Box<Located<Expression>>,
    pub unique_name_id: UniqueNameDataId,
}

pub struct MatchAs {
    pub expression: Box<Located<Expression>>,
    pub branches: Vec<Located<Branch>>,
}

pub struct Branch {
    pub pattern: Located<Pattern>,
    pub expression: Located<Expression>,
}
