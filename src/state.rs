#[derive(Clone, Copy)]
pub struct Unresolved;
#[derive(Clone, Copy)]
pub struct Resolved;
#[derive(Clone, Copy)]
pub struct Renamed;

pub trait AfterUnresolved {}
pub trait BeforeRenamed {}

impl AfterUnresolved for Resolved {}
impl AfterUnresolved for Renamed {}

impl BeforeRenamed for Unresolved {}
impl BeforeRenamed for Resolved {}
