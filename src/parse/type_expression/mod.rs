pub mod path;
pub mod application;

pub type Path<State>        = path::Path<State>;
pub type Application<State> = application::Application<State>;

pub enum TypeExpression<State> {
    Path(Path<State>),
    Application(Application<State>),
}
