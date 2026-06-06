use crate::{
    compilation::anf::{self, Atom},
    location::Located,
    parse::expression::Expression,
};


pub struct Application {
    function: Box<Located<Expression>>,
    argument: Box<Located<Expression>>,
}

impl Application {
    pub fn new(function: Located<Expression>, argument: Located<Expression>) -> Self {
        Self {
            function: Box::new(function),
            argument: Box::new(argument),
        }
    }

    pub fn function(&self) -> &Located<Expression> {
        &self.function
    }

    pub fn argument(&self) -> &Located<Expression> {
        &self.argument
    }

    pub fn into_anf(self, transformer: &anf::Transformer, k: anf::Continuation) -> anf::Expression {
        let Self { function, argument } = self;

        #[rustfmt::skip]
        let result = argument.into_data().into_anf(transformer, Box::new(|argument| {
            function.into_data().into_anf(transformer, Box::new(|function| {
                let (variable, path) = transformer.new_anf_local();

                let application = anf::expression::Application::new(
                    variable,
                    function,
                    argument,
                    k(Atom::Path(path))
                );

                anf::Expression::Application(application)
            }))
        }));

        result
    }
}
