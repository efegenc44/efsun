use crate::{
    interner::InternId,
    location::Located,
    resolution::bound::Path,
    state::{AfterUnresolved, Unresolved},
};

pub type Constructor<State> = constructor::Constructor<State>;

pub struct Structure<T> {
    name: Located<InternId>,
    variables: Vec<Located<InternId>>,
    constructors: Vec<Located<Constructor<T>>>,
    path: Option<Path>,
}

pub struct UnresolvedObservation {
    pub name: Located<InternId>,
    pub variables: Vec<Located<InternId>>,
    pub constructors: Vec<Located<Constructor<Unresolved>>>,
}

impl From<UnresolvedObservation> for Structure<Unresolved> {
    fn from(value: UnresolvedObservation) -> Self {
        Self {
            name: value.name,
            variables: value.variables,
            constructors: value.constructors,
            path: None,
        }
    }
}

pub struct Observation<State: AfterUnresolved> {
    pub name: Located<InternId>,
    pub variables: Vec<Located<InternId>>,
    pub constructors: Vec<Located<Constructor<State>>>,
    pub path: Path,
}

impl<S: AfterUnresolved> From<Observation<S>> for Structure<S> {
    fn from(value: Observation<S>) -> Self {
        Self {
            name: value.name,
            variables: value.variables,
            constructors: value.constructors,
            path: Some(value.path),
        }
    }
}

impl<T> Structure<T> {
    pub fn name(&self) -> Located<InternId> {
        self.name
    }

    pub fn variables(&self) -> &[Located<InternId>] {
        &self.variables
    }

    pub fn constructors(&self) -> &[Located<Constructor<T>>] {
        &self.constructors
    }
}

impl Structure<Unresolved> {
    pub fn observe(self) -> UnresolvedObservation {
        UnresolvedObservation {
            name: self.name,
            variables: self.variables,
            constructors: self.constructors,
        }
    }
}

impl<S: AfterUnresolved> Structure<S> {
    pub fn path(&self) -> &Path {
        self.path.as_ref().unwrap()
    }

    pub fn observe(self) -> Observation<S> {
        Observation {
            name: self.name,
            variables: self.variables,
            constructors: self.constructors,
            path: self.path.unwrap(),
        }
    }
}

pub mod constructor {
    use crate::{
        interner::InternId,
        location::Located,
        parse::type_expression::TypeExpression,
        resolution::bound::Path,
        state::{AfterUnresolved, Unresolved},
    };

    pub struct Constructor<State> {
        name: Located<InternId>,
        arguments: Vec<Located<TypeExpression<State>>>,
        path: Option<Path>,
    }

    pub struct UnresolvedObservation {
        pub name: Located<InternId>,
        pub arguments: Vec<Located<TypeExpression<Unresolved>>>,
    }

    impl From<UnresolvedObservation> for Constructor<Unresolved> {
        fn from(value: UnresolvedObservation) -> Self {
            Self {
                name: value.name,
                arguments: value.arguments,
                path: None,
            }
        }
    }

    pub struct Observation<State: AfterUnresolved> {
        pub name: Located<InternId>,
        pub arguments: Vec<Located<TypeExpression<State>>>,
        pub path: Path,
    }

    impl<S: AfterUnresolved> From<Observation<S>> for Constructor<S> {
        fn from(value: Observation<S>) -> Self {
            Self {
                name: value.name,
                arguments: value.arguments,
                path: Some(value.path),
            }
        }
    }

    impl<State> Constructor<State> {
        pub fn name(&self) -> Located<InternId> {
            self.name
        }

        pub fn arguments(&self) -> &[Located<TypeExpression<State>>] {
            &self.arguments
        }
    }

    impl Constructor<Unresolved> {
        pub fn observe(self) -> UnresolvedObservation {
            UnresolvedObservation {
                name: self.name,
                arguments: self.arguments,
            }
        }
    }

    impl<S: AfterUnresolved> Constructor<S> {
        pub fn path(&self) -> &Path {
            self.path.as_ref().unwrap()
        }

        pub fn observe(self) -> Observation<S> {
            Observation {
                name: self.name,
                arguments: self.arguments,
                path: self.path.unwrap(),
            }
        }
    }
}
