use std::fmt::Display;

pub struct Interner {
    pub strings: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct InternId(usize);

impl InternId {
    pub fn new(value: usize) -> Self {
        Self(value)
    }

    pub fn dummy() -> Self {
        Self(0)
    }
}

impl Interner {
    pub fn new() -> Self {
        Interner { strings: Vec::default() }
    }

    pub fn intern(&mut self, string: String) -> InternId {
        for (id, interned) in self.strings.iter().enumerate() {
            if interned == &string {
                return InternId(id);
            }
        }

        self.strings.push(string);
        InternId(self.strings.len() - 1)
    }

    pub fn lookup(&self, id: InternId) -> &str {
        self.strings.get(id.0).unwrap()
    }

    pub fn intern_id(&self, string: &str) -> InternId {
        for (id, interned) in self.strings.iter().enumerate() {
            if interned == string {
                return InternId(id);
            }
        }

        unreachable!()
    }
}

impl Display for InternId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}