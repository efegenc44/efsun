use crate::resolution::bound::{BoundId, Capture};

pub struct Frame<T> {
    locals: Vec<T>,
    captures: Vec<Capture>,
}

impl<T> Frame<T> {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            captures: Vec::new(),
        }
    }

    pub fn captures(&self) -> &[Capture] {
        &self.captures
    }

    pub fn captures_mut(&mut self) -> &mut Vec<Capture> {
        &mut self.captures
    }
}

impl<T: Eq> Frame<T> {
    pub fn resolve(&self, identifier: T) -> Option<BoundId> {
        for (index, intern_id) in self.locals.iter().rev().enumerate() {
            if identifier == *intern_id {
                return Some(BoundId::new(self.locals.len() - 1 - index));
            }
        }

        None
    }
}

pub struct Stack<T>(Vec<Frame<T>>);

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn current_frame(&self) -> &Frame<T> {
        self.0.last().unwrap()
    }

    pub fn current_frame_mut(&mut self) -> &mut Frame<T> {
        self.0.last_mut().unwrap()
    }

    pub fn push_frame(&mut self) {
        self.0.push(Frame::new())
    }

    pub fn pop_frame(&mut self) -> Vec<Capture> {
        self.0.pop().unwrap().captures
    }

    pub fn push_local(&mut self, local: T) {
        self.0.last_mut().unwrap().locals.push(local);
    }

    pub fn pop_local(&mut self) {
        self.0.last_mut().unwrap().locals.pop();
    }
}

impl<T: Eq + Copy + Clone> Stack<T> {
    pub fn capture(&mut self, identifier: T) -> Option<Capture> {
        self.capture_in_frame(identifier, 1)
    }

    fn capture_in_frame(&mut self, identifier: T, frame_depth: usize) -> Option<Capture> {
        if frame_depth == self.0.len() {
            return None;
        }

        let index = self.0.len() - 1 - frame_depth;

        let capture = match self.0[index].resolve(identifier) {
            Some(id) => Capture::Local(id),
            None => {
                let capture = self.capture_in_frame(identifier, frame_depth + 1)?;

                let id = match self.0[index].captures.iter().position(|c| *c == capture) {
                    Some(id) => id,
                    None => {
                        self.0[index].captures.push(capture);
                        self.0[index].captures.len() - 1
                    },
                };

                Capture::Outer(BoundId::new(id))
            },
        };

        Some(capture)
    }
}
