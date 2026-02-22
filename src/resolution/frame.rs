use crate::resolution::bound::{Bound, BoundId, Capture};

pub struct Frame<T> {
    locals: Vec<T>,
    captures: Vec<Capture>,
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

pub struct ResolutionStack<T>(Vec<Frame<T>>);

impl<T> ResolutionStack<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push_frame(&mut self) {
        self.0.push(Frame {
            locals: Vec::new(),
            captures: Vec::new()
        });
    }

    pub fn pop_frame(&mut self) -> Vec<Capture> {
        self.0.pop().unwrap().captures
    }

    pub fn push_local(&mut self, value: T) {
        self.0.last_mut().unwrap().locals.push(value);
    }

    pub fn pop_local(&mut self) -> T {
        self.0.last_mut().unwrap().locals.pop().unwrap()
    }
}

impl<T: Eq + Clone> ResolutionStack<T> {
    pub fn locally_resolve(&mut self, value: T) -> Option<Bound> {
        let current_frame = self.0.last().unwrap();

        match current_frame.resolve(value.clone()) {
            Some(id) => Some(Bound::Local(id)),
            None => {
                let capture = self.capture(value)?;
                let frame_captures = &mut self.0.last_mut().unwrap().captures;

                let id = match frame_captures.iter().position(|c| *c == capture) {
                    Some(id) => id,
                    None => {
                        frame_captures.push(capture);
                        frame_captures.len() - 1
                    }
                };

                Some(Bound::Capture(BoundId::new(id)))
            }
        }
    }

    pub fn capture(&mut self, value: T) -> Option<Capture> {
        self.capture_in_frame(value, 1)
    }

    fn capture_in_frame(&mut self, value: T, frame_depth: usize) -> Option<Capture> {
        if frame_depth == self.0.len() {
            return None;
        }

        let index = self.0.len() - 1 - frame_depth;
        let current_frame = &self.0[index];

        let capture = match current_frame.resolve(value.clone()) {
            Some(id) => Capture::Local(id),
            None => {
                let capture = self.capture_in_frame(value, frame_depth + 1)?;
                let frame_captures = &mut self.0[index].captures;

                let id = match frame_captures.iter().position(|c| *c == capture) {
                    Some(id) => id,
                    None => {
                        frame_captures.push(capture);
                        frame_captures.len() - 1
                    },
                };

                Capture::Outer(BoundId::new(id))
            },
        };

        Some(capture)
    }
}

pub struct CheckStack<T>(Vec<Frame<T>>);

impl<T> CheckStack<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push_frame(&mut self, captures: Vec<Capture>) {
        self.0.push(Frame {
            locals: Vec::new(),
            captures
        });
    }

    pub fn pop_frame(&mut self) {
        self.0.pop().unwrap();
    }

    pub fn push_local(&mut self, value: T) {
        self.0.last_mut().unwrap().locals.push(value)
    }

    pub fn pop_local(&mut self) -> T {
        self.0.last_mut().unwrap().locals.pop().unwrap()
    }
}

impl<T: Clone> CheckStack<T> {
    pub fn get_local(&self, id: BoundId) -> T {
        let current_frame = &self.0.last().unwrap();
        let index = current_frame.locals.len() - 1 - id.value();

        current_frame.locals[index].clone()
    }

    pub fn get_capture(&mut self, id: BoundId) -> T {
        let capture = self.0.last().unwrap().captures[id.value()];

        self.get_capture_in_frame(capture, 1)
    }

    fn get_capture_in_frame(&mut self, capture: Capture, frame_depth: usize) -> T {
        let index = self.0.len() - 1 - frame_depth;
        let current_frame = &self.0[index];

        match capture {
            Capture::Local(id) => {
                let local_index = current_frame.locals.len() - 1 - id.value();
                current_frame.locals[local_index].clone()
            },
            Capture::Outer(id) => {
                let capture = current_frame.captures[id.value()];
                self.get_capture_in_frame(capture, frame_depth + 1)
            },
        }
    }
}