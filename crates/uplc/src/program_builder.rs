#![cfg_attr(test, allow(non_snake_case))]

use crate::ast::{Name, Program, Term, Unique};
use std::cell::RefCell;
use std::collections::HashMap;

#[cfg(test)]
mod tests;

mod apply;
mod constant;
mod delay;
mod error;
mod force;
mod lambda;
mod var;

pub use apply::*;
pub use constant::*;
pub use delay::*;
pub use error::*;
pub use force::*;
pub use lambda::*;
pub use var::*;

pub struct Builder {
    version: (usize, usize, usize),
    term: Term<Name>,
}

struct Context {
    next_unique: isize,
    names: HashMap<String, Unique>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            next_unique: 0,
            names: HashMap::new(),
        }
    }

    pub fn get_name(&mut self, name_str: &str) -> Name {
        if let Some(unique) = self.names.get(name_str) {
            Name {
                text: name_str.to_string(),
                unique: *unique,
            }
        } else {
            let next_unique = self.next_unique;
            self.next_unique = next_unique + 1;
            let unique = Unique::new(next_unique);
            self.names.insert(name_str.to_string(), unique);
            Name {
                text: name_str.to_string(),
                unique,
            }
        }
    }
}

pub struct Core {
    version: (usize, usize, usize),
    ctx: RefCell<Context>,
}

pub trait WithTerm
where
    Self: Sized,
{
    type Next;

    fn next(self, term: Term<Name>) -> Self::Next;
    fn get_name(&self, name_str: &str) -> Name;
}

impl WithTerm for Core {
    type Next = Builder;
    fn next(self, term: Term<Name>) -> Self::Next {
        Builder {
            version: self.version,
            term,
        }
    }

    fn get_name(&self, name_str: &str) -> Name {
        let mut ctx = self.ctx.borrow_mut();
        ctx.get_name(name_str)
    }
}

impl Builder {
    /// Max: `9223372036854775807`
    pub fn start(maj: usize, min: usize, patch: usize) -> Core {
        Core {
            version: (maj, min, patch),
            ctx: RefCell::new(Context::new()),
        }
    }

    pub fn build_named(&self) -> Program<Name> {
        Program {
            version: self.version,
            term: self.term.clone(),
        }
    }
}
