#![cfg_attr(test, allow(non_snake_case))]

use crate::ast::{Name, Program, Term, Unique};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

#[cfg(test)]
mod tests;

mod constant;
mod lambda;

pub use constant::*;
pub use lambda::*;

pub struct Builder {
    version: (usize, usize, usize),
    term: Term<Name>,
}

pub struct NeedsTerm {
    version: (usize, usize, usize),
    // TODO: Hide these two behind interface
    next_unique: Cell<isize>,
    names: RefCell<HashMap<String, Unique>>,
}

pub trait WithTerm
where
    Self: Sized,
{
    type Next;

    fn next(self, term: Term<Name>) -> Self::Next;
    fn get_name(&self, name_str: &str) -> Name;
}

impl WithTerm for NeedsTerm {
    type Next = Builder;
    fn next(self, term: Term<Name>) -> Self::Next {
        Builder {
            version: self.version,
            term,
        }
    }

    fn get_name(&self, name_str: &str) -> Name {
        let mut names = self.names.borrow_mut();
        if let Some(unique) = names.get(name_str) {
            Name {
                text: name_str.to_string(),
                unique: *unique,
            }
        } else {
            let next_unique = self.next_unique.get();
            self.next_unique.set(next_unique + 1);
            let unique = Unique::new(next_unique);
            names.insert(name_str.to_string(), unique);
            Name {
                text: name_str.to_string(),
                unique,
            }
        }
    }
}

impl Builder {
    #[allow(clippy::new_ret_no_self)]
    /// Max: `9223372036854775807`
    pub fn new(maj: usize, min: usize, patch: usize) -> NeedsTerm {
        NeedsTerm {
            version: (maj, min, patch),
            next_unique: Cell::new(0),
            names: RefCell::new(HashMap::new()),
        }
    }

    pub fn build_named(&self) -> Program<Name> {
        Program {
            version: self.version,
            term: self.term.clone(),
        }
    }
}
