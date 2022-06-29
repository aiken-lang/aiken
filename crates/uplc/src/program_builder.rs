#![cfg_attr(test, allow(non_snake_case))]

use crate::ast::{Name, Program, Term, Unique};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

#[cfg(test)]
mod tests;

mod constant;

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

pub struct LambdaBuilder<T> {
    outer: T,
    parameter_name: Name,
}

pub trait WithTerm
where
    Self: Sized,
{
    type Next;

    fn next(self, term: Term<Name>) -> Self::Next;
    fn get_name(&self, name_str: &str) -> Name;

    fn with_lambda(self, name_str: &str) -> LambdaBuilder<Self> {
        let parameter_name = self.get_name(name_str);
        LambdaBuilder {
            outer: self,
            parameter_name,
        }
    }
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

impl<T: WithTerm> WithTerm for LambdaBuilder<T> {
    type Next = T::Next;

    fn next(self, term: Term<Name>) -> Self::Next {
        let term = Term::Lambda {
            parameter_name: self.parameter_name,
            body: Box::new(term),
        };
        self.outer.next(term)
    }

    fn get_name(&self, name_str: &str) -> Name {
        self.outer.get_name(name_str)
    }
}

impl Builder {
    #[allow(clippy::new_ret_no_self)]
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
