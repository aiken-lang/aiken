#![cfg_attr(test, allow(non_snake_case))]

use uplc::ast::{Constant, Name, Program, Term, Unique};

#[cfg(test)]
mod tests;

pub struct Builder {
    version: (usize, usize, usize),
    term: Term<Name>,
}

pub struct Empty {
    version: (usize, usize, usize),
}

pub struct LambdaBuilder<T> {
    outer: T,
}

pub trait WithTerm
where
    Self: Sized,
{
    type Next;

    fn next(self, term: Term<Name>) -> Self::Next;

    fn with_constant_int(self, int: isize) -> Self::Next {
        let term = Term::Constant(Constant::Integer(int));
        self.next(term)
    }

    fn with_lambda(self) -> Self::Next {
        let text = "i_0".to_string();
        let unique = Unique::new(0);
        let parameter_name = Name { text, unique };
        let term = Term::Lambda {
            parameter_name,
            body: Box::new(Term::Constant(Constant::Integer(1))),
        };
        self.next(term)
    }
}

impl WithTerm for Empty {
    type Next = Builder;
    fn next(self, term: Term<Name>) -> Self::Next {
        Builder {
            version: self.version,
            term,
        }
    }
}

impl<T: WithTerm> WithTerm for LambdaBuilder<T> {
    type Next = T::Next;

    fn next(self, term: Term<Name>) -> Self::Next {
        self.outer.next(term)
    }
}

impl Builder {
    pub fn new(maj: usize, min: usize, patch: usize) -> Empty {
        Empty {
            version: (maj, min, patch),
        }
    }

    pub fn build_named(&self) -> Program<Name> {
        Program {
            version: self.version,
            term: self.term.clone(),
        }
    }
}
