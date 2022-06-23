#![cfg_attr(test, allow(non_snake_case))]

use uplc::ast::{Constant, Name, Program, Term, Unique};

#[cfg(test)]
mod tests;

#[derive(Default)]
pub struct Builder {
    version: (usize, usize, usize),
    term_builder: TermBuilder,
}

#[derive(Clone)]
enum TermBuilder {
    Null,
    Constant(Constant),
    Lambda(Box<TermBuilder>),
}

impl TermBuilder {
    pub fn build_named(&self) -> Term<Name> {
        match self.clone() {
            Self::Null => todo!("Make fallible?"),
            Self::Constant(c) => Term::Constant(c),
            Self::Lambda(term) => Term::Lambda {
                parameter_name: Name {
                    text: "i".to_string(),
                    unique: Unique::new(0),
                },
                body: Box::new(term.build_named()),
            },
        }
    }
}

impl Default for TermBuilder {
    fn default() -> Self {
        Self::Null
    }
}

impl Builder {
    pub fn with_version(&mut self, maj: usize, min: usize, patch: usize) -> &mut Self {
        self.version = (maj, min, patch);
        self
    }

    pub fn with_constant_int(&mut self, int: isize) -> &mut Self {
        self.term_builder = TermBuilder::Constant(Constant::Integer(int));
        self
    }

    pub fn with_lambda(&mut self, int: isize) -> &mut Self {
        let inner = TermBuilder::Constant(Constant::Integer(int));
        self.term_builder = TermBuilder::Lambda(Box::new(inner));
        self
    }

    pub fn build_named(&self) -> Program<Name> {
        Program {
            version: self.version,
            term: self.term_builder.build_named(),
        }
    }
}
