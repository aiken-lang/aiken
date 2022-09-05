use std::rc::Rc;

use crate::ast::{Name, Term};
use crate::program_builder::WithTerm;

pub struct ForceBuilder<T> {
    outer: T,
}

impl<T: WithTerm> WithTerm for ForceBuilder<T> {
    type Next = T::Next;

    fn next(self, term: Term<Name>) -> Self::Next {
        let term = Term::Force(Rc::new(term));
        self.outer.next(term)
    }

    fn get_name(&self, name_str: &str) -> Name {
        self.outer.get_name(name_str)
    }
}

pub trait WithForce: WithTerm {
    fn with_force(self) -> ForceBuilder<Self> {
        ForceBuilder { outer: self }
    }
}

// This is a naive blanket impl. If needed, we can control which states of the builder can
// call this by implementing manually.
impl<T: WithTerm> WithForce for T {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::program_builder::{Builder, WithConstant, WithLambda};

    #[test]
    fn build_named__with_force() {
        let code = r"(program
                       1.2.3
                       (force (lam i_0 (con integer 1)))
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(1, 2, 3)
            .with_force()
            .with_lambda("i_0")
            .with_int(1)
            .build_named();
        assert_eq!(expected, actual);
    }
}
