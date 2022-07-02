use crate::ast::{Name, Term};
use crate::program_builder::WithTerm;

pub struct DelayBuilder<T> {
    outer: T,
}

impl<T: WithTerm> WithTerm for DelayBuilder<T> {
    type Next = T::Next;

    fn next(self, term: Term<Name>) -> Self::Next {
        let term = Term::Delay(Box::new(term));
        self.outer.next(term)
    }

    fn get_name(&self, name_str: &str) -> Name {
        self.outer.get_name(name_str)
    }
}

pub trait WithDelay: WithTerm {
    fn with_delay(self) -> DelayBuilder<Self> {
        DelayBuilder { outer: self }
    }
}

// This is a naive blanket impl. If needed, we can control which states of the builder can
// call this by implementing manually.
impl<T: WithTerm> WithDelay for T {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::program_builder::{Builder, WithConstant, WithLambda};

    #[test]
    fn build_named__with_delay() {
        let code = r"(program
                       1.2.3
                       (delay (con integer 1))
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(1, 2, 3)
            .with_delay()
            .with_int(1)
            .build_named();
        assert_eq!(expected, actual);
    }

    #[test]
    fn build_named__with_delay__with_lambda() {
        let code = r"(program
                       1.2.3
                       (delay (lam i_0 (con integer 1)))
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(1, 2, 3)
            .with_delay()
            .with_lambda("i_0")
            .with_int(1)
            .build_named();
        assert_eq!(expected, actual);
    }
}
