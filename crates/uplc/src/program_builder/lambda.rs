use std::rc::Rc;

use crate::ast::{Name, Term};
use crate::program_builder::WithTerm;

pub struct LambdaBuilder<T> {
    outer: T,
    parameter_name: Name,
}

impl<T: WithTerm> WithTerm for LambdaBuilder<T> {
    type Next = T::Next;

    fn next(self, term: Term<Name>) -> Self::Next {
        let term = Term::Lambda {
            parameter_name: self.parameter_name.into(),
            body: Rc::new(term),
        };
        self.outer.next(term)
    }

    fn get_name(&self, name_str: &str) -> Name {
        self.outer.get_name(name_str)
    }
}

pub trait WithLambda: WithTerm {
    fn with_lambda(self, name_str: &str) -> LambdaBuilder<Self> {
        let parameter_name = self.get_name(name_str);
        LambdaBuilder {
            outer: self,
            parameter_name,
        }
    }
}

// This is a naive blanket impl. If needed, we can control which states of the builder can
// call this by implementing manually.
impl<T: WithTerm> WithLambda for T {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::program_builder::constant::WithConstant;
    use crate::program_builder::Builder;

    #[test]
    fn build_named__with_lam() {
        let code = r"(program
                       1.2.3
                       (lam i_0 (con integer 1))
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(1, 2, 3)
            .with_lambda("i_0")
            .with_int(1)
            .build_named();
        assert_eq!(expected, actual);
    }

    #[test]
    fn build_named__with_nested_lam() {
        let code = r"(program
                       1.2.3
                       (lam i_0 (lam i_1 (con integer 1)))
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(1, 2, 3)
            .with_lambda("i_0")
            .with_lambda("i_1")
            .with_int(1)
            .build_named();
        assert_eq!(expected, actual);
    }
}
