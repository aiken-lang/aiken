use crate::ast::{Name, Term};
use crate::program_builder::WithTerm;

pub struct ApplyBuilderFirst<T> {
    outer: T,
}

pub struct ApplyBuilderSecond<T> {
    outer: T,
    first: Term<Name>,
}

impl<T: WithTerm> WithTerm for ApplyBuilderFirst<T> {
    type Next = ApplyBuilderSecond<T>;

    fn next(self, term: Term<Name>) -> Self::Next {
        ApplyBuilderSecond {
            outer: self.outer,
            first: term,
        }
    }

    fn get_name(&self, name_str: &str) -> Name {
        self.outer.get_name(name_str)
    }
}

impl<T: WithTerm> WithTerm for ApplyBuilderSecond<T> {
    type Next = T::Next;

    fn next(self, term: Term<Name>) -> Self::Next {
        let term = Term::Apply {
            function: Box::new(self.first),
            argument: Box::new(term),
        };
        self.outer.next(term)
    }

    fn get_name(&self, name_str: &str) -> Name {
        self.outer.get_name(name_str)
    }
}

pub trait WithApply: WithTerm {
    fn with_apply(self) -> ApplyBuilderFirst<Self> {
        ApplyBuilderFirst { outer: self }
    }
}

// This is a naive blanket impl. If needed, we can control which states of the builder can
// call this by implementing manually.
impl<T: WithTerm> WithApply for T {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::program_builder::{Builder, WithConstant, WithLambda, WithVar};

    #[test]
    fn build_named__with_apply() {
        let my_var = "i_0";
        let code = r"(program
                       1.2.3
                       [(lam i_0 i_0) (con integer 1)] 
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(1, 2, 3)
            .with_apply()
            .with_lambda(my_var)
            .with_var(my_var)
            .with_int(1)
            .build_named();
        assert_eq!(expected, actual);
    }

    #[test]
    fn build_named__with_apply__with_lambda_as_arg() {
        let my_var = "i_0";
        let their_var = "i_1";
        let code = r"(program
                       1.2.3
                       [(lam i_0 i_0) (lam i_1 (con integer 1))] 
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(1, 2, 3)
            .with_apply()
            .with_lambda(my_var)
            .with_var(my_var)
            .with_lambda(their_var)
            .with_int(1)
            .build_named();
        assert_eq!(expected, actual);
    }
}
