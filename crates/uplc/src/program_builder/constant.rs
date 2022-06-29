use crate::ast::{Constant, Term};
use crate::program_builder::WithTerm;

pub trait WithConstant: WithTerm {
    fn with_constant_int(self, int: isize) -> Self::Next {
        let term = Term::Constant(Constant::Integer(int));
        self.next(term)
    }

    fn with_bool(self, bool: bool) -> Self::Next {
        let term = Term::Constant(Constant::Bool(bool));
        self.next(term)
    }
}

impl<T: WithTerm> WithConstant for T {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::program_builder::Builder;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn build_named__with_const(
            int: isize
        ) {
            let code = format!(r"(program
                           11.22.33
                           (con integer {})
                         )", int);
            let expected = parser::program(&code).unwrap();
            let actual = Builder::new(11, 22, 33).with_constant_int(int).build_named();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn build_named__with_true() {
        let code = r"(program
                       11.22.33
                       (con bool True)
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::new(11, 22, 33).with_bool(true).build_named();
        assert_eq!(expected, actual);
    }

    #[test]
    fn build_named__with_false() {
        let code = r"(program
                       11.22.33
                       (con bool False)
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::new(11, 22, 33).with_bool(false).build_named();
        assert_eq!(expected, actual);
    }
}
