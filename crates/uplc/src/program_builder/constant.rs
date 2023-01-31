use crate::ast::{Constant, Term};
use crate::program_builder::WithTerm;

pub trait WithConstant: WithTerm {
    fn with_int(self, int: i128) -> Self::Next {
        let term = Term::Constant(Constant::Integer(int).into());
        self.next(term)
    }

    fn with_byte_string(self, bytes: Vec<u8>) -> Self::Next {
        let term = Term::Constant(Constant::ByteString(bytes).into());
        self.next(term)
    }

    fn with_string(self, string: String) -> Self::Next {
        let term = Term::Constant(Constant::String(string).into());
        self.next(term)
    }

    fn with_unit(self) -> Self::Next {
        let term = Term::Constant(Constant::Unit.into());
        self.next(term)
    }

    fn with_bool(self, bool: bool) -> Self::Next {
        let term = Term::Constant(Constant::Bool(bool).into());
        self.next(term)
    }
}

// This is a naive blanket impl. If needed, we can control which states of the builder can
// call this by implementing manually.
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
            int: i128
        ) {
            let code = format!(r"(program
                           11.22.33
                           (con integer {})
                         )", int);
            let expected = parser::program(&code).unwrap();
            let actual = Builder::start(11, 22, 33).with_int(int).build_named();
            assert_eq!(expected, actual);
        }
    }

    proptest! {
        #[test]
        fn build_named__with_bytestring(
           bytes: Vec<u8>
        ) {
            let bstring = hex::encode(&bytes);
            let code = format!(r"(program
                           11.22.33
                           (con bytestring #{})
                         )", bstring);
            let expected = parser::program(&code).unwrap();
            let actual = Builder::start(11, 22, 33)
                .with_byte_string(bytes)
                .build_named();
            assert_eq!(expected, actual);
        }
    }

    prop_compose! {
        fn safe_string()(
            some_string: String
        ) -> String {
            some_string.chars().filter(|a| *a != '\"').collect()
        }
    }

    proptest! {
        #[test]
        fn build_named__with_string(
            some_string in safe_string()
        ) {
            let code = format!(
                r#"(program
                               11.22.33
                               (con string "{}")
                             )"#,
                &some_string
            );
            let expected = parser::program(&code).unwrap();
            let actual = Builder::start(11, 22, 33)
                .with_string(some_string)
                .build_named();
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn build_named__with_unit() {
        let code = r"(program
                       11.22.33
                       (con unit ())
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(11, 22, 33).with_unit().build_named();
        assert_eq!(expected, actual);
    }

    #[test]
    fn build_named__with_true() {
        let code = r"(program
                       11.22.33
                       (con bool True)
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(11, 22, 33).with_bool(true).build_named();
        assert_eq!(expected, actual);
    }

    #[test]
    fn build_named__with_false() {
        let code = r"(program
                       11.22.33
                       (con bool False)
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(11, 22, 33).with_bool(false).build_named();
        assert_eq!(expected, actual);
    }
}
