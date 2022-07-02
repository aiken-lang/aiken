use crate::ast::Term;
use crate::program_builder::WithTerm;

pub trait WithError: WithTerm {
    fn with_error(self) -> Self::Next {
        let term = Term::Error;
        self.next(term)
    }
}

impl<T: WithTerm> WithError for T {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::program_builder::Builder;

    #[test]
    fn build_named__with_error() {
        let code = r"(program
                       11.22.33
                       (error)
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(11, 22, 33).with_error().build_named();
        assert_eq!(expected, actual);
    }
}
