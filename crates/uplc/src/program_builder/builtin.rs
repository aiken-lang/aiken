use crate::ast::Term;
use crate::builtins::DefaultFunction;
use crate::program_builder::WithTerm;

pub trait WithBuiltin: WithTerm {
    // TODO: Add all the builtin variants explicitly
    fn with_builtin(self, builtin: DefaultFunction) -> Self::Next {
        let term = Term::Builtin(builtin);
        self.next(term)
    }
}

impl<T: WithTerm> WithBuiltin for T {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::program_builder::Builder;

    #[test]
    fn build_named() {
        let code = r"(program
                       11.22.33
                       (builtin addInteger)
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(11, 22, 33)
            .with_builtin(DefaultFunction::AddInteger)
            .build_named();
        assert_eq!(expected, actual);
    }
}
