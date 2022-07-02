use crate::ast::Term;
use crate::program_builder::WithTerm;

pub trait WithVar: WithTerm {
    fn with_var(self, name_str: &str) -> Self::Next {
        let name = self.get_name(name_str);
        let term = Term::Var(name);
        self.next(term)
    }
}

// This is a naive blanket impl. If needed, we can control which states of the builder can
// call this by implementing manually.
impl<T: WithTerm> WithVar for T {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    use crate::program_builder::{Builder, WithLambda};

    #[test]
    fn build_named__with_var() {
        let var_name = "i_0";
        let code = r"(program
                       1.2.3
                       (lam i_0 i_0)
                     )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::start(1, 2, 3)
            .with_lambda(var_name)
            .with_var(var_name)
            .build_named();
        assert_eq!(expected, actual);
    }
}
