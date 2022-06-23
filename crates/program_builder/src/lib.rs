use uplc::ast::{Constant, DeBruijn, Name, Program, Term};

#[derive(Default)]
pub struct Builder {
    // version: (usize, usize, usize),
}

impl Builder {
    pub fn build_named(&self) -> Program<Name> {
        Program {
            version: (11, 22, 33),
            term: Term::Constant(Constant::Integer(11)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Builder;
    use uplc::parser;

    #[test]
    fn it_works() {
        let code = r"(program
                           11.22.33
                           (con integer 11)
                         )";
        let expected = parser::program(code).unwrap();
        let actual = Builder::default().build_named();
        assert_eq!(expected, actual);
    }
}
