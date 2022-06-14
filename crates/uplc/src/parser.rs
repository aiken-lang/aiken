use std::str::FromStr;

use crate::{
    ast::{Constant, Name, Program, Term},
    builtins::DefaultFunction,
};

use interner::Interner;
use peg::{error::ParseError, str::LineCol};

mod interner;

/// Parse a `Program` from a str.
pub fn program(src: &str) -> Result<Program<Name>, ParseError<LineCol>> {
    // initialize the string interner to get unique name
    let mut interner = Interner::new();

    // run the generated parser
    let mut program = uplc::program(src)?;

    // assign proper unique ids in place
    interner.program(&mut program);

    Ok(program)
}

peg::parser! {
    grammar uplc() for str {
        pub rule program() -> Program<Name>
          = _* "(" _* "program" _+ v:version() _+ t:term() _* ")" _* {
            Program {version: v, term: t}
          }

        rule version() -> (usize, usize, usize)
          = major:number() "." minor:number() "." patch:number()  {
            (major as usize, minor as usize, patch as usize)
          }

        rule term() -> Term<Name>
          = constant()
          / builtin()
          / var()
          / lambda()
          / apply()
          / delay()
          / force()
          / error()

        rule constant() -> Term<Name>
          = "(" _* "con" _+ con:(
            constant_integer()
            / constant_bytestring()
            / constant_string()
            / constant_unit()
            / constant_bool()
            ) _* ")" {
            Term::Constant(con)
          }

        rule builtin() -> Term<Name>
          = "(" _* "builtin" _+ b:ident() _* ")" {
            Term::Builtin(DefaultFunction::from_str(&b).unwrap())
          }

        rule var() -> Term<Name>
          = n:name() { Term::Var(n) }

        rule lambda() -> Term<Name>
          = "(" _* "lam" _+ parameter_name:name() _+ t:term() _* ")" {
            Term::Lambda { parameter_name, body: Box::new(t) }
          }

        #[cache_left_rec]
        rule apply() -> Term<Name>
          = "[" _* initial:term() _+ terms:(t:term() _* { t })+ "]" {
            terms
                .into_iter()
                .fold(initial, |lhs, rhs| Term::Apply {
                    function: Box::new(lhs),
                    argument: Box::new(rhs)
                })
          }

        rule delay() -> Term<Name>
          = "(" _* "delay" _+ t:term() _* ")" { Term::Delay(Box::new(t)) }

        rule force() -> Term<Name>
          = "(" _* "force" _+ t:term() _* ")" { Term::Force(Box::new(t)) }

        rule error() -> Term<Name>
          = "(" _* "error" _* ")" { Term::Error }

        rule constant_integer() -> Constant
          = "integer" _+ i:number() { Constant::Integer(i as isize) }

        rule constant_bytestring() -> Constant
          = "bytestring" _+ "#" i:ident()* {
            Constant::ByteString(hex::decode(String::from_iter(i)).unwrap())
          }

        rule constant_string() -> Constant
          = "string" _+ "\"" s:[^ '"']* "\"" { Constant::String(String::from_iter(s)) }

        rule constant_bool() -> Constant
          = "bool" _+ b:$("True" / "False") { Constant::Bool(b == "True") }

        rule constant_unit() -> Constant
          = "unit" _+ "()" { Constant::Unit }

        rule number() -> isize
          = n:$("-"* ['0'..='9']+) {? n.parse().or(Err("isize")) }

        rule name() -> Name
          = text:ident() { Name { text, unique: 0.into() } }

        rule ident() -> String
          = i:['a'..='z' | 'A'..='Z' | '0'..='9' | '_']+ {
            String::from_iter(i)
          }

        rule _ = [' ' | '\n']
    }
}

#[cfg(test)]
mod test {
    use crate::ast::{Constant, Name, Program, Term};

    #[test]
    fn parse_program() {
        let code = r#"
        (program 11.22.33
            (con integer 11)
        )
        "#;
        let program = super::program(code).unwrap();

        assert_eq!(
            program,
            Program::<Name> {
                version: (11, 22, 33),
                term: Term::Constant(Constant::Integer(11)),
            }
        );
    }
}
