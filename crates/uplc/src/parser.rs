use std::str::FromStr;

use crate::{
    ast::{Constant, Name, Program, Term, Type},
    builtins::DefaultFunction,
};

use interner::Interner;
use pallas_primitives::alonzo::PlutusData;
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
          = constant_term()
          / builtin()
          / var()
          / lambda()
          / apply()
          / delay()
          / force()
          / error()

        rule constant_term() -> Term<Name>
          = con:constant() {
            Term::Constant(con)
          }

        rule constant() -> Constant
          = "(" _* "con" _+ con:(
            constant_integer()
            / constant_bytestring()
            / constant_string()
            / constant_unit()
            / constant_bool()
            / constant_list()
            / constant_pair()
            ) _* ")" {
              con
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
          = "(" _* "delay" _* t:term() _* ")" { Term::Delay(Box::new(t)) }

        rule force() -> Term<Name>
          = "(" _* "force" _* t:term() _* ")" { Term::Force(Box::new(t)) }

        rule error() -> Term<Name>
          = "(" _* "error" _* ")" { Term::Error }

        rule constant_type() -> Type
          = r:(
              immediate_type:(
                "integer" { Type::Integer } /
                "bytestring" {Type::ByteString} /
                "string" {Type::String} /
                "bool" {Type::Bool} /
                "unit" {Type::Unit}
              ) {immediate_type} /
              compound_type:( "(" _*  c:(
                  "list" _+ lt:constant_type() { Type::List(Box::new(lt)) } /
                  "pair" _+ pl:constant_type() _* pr:constant_type() { Type::Pair(Box::new(pl), Box::new(pr)) }
                ) _* ")" {c}
              ) { compound_type }
            ) { r }


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

        rule constant_list() -> Constant
          = "(" _* "list" _+ t:constant_type() _* ")" _+ "[" _* contents:(ls:(t:constant() _* "," _* { t })* _* l:constant() {
            ls.into_iter().chain(std::iter::once(l)).collect::<Vec<_>>()
          })? "]" {?
              match contents {
                None => Ok(Constant::ProtoList(t, vec![])),
                Some(ls) => {
                  return if ls.iter().all(|x| Type::from(x) == t) {
                    Err("Not all members of list have declared type")
                  } else {
                    Ok(Constant::ProtoList(t, ls))
                  }
                }
              }
          }
          use pallas_primitives::alonzo::PlutusData;

        rule constant_pair() -> Constant
          = "(" _* "pair" _+ lt:constant_type() _+ rt:constant_type() _* ")" _+ "(" _* l:constant() _* "," _* r:constant() _* ")" {?
            return if Type::from(&l) != lt {
              Err("Declared left type of pair and actual type of left member distinct")
            } else if Type::from(&r) != rt {
              Err("Declared right type of pair and actual type of right member distinct")
            } else {
              Ok(Constant::ProtoPair(Type::from(&l), Type::from(&r), Box::new(l), Box::new(r)))
            }
          }

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

#[cfg(test)]
mod test2 {
    use crate::ast::{Constant, Name, Program, Term, Type};

    #[test]
    fn parse_program() {
        let code = r#"
        (program 11.22.33
            (con pair ((con list [(con integer 11), (con integer 12)]), (con bytestring #10)))
        )
        "#;
        let program = super::program(code).unwrap();

        assert_eq!(
            program,
            Program::<Name> {
                version: (11, 22, 33),
                term: Term::Constant(Constant::ProtoPair(
                    Type::List(Box::new(Type::Integer)),
                    Type::ByteString,
                    Box::new(Constant::ProtoList(
                        Type::Integer,
                        vec![Constant::Integer(11), Constant::Integer(12)]
                    )),
                    Box::new(Constant::ByteString(vec![0x10]))
                )),
            }
        );
    }
}
