use std::str::FromStr;

use crate::{
    ast::{Constant, Name, Program, Term},
    builtins::DefaultFunction,
};

use interner::Interner;
use minicbor::data::Tag;
use num_bigint::{BigInt, Sign};
use pallas_codec::utils::{KeyValuePairs, MaybeIndefArray};
use pallas_primitives::babbage::{BigInt as PlutusBigInt, Constr, PlutusData};
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
            / constant_data()
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
          = "(" _* "delay" _* t:term() _* ")" { Term::Delay(Box::new(t)) }

        rule force() -> Term<Name>
          = "(" _* "force" _* t:term() _* ")" { Term::Force(Box::new(t)) }

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

        rule constant_data() -> Constant
          = "data" _+ d:plutus_data() { Constant::Data(d) }

        rule plutus_data() -> PlutusData
          = plutus_bigint() / plutus_bytestring() / plutus_array() / plutus_map() / plutus_constr()

        rule plutus_bigint() -> PlutusData
          = n:$("-"* ['0'..='9']+) {
            PlutusData::BigInt(
              match n.parse::<BigInt>().unwrap().to_bytes_be() {
                (Sign::NoSign | Sign::Plus, bytes) => PlutusBigInt::BigUInt(bytes.into()),
                (Sign::Minus, bytes) => PlutusBigInt::BigNInt(bytes.into()),
              }.into()
            )
          }

        rule plutus_bytestring() -> PlutusData
          = "#" i:ident()* {
            PlutusData::BoundedBytes(hex::decode(String::from_iter(i)).unwrap().into())
          }

        rule plutus_array() -> PlutusData
          = "[" _* c:((_* c:plutus_data() _* {c}) ** ",") _* "]" {
            PlutusData::Array(MaybeIndefArray::Def(c.into()))
          }

        rule plutus_map() -> PlutusData
          = "{" _* c:((_* k:plutus_data() _* "=" _* v:plutus_data() {(k, v)}) ** ",") _* "}" {
            PlutusData::Map(KeyValuePairs::Def(c.into()))
          }

        rule plutus_constr() -> PlutusData
          = "(" _* "constr" _+ n:number() _+ "[" _* d:((_* c:plutus_data() _* {c}) ** ",") _* "]" {
            PlutusData::Constr(Constr {
              tag: 0xFF,
              any_constructor: Some(n.try_into().unwrap()),
              fields: MaybeIndefArray::Def(d.into())
            })
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
