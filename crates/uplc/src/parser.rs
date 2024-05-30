use std::{ops::Neg, rc::Rc, str::FromStr};

use crate::{
    ast::{Constant, Name, Program, Term, Type},
    builtins::DefaultFunction,
    machine::runtime::Compressable,
    machine::value::to_pallas_bigint,
};

use interner::Interner;
use num_bigint::BigInt;
use pallas_primitives::alonzo::PlutusData;
use peg::{error::ParseError, str::LineCol};

pub mod interner;

/// Parse a `Program` from a str.
pub fn program(src: &str) -> Result<Program<Name>, ParseError<LineCol>> {
    // initialize the string interner to get unique name
    let mut interner = Interner::new();

    // run the generated parser
    let program = uplc::program(src, &mut interner)?;

    Ok(program)
}

pub fn term(src: &str) -> Result<Term<Name>, ParseError<LineCol>> {
    // initialize the string interner to get unique name
    let mut interner = Interner::new();

    // run the generated parser
    let term = uplc::term(src, &mut interner)?;

    Ok(term)
}

// Returns the inner type of a list, provided that the given type is a list.
fn list_sub_type(type_info: Option<&Type>) -> Option<&Type> {
    match type_info {
        Some(Type::List(t)) => Some(t),
        _ => None,
    }
}

// Returns the left and right types of a pair, provided that the given type is a pair.
fn pair_sub_type(type_info: Option<&Type>) -> Option<(&Type, &Type)> {
    match type_info {
        Some(Type::Pair(l, r)) => Some((l, r)),
        _ => None,
    }
}

pub fn escape(string: &str) -> String {
    string
        .chars()
        .flat_map(|c| match c {
            '\n' => vec!['\\', c],
            '\r' => vec!['\\', c],
            '\t' => vec!['\\', c],
            '\'' => vec!['\\', c],
            '\\' => vec!['\\', c],
            '"' => vec!['\\', c],
            _ => vec![c],
        })
        .collect::<String>()
}

peg::parser! {
    grammar uplc() for str {
        pub rule program(interner: &mut Interner) -> Program<Name>
          = _* "(" _* "program" _+ v:version() _+ t:term(interner) _* ")" _* {
            Program {version: v, term: t}
          }

        rule comma() = _* "," _*

        rule version() -> (usize, usize, usize)
          = major:decimal() "." minor:decimal() "." patch:decimal()  {
            (major, minor, patch)
          }

        pub rule term(interner: &mut Interner) -> Term<Name>
          = constant()
          / builtin()
          / var(interner)
          / lambda(interner)
          / apply(interner)
          / delay(interner)
          / force(interner)
          / error()
          / constr(interner)
          / case(interner)

        rule constant() -> Term<Name>
          = "(" _* "con" _+ con:(
            constant_integer()
            / constant_bytestring()
            / constant_string()
            / constant_unit()
            / constant_bool()
            / constant_data()
            / constant_g1_element()
            / constant_g2_element()
            / constant_list()
            / constant_pair()
            ) _* ")" {
            Term::Constant(con.into())
          }

        rule builtin() -> Term<Name>
          = "(" _* "builtin" _+ b:ident() _* ")" {
            Term::Builtin(DefaultFunction::from_str(&b).unwrap())
          }

        rule var(interner: &mut Interner) -> Term<Name>
          = n:name(interner) { Term::Var(n.into()) }

        rule lambda(interner: &mut Interner) -> Term<Name>
          = "(" _* "lam" _+ parameter_name:name(interner) _+ t:term(interner) _* ")" {
            Term::Lambda { parameter_name: parameter_name.into(), body: Rc::new(t) }
          }

        rule apply(interner: &mut Interner) -> Term<Name>
          = "[" _* initial:term(interner) _+ terms:(t:term(interner) _* { t })+ "]" {
            terms
                .into_iter()
                .fold(initial, |lhs, rhs| Term::Apply {
                    function: Rc::new(lhs),
                    argument: Rc::new(rhs)
                })
          }

        rule delay(interner: &mut Interner) -> Term<Name>
          = "(" _* "delay" _* t:term(interner) _* ")" { Term::Delay(Rc::new(t)) }

        rule force(interner: &mut Interner) -> Term<Name>
          = "(" _* "force" _* t:term(interner) _* ")" { Term::Force(Rc::new(t)) }

        rule error() -> Term<Name>
          = "(" _* "error" _* ")" { Term::Error }

        rule constr(interner: &mut Interner) -> Term<Name>
          = "(" _* "constr" _+ tag:decimal() _* fields:(t:term(interner) _* { t })* _* ")" {
            Term::Constr { tag, fields }
          }

        rule case(interner: &mut Interner) -> Term<Name>
          = "(" _* "case" _+ constr:term(interner) _* branches:(t:term(interner) _* { t })* _* ")" {
            Term::Case { constr: constr.into(), branches }
          }

        rule constant_integer() -> Constant
          = "integer" _+ i:big_number() { Constant::Integer(i) }

        rule constant_bytestring() -> Constant
          = "bytestring" _+ bs:bytestring() { Constant::ByteString(bs) }

        rule constant_string() -> Constant
          = "string" _+ s:string() { Constant::String(s) }

        rule constant_bool() -> Constant
          = "bool" _+ b:boolean() { Constant::Bool(b) }

        rule constant_unit() -> Constant
          = "unit" _+ "()" { Constant::Unit }

        rule constant_data() -> Constant
          = "data" _+ "(" _* d:data() _* ")" { Constant::Data(d) }

        rule constant_g1_element() -> Constant
          = "bls12_381_G1_element" _+ element:g1_element() {
                Constant::Bls12_381G1Element(Box::new(element))
            }

        rule constant_g2_element() -> Constant
          = "bls12_381_G2_element" _+ element:g2_element() {
                Constant::Bls12_381G2Element(Box::new(element))
            }

        rule constant_list() -> Constant
          = "(" _* "list" _* t:type_info() _* ")" _+ ls:list(Some(&t)) {
            Constant::ProtoList(t, ls)
          }

        rule constant_pair() -> Constant
          = "(" _* "pair" _+ l:type_info() _+ r:type_info() _* ")" _+ p:pair(Some((&l, &r))) {
            Constant::ProtoPair(l, r, p.0.into(), p.1.into())
          }

        rule pair(type_info: Option<(&Type, &Type)>) -> (Constant, Constant)
          = "(" _* x:typed_constant(type_info.map(|t| t.0)) comma() y:typed_constant(type_info.map(|t| t.1)) _* ")" { (x, y) }

        rule decimal() -> usize
          = n:$(['0'..='9']+) {? n.parse().or(Err("usize")) }

        rule number() -> isize
          = n:$("-"* ['0'..='9']+) {? n.parse().or(Err("isize")) }

        rule big_number() -> BigInt
          = n:$("-"* ['0'..='9']+) {? (if n.starts_with('-') { BigInt::parse_bytes(&n.as_bytes()[1..], 10).map(|i| i.neg()) } else { BigInt::parse_bytes(n.as_bytes(), 10) }).ok_or("BigInt") }

        rule boolean() -> bool
          = b:$("True" / "False") { b == "True" }

        rule bytestring() -> Vec<u8>
          = "#" i:ident()* {?
              hex::decode(String::from_iter(i)).map_err(|_| "Invalid bytestring")
          }

        rule bls_element() -> Vec<u8>
          = "0x" i:ident()* {?
              hex::decode(String::from_iter(i)).map_err(|_| "Invalid bls element hex")
            }

        rule g1_element() -> blst::blst_p1
          = element:bls_element() {?
              blst::blst_p1::uncompress(&element).map_err(|_| "Invalid bls g1 element encoding")
            }

        rule g2_element() -> blst::blst_p2
          = element:bls_element() {?
              blst::blst_p2::uncompress(&element).map_err(|_| "Invalid bls g2 element encoding")
        }

        rule string() -> String
          = "\"" s:character()* "\"" { String::from_iter(s) }

        rule character() -> char
          = "\\n"  { '\n' } // newline (line feed)
          / "\\r"  { '\r' } // carriage return
          / "\\t"  { '\t' } // horizontal tab
          / "\\\"" { '\"' } // double quote
          / "\\'"  { '\'' } // single quote
          / "\\\\" { '\\' } // backslash
          / "\\x" i:character() i2:character() {? match hex::decode([i,i2].iter().collect::<String>()) {
              Ok(res) => {Ok(res[0].into())},
              Err(_) => {Err("Invalid hex encoding of escaped byte")},
          } } // hex encoded byte
          / [ ^ '"' ]
          / expected!("or any valid ascii character")

        rule data() -> PlutusData
          = _* "Constr" _+ t:decimal() _+ fs:plutus_list() {?
            Ok(crate::ast::Data::constr(
                u64::try_from(t).or(Err("tag"))?,
                fs,
            ))
          }
          / _* "Map" _+ kvps:plutus_key_value_pairs() {
            PlutusData::Map(pallas_codec::utils::KeyValuePairs::Def(kvps))
          }
          / _* "List" _+ ls:plutus_list() { PlutusData::Array(ls) }
          / _* "I" _+ n:big_number() { PlutusData::BigInt(to_pallas_bigint(&n)) }
          / _* "B" _+ "#" i:ident()* {?
            Ok(PlutusData::BoundedBytes(
              hex::decode(String::from_iter(i)).or(Err("bytes"))?.into()
            ))
          }

        rule plutus_list() -> Vec<PlutusData>
          = "[" _* xs:(data() ** comma()) _* "]" { xs }

        rule plutus_key_value_pairs() -> Vec<(PlutusData, PlutusData)>
          = "[" _* kvps:(plutus_key_value_pair() ** comma()) _* "]" { kvps }

        rule plutus_key_value_pair() -> (PlutusData, PlutusData)
          = "(" _* k:data() comma() v:data() _* ")" { (k, v) }

        rule list(type_info: Option<&Type>) -> Vec<Constant>
          = "[" _* xs:(typed_constant(type_info) ** comma()) _* "]" { xs }

        rule typed_constant(type_info : Option<&Type>) -> Constant
          = "()" {?
              match type_info {
                Some(Type::Unit) => Ok(Constant::Unit),
                _ => Err("found 'Unit' instead of expected type")
              }
            }
          / b:boolean() {?
              match type_info {
                Some(Type::Bool) => Ok(Constant::Bool(b)),
                _ => Err("found 'Bool' instead of expected type")
              }
            }
          / n:big_number() {?
              match type_info {
                Some(Type::Integer) => Ok(Constant::Integer(n)),
                _ => Err("found 'Integer' instead of expected type")
              }
            }
          / bs:bytestring() {?
              match type_info {
                Some(Type::ByteString) => Ok(Constant::ByteString(bs)),
                _ => Err("found 'ByteString' instead of expected type")
              }
            }
          / s:string() {?
              match type_info {
                Some(Type::String) => Ok(Constant::String(s)),
                _ => Err("found 'String' instead of expected type")
              }
            }
            / s:data() {?
                match type_info {
                    Some(Type::Data) => Ok(Constant::Data(s)),
                    _ => Err("found 'Data' instead of expected type")
                }
            }
            / element:g1_element() {?
                match type_info {
                    Some(Type::Bls12_381G1Element) => Ok(Constant::Bls12_381G1Element(Box::new(element))),
                    _ => Err("found 'Bls12_381G1Element' instead of expected type")

                }
            }
            / element:g2_element() {?
                match type_info {
                    Some(Type::Bls12_381G2Element) => Ok(Constant::Bls12_381G2Element(Box::new(element))),
                    _ => Err("found 'Bls12_381G2Element' instead of expected type")

                }
            }
            / ls:list(list_sub_type(type_info)) {?
                match type_info {
                    Some(Type::List(t)) => Ok(Constant::ProtoList(t.as_ref().clone(), ls)),
                    _ => Err("found 'List' instead of expected type")
                }
            }
            / p:pair(pair_sub_type(type_info)) {?
                match type_info {
                    Some(Type::Pair(l, r)) =>
                        Ok(
                            Constant::ProtoPair(
                                l.as_ref().clone(),
                                r.as_ref().clone(),
                                p.0.into(),
                                p.1.into()
                            )
                        ),
                    _ => Err("found 'Pair' instead of expected type")
                }
            }

        rule type_info() -> Type
          = _* "unit" { Type::Unit }
          / _* "bool" { Type::Bool }
          / _* "integer" { Type::Integer }
          / _* "bytestring" { Type::ByteString }
          / _* "string" { Type::String }
          / _* "data" { Type::Data }
          / _* "bls12_381_G1_element" { Type::Bls12_381G1Element }
          / _* "bls12_381_G1_element" { Type::Bls12_381G2Element }
          / _* "(" _* "list" _+ t:type_info() _* ")" {
              Type::List(t.into())
            }
          / _* "(" _* "pair" _+ l:type_info() _+ r:type_info() _* ")" {
              Type::Pair(l.into(), r.into())
            }

        rule name(interner: &mut Interner) -> Name
          = text:ident() {
                let unique = interner.intern(&text);
                Name { text, unique }
            }

        rule ident() -> String
          = i:['a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '\'']+ {
            String::from_iter(i)
          }

        rule _ = [' ' | '\n' | '\r' | '\t'] / "--" $([^ '\n']*) "\n"
    }
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;
    use pretty_assertions::assert_eq;

    use crate::ast::{Constant, Name, Program, Term, Type, Unique};
    use crate::builtins::DefaultFunction;
    use std::rc::Rc;

    #[test]
    fn parse_apply() {
        let uplc = "(program 1.0.0 [(lam x x) (con integer 0)])";
        let x = Name {
            text: "x".to_string(),
            unique: Unique::new(0),
        };
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (1, 0, 0),
                term: Term::Apply {
                    function: Rc::new(Term::Lambda {
                        parameter_name: x.clone().into(),
                        body: Rc::new(Term::Var(x.into())),
                    }),
                    argument: Rc::new(Term::Constant(Constant::Integer(0.into()).into()))
                }
            }
        )
    }

    #[test]
    fn parse_lambda() {
        let uplc = "(program 1.0.0 (lam x x))";
        let x = Name {
            text: "x".to_string(),
            unique: Unique::new(0),
        };
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (1, 0, 0),
                term: Term::Lambda {
                    parameter_name: x.clone().into(),
                    body: Rc::new(Term::Var(x.into())),
                }
            }
        )
    }

    #[test]
    fn parse_delay_lambda() {
        let uplc = "(program 1.0.0 (lam x (delay x)))";
        let x = Name {
            text: "x".to_string(),
            unique: Unique::new(0),
        };
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (1, 0, 0),
                term: Term::Lambda {
                    parameter_name: x.clone().into(),
                    body: Rc::new(Term::Delay(Rc::new(Term::Var(x.into())))),
                }
            }
        )
    }

    #[test]
    fn parse_error() {
        let uplc = "(program 1.0.0 (error))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (1, 0, 0),
                term: Term::Error
            }
        )
    }

    #[test]
    fn parse_formatted() {
        let uplc = r#"
        (program 11.22.33
            (con integer 11)
        )
        "#;
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (11, 22, 33),
                term: Term::Constant(Constant::Integer(11.into()).into()),
            }
        );
    }

    #[test]
    fn parse_builtin_add_integer_curried() {
        parse_builtin_integer(
            "(program 1.0.0 [ [ (builtin addInteger) (con integer 1)] (con integer 1) ])",
            DefaultFunction::AddInteger,
            1,
            1,
        );
    }

    #[test]
    fn parse_builtin_add_integer() {
        parse_builtin_integer(
            "(program 1.0.0 [ (builtin addInteger) (con integer 1) (con integer 2) ])",
            DefaultFunction::AddInteger,
            1,
            2,
        );
    }

    #[test]
    fn parse_builtin_subtract_integer() {
        parse_builtin_integer(
            "(program 1.0.0 [ (builtin subtractInteger) (con integer 42) (con integer 14) ])",
            DefaultFunction::SubtractInteger,
            42,
            14,
        )
    }

    #[test]
    fn parse_builtin_multiply_integer() {
        parse_builtin_integer(
            "(program 1.0.0 [ (builtin multiplyInteger) (con integer 1) (con integer -1) ])",
            DefaultFunction::MultiplyInteger,
            1,
            -1,
        )
    }

    #[test]
    fn parse_builtin_divide_integer() {
        parse_builtin_integer(
            "(program 1.0.0 [ (builtin divideInteger) (con integer 1) (con integer 0) ])",
            DefaultFunction::DivideInteger,
            1,
            0,
        )
    }

    #[test]
    fn parse_builtin_quotient_integer() {
        parse_builtin_integer(
            "(program 1.0.0 [ (builtin quotientInteger) (con integer 1) (con integer 0) ])",
            DefaultFunction::QuotientInteger,
            1,
            0,
        )
    }

    #[test]
    fn parse_builtin_remainder_integer() {
        parse_builtin_integer(
            "(program 1.0.0 [ (builtin remainderInteger) (con integer 1) (con integer 0) ])",
            DefaultFunction::RemainderInteger,
            1,
            0,
        )
    }

    #[test]
    fn parse_builtin_mod_integer() {
        parse_builtin_integer(
            "(program 1.0.0 [ [ (builtin modInteger) (con integer 2) ] (con integer 3) ])",
            DefaultFunction::ModInteger,
            2,
            3,
        )
    }

    #[test]
    fn parse_builtin_equals_integer() {
        parse_builtin_integer(
            "(program 1.0.0 [ [ (builtin equalsInteger) (con integer 1) ] (con integer 2) ])",
            DefaultFunction::EqualsInteger,
            1,
            2,
        )
    }

    #[test]
    fn parse_builtin_less_than_integer() {
        parse_builtin_integer(
            "(program 1.0.0 [ [ (builtin lessThanInteger) (con integer 1) ] (con integer 2) ])",
            DefaultFunction::LessThanInteger,
            1,
            2,
        )
    }

    #[test]
    fn parse_builtin_less_than_equals_integer() {
        parse_builtin_integer(
            "(program 1.0.0 [ [ (builtin lessThanEqualsInteger) (con integer 1) ] (con integer 2) ])",
            DefaultFunction::LessThanEqualsInteger,
            1,
            2,
        )
    }

    #[test]
    fn parse_builtin_append_bytestring() {
        let uplc = "(program 1.0.0 [ [(builtin appendByteString) (con bytestring #00FF)] (con bytestring #FF00) ])";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (1, 0, 0),
                term: Term::Apply {
                    function: Rc::new(Term::Apply {
                        function: Rc::new(Term::Builtin(DefaultFunction::AppendByteString)),
                        argument: Rc::new(Term::Constant(
                            Constant::ByteString(vec![0x00, 0xFF]).into()
                        )),
                    }),
                    argument: Rc::new(Term::Constant(
                        Constant::ByteString(vec![0xFF, 0x00]).into()
                    ))
                }
            }
        )
    }

    #[test]
    fn parse_builtin_cons_bytestring() {
        let uplc =
            "(program 1.0.0 [(builtin consByteString) (con integer 256) (con bytestring #)])";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (1, 0, 0),
                term: Term::Apply {
                    function: Rc::new(Term::Apply {
                        function: Rc::new(Term::Builtin(DefaultFunction::ConsByteString)),
                        argument: Rc::new(Term::Constant(Constant::Integer(256.into()).into())),
                    }),
                    argument: Rc::new(Term::Constant(Constant::ByteString(vec![]).into()))
                }
            }
        )
    }

    #[test]
    fn parse_builtin_slice_bytestring() {
        let uplc = "(program 0.0.0 [ [ [ (builtin sliceByteString) (con integer 1)] (con integer 2) ] (con bytestring #00ffaa) ])";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Apply {
                    function: Rc::new(Term::Apply {
                        function: Rc::new(Term::Apply {
                            function: Rc::new(Term::Builtin(DefaultFunction::SliceByteString)),
                            argument: Rc::new(Term::Constant(Constant::Integer(1.into()).into())),
                        }),
                        argument: Rc::new(Term::Constant(Constant::Integer(2.into()).into())),
                    }),
                    argument: Rc::new(Term::Constant(
                        Constant::ByteString(vec![0x00, 0xFF, 0xAA]).into()
                    ))
                }
            }
        )
    }

    #[test]
    fn parse_builtin_length_of_bytestring() {
        let uplc = "(program 0.0.0 [ (builtin lengthOfByteString) (con bytestring #00ffaa) ])";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Apply {
                    function: Rc::new(Term::Builtin(DefaultFunction::LengthOfByteString)),
                    argument: Rc::new(Term::Constant(
                        Constant::ByteString(vec![0x00, 0xFF, 0xAA]).into()
                    ))
                },
            }
        )
    }

    #[test]
    fn parse_builtin_index_bytestring() {
        let uplc = "(program 1.0.0 [(builtin indexByteString) (con bytestring #00) (con integer 9223372036854775808)])";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (1, 0, 0),
                term: Term::Apply {
                    function: Rc::new(Term::Apply {
                        function: Rc::new(Term::Builtin(DefaultFunction::IndexByteString)),
                        argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00]).into()))
                    }),
                    argument: Rc::new(Term::Constant(
                        Constant::Integer(
                            BigInt::parse_bytes("9223372036854775808".as_bytes(), 10).unwrap()
                        )
                        .into()
                    )),
                }
            }
        )
    }

    #[test]
    fn parse_builtin_equals_bytestring() {
        let uplc = "(program 0.0.0 [ [ (builtin equalsByteString) (con bytestring #00ffaa) ] (con bytestring #00ffaa) ])";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Apply {
                    function: Rc::new(Term::Apply {
                        function: Rc::new(Term::Builtin(DefaultFunction::EqualsByteString)),
                        argument: Rc::new(Term::Constant(
                            Constant::ByteString(vec![0x00, 0xff, 0xaa]).into()
                        ))
                    }),
                    argument: Rc::new(Term::Constant(
                        Constant::ByteString(vec![0x00, 0xff, 0xaa]).into()
                    )),
                }
            }
        )
    }

    #[test]
    fn parse_builtin_less_than_bytestring() {
        let uplc = "(program 0.0.0 [ [(builtin lessThanByteString) (con bytestring #00ff)] (con bytestring #00ffaa) ])";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Apply {
                    function: Rc::new(Term::Apply {
                        function: Rc::new(Term::Builtin(DefaultFunction::LessThanByteString)),
                        argument: Rc::new(Term::Constant(
                            Constant::ByteString(vec![0x00, 0xff]).into()
                        ))
                    }),
                    argument: Rc::new(Term::Constant(
                        Constant::ByteString(vec![0x00, 0xff, 0xaa]).into()
                    )),
                }
            }
        )
    }

    #[test]
    fn parse_builtin_less_than_equals_bytestring() {
        let uplc = "(program 0.0.0 [ [(builtin lessThanEqualsByteString) (con bytestring #00ff)] (con bytestring #00) ])";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Apply {
                    function: Rc::new(Term::Apply {
                        function: Rc::new(Term::Builtin(DefaultFunction::LessThanEqualsByteString)),
                        argument: Rc::new(Term::Constant(
                            Constant::ByteString(vec![0x00, 0xff]).into()
                        ))
                    }),
                    argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00]).into())),
                }
            }
        )
    }

    #[test]
    fn parse_list_empty() {
        let uplc = "(program 0.0.0 (con (list unit) []))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoList(Type::Unit, vec![]).into())
            }
        )
    }

    #[test]
    fn parse_list_singleton_unit() {
        let uplc = "(program 0.0.0 (con (list unit) [ () ]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoList(Type::Unit, vec![Constant::Unit]).into())
            }
        )
    }

    #[test]
    fn parse_list_bools() {
        let uplc = "(program 0.0.0 (con (list bool) [True, False, True]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(
                    Constant::ProtoList(
                        Type::Bool,
                        vec![
                            Constant::Bool(true),
                            Constant::Bool(false),
                            Constant::Bool(true)
                        ]
                    )
                    .into()
                )
            }
        )
    }

    #[test]
    fn parse_list_bytestrings() {
        let uplc = "(program 0.0.0 (con (list bytestring) [#00, #01]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(
                    Constant::ProtoList(
                        Type::ByteString,
                        vec![
                            Constant::ByteString(vec![0x00]),
                            Constant::ByteString(vec![0x01]),
                        ]
                    )
                    .into()
                )
            }
        )
    }

    #[test]
    fn parse_list_list_integers() {
        let uplc = "(program 0.0.0 (con (list (list integer)) [[14,42], [1337]]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(
                    Constant::ProtoList(
                        Type::List(Type::Integer.into()),
                        vec![
                            Constant::ProtoList(
                                Type::Integer,
                                vec![Constant::Integer(14.into()), Constant::Integer(42.into())]
                            ),
                            Constant::ProtoList(
                                Type::Integer,
                                vec![Constant::Integer(1337.into())]
                            )
                        ]
                    )
                    .into()
                )
            }
        )
    }

    #[test]
    fn parse_list_multiline() {
        let uplc = r#"
        (program 0.0.0
            (con (list integer)
              [ 14
              , 42
              ]
            )
        )"#;
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(
                    Constant::ProtoList(
                        Type::Integer,
                        vec![Constant::Integer(14.into()), Constant::Integer(42.into())],
                    )
                    .into()
                )
            }
        )
    }

    #[test]
    fn parse_pair_unit_unit() {
        let uplc = "(program 0.0.0 (con (pair unit unit) ((),())))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(
                    Constant::ProtoPair(
                        Type::Unit,
                        Type::Unit,
                        Constant::Unit.into(),
                        Constant::Unit.into()
                    )
                    .into()
                )
            }
        )
    }

    #[test]
    fn parse_pair_bool_pair_integer_bytestring() {
        let uplc = "(program 0.0.0 (con (pair bool (pair integer bytestring)) (True, (14, #42))))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(
                    Constant::ProtoPair(
                        Type::Bool,
                        Type::Pair(Type::Integer.into(), Type::ByteString.into()),
                        Constant::Bool(true).into(),
                        Constant::ProtoPair(
                            Type::Integer,
                            Type::ByteString,
                            Constant::Integer(14.into()).into(),
                            Constant::ByteString(vec![0x42]).into(),
                        )
                        .into()
                    )
                    .into()
                )
            }
        )
    }

    #[test]
    fn parse_pair_string_list_integer() {
        let uplc = "(program 0.0.0 (con (pair string (list integer)) (\"foo\", [14, 42])))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(
                    Constant::ProtoPair(
                        Type::String,
                        Type::List(Type::Integer.into()),
                        Constant::String(String::from("foo")).into(),
                        Constant::ProtoList(
                            Type::Integer,
                            vec![Constant::Integer(14.into()), Constant::Integer(42.into())],
                        )
                        .into()
                    )
                    .into()
                )
            }
        )
    }

    #[test]
    fn parse_pair_multiline() {
        let uplc = r#"
        (program 0.0.0
            (con (pair integer integer)
              (14, 42)
            )
        )"#;
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(
                    Constant::ProtoPair(
                        Type::Integer,
                        Type::Integer,
                        Constant::Integer(14.into()).into(),
                        Constant::Integer(42.into()).into()
                    )
                    .into()
                )
            }
        )
    }

    #[test]
    fn parse_list_type_mismatch() {
        let uplc = "(program 0.0.0 (con (list integer) [True, False]))";
        assert!(super::program(uplc).is_err())
    }

    #[test]
    fn parse_list_mixed_types() {
        let uplc = "(program 0.0.0 (con (list integer) [14, False]))";
        assert!(super::program(uplc).is_err())
    }

    // Helper function for all simple programs that involve only a direct application of a builtin
    // function operating on two integers.
    fn parse_builtin_integer(uplc: &str, default_function: DefaultFunction, x: i128, y: i128) {
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (1, 0, 0),
                term: Term::Apply {
                    function: Rc::new(Term::Apply {
                        function: Rc::new(Term::Builtin(default_function)),
                        argument: Rc::new(Term::Constant(Constant::Integer(x.into()).into())),
                    }),
                    argument: Rc::new(Term::Constant(Constant::Integer(y.into()).into()))
                }
            }
        )
    }
}
