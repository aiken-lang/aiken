use std::{rc::Rc, str::FromStr};

use crate::{
    ast::{Constant, Name, Program, Term, Type},
    builtins::DefaultFunction,
};

use interner::Interner;
use pallas_primitives::{alonzo::PlutusData, Fragment};
use peg::{error::ParseError, str::LineCol};

pub mod interner;

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

pub fn term(src: &str) -> Result<Term<Name>, ParseError<LineCol>> {
    // initialize the string interner to get unique name
    let mut interner = Interner::new();

    // run the generated parser
    let mut term = uplc::term(src)?;

    // assign proper unique ids in place
    interner.term(&mut term);

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

        pub rule term() -> Term<Name>
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
            / constant_list()
            / constant_pair()
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
            Term::Lambda { parameter_name, body: Rc::new(t) }
          }

        #[cache_left_rec]
        rule apply() -> Term<Name>
          = "[" _* initial:term() _+ terms:(t:term() _* { t })+ "]" {
            terms
                .into_iter()
                .fold(initial, |lhs, rhs| Term::Apply {
                    function: Rc::new(lhs),
                    argument: Rc::new(rhs)
                })
          }

        rule delay() -> Term<Name>
          = "(" _* "delay" _* t:term() _* ")" { Term::Delay(Rc::new(t)) }

        rule force() -> Term<Name>
          = "(" _* "force" _* t:term() _* ")" { Term::Force(Rc::new(t)) }

        rule error() -> Term<Name>
          = "(" _* "error" _* ")" { Term::Error }

        rule constant_integer() -> Constant
          = "integer" _+ i:big_number() { Constant::Integer(i as i128) }

        rule constant_bytestring() -> Constant
          = "bytestring" _+ bs:bytestring() { Constant::ByteString(bs) }

        rule constant_string() -> Constant
          = "string" _+ s:string() { Constant::String(s) }

        rule constant_bool() -> Constant
          = "bool" _+ b:boolean() { Constant::Bool(b) }

        rule constant_unit() -> Constant
          = "unit" _+ "()" { Constant::Unit }

        rule constant_data() -> Constant
          = "data" _+ d:data() { Constant::Data(d) }

        rule constant_list() -> Constant
          = "list" _* "<" _* t:type_info() _* ">" _+ ls:list(Some(&t)) {
            Constant::ProtoList(t, ls)
          }

        rule constant_pair() -> Constant
          = "pair" _* "<" _* l:type_info() _* "," r:type_info() _* ">" _+ p:pair(Some((&l, &r))) {
            Constant::ProtoPair(l, r, Box::new(p.0), Box::new(p.1))
          }

        rule pair(type_info: Option<(&Type, &Type)>) -> (Constant, Constant)
          = "[" _* x:typed_constant(type_info.map(|t| t.0)) _* "," _* y:typed_constant(type_info.map(|t| t.1)) _* "]" { (x, y) }

        rule number() -> isize
          = n:$("-"* ['0'..='9']+) {? n.parse().or(Err("isize")) }

        rule big_number() -> i128
          = n:$("-"* ['0'..='9']+) {? n.parse().or(Err("i128")) }

        rule boolean() -> bool
          = b:$("True" / "False") { b == "True" }

        rule bytestring() -> Vec<u8>
          = "#" i:ident()* { hex::decode(String::from_iter(i)).unwrap() }

        rule string() -> String
          = "\"" s:[^ '"']* "\"" { String::from_iter(s) }

        rule data() -> PlutusData
          = "#" i:ident()* {
              PlutusData::decode_fragment(
                  hex::decode(String::from_iter(i)).unwrap().as_slice()
              ).unwrap()
            }

        rule list(type_info: Option<&Type>) -> Vec<Constant>
          = "[" _* xs:(typed_constant(type_info) ** (_* "," _*)) _* "]" { xs }

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
          / ls:list(list_sub_type(type_info)) {?
              match type_info {
                Some(Type::List(t)) => Ok(Constant::ProtoList(*t.clone(), ls)),
                _ => Err("found 'List' instead of expected type")
              }
            }
          / p:pair(pair_sub_type(type_info)) {?
              match type_info {
                Some(Type::Pair(l, r)) => Ok(Constant::ProtoPair(*l.clone(), *r.clone(), Box::new(p.0), Box::new(p.1))),
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
          / _* "list" _* "<" _* t:type_info() _* ">" {
              Type::List(Box::new(t))
            }
          / _* "pair" _* "<" l:type_info() "," r:type_info() ">" {
              Type::Pair(Box::new(l), Box::new(r))
            }

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
                        parameter_name: x.clone(),
                        body: Rc::new(Term::Var(x)),
                    }),
                    argument: Rc::new(Term::Constant(Constant::Integer(0)))
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
                    parameter_name: x.clone(),
                    body: Rc::new(Term::Var(x)),
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
                    parameter_name: x.clone(),
                    body: Rc::new(Term::Delay(Rc::new(Term::Var(x)))),
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
                term: Term::Constant(Constant::Integer(11)),
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
                        argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00, 0xFF]))),
                    }),
                    argument: Rc::new(Term::Constant(Constant::ByteString(vec![0xFF, 0x00])))
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
                        argument: Rc::new(Term::Constant(Constant::Integer(256))),
                    }),
                    argument: Rc::new(Term::Constant(Constant::ByteString(vec![])))
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
                            argument: Rc::new(Term::Constant(Constant::Integer(1))),
                        }),
                        argument: Rc::new(Term::Constant(Constant::Integer(2))),
                    }),
                    argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00, 0xFF, 0xAA])))
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
                    argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00, 0xFF, 0xAA])))
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
                        argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00])))
                    }),
                    argument: Rc::new(Term::Constant(Constant::Integer(9223372036854775808))),
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
                        argument: Rc::new(Term::Constant(Constant::ByteString(vec![
                            0x00, 0xff, 0xaa
                        ])))
                    }),
                    argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00, 0xff, 0xaa]))),
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
                        argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00, 0xff])))
                    }),
                    argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00, 0xff, 0xaa]))),
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
                        argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00, 0xff])))
                    }),
                    argument: Rc::new(Term::Constant(Constant::ByteString(vec![0x00]))),
                }
            }
        )
    }

    #[test]
    fn parse_list_empty() {
        let uplc = "(program 0.0.0 (con list<unit> []))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoList(Type::Unit, vec![]))
            }
        )
    }

    #[test]
    fn parse_list_singleton_unit() {
        let uplc = "(program 0.0.0 (con list<unit> [ () ]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoList(Type::Unit, vec![Constant::Unit]))
            }
        )
    }

    #[test]
    fn parse_list_bools() {
        let uplc = "(program 0.0.0 (con list<bool> [True, False, True]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoList(
                    Type::Bool,
                    vec![
                        Constant::Bool(true),
                        Constant::Bool(false),
                        Constant::Bool(true)
                    ]
                ))
            }
        )
    }

    #[test]
    fn parse_list_bytestrings() {
        let uplc = "(program 0.0.0 (con list<bytestring> [#00, #01]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoList(
                    Type::ByteString,
                    vec![
                        Constant::ByteString(vec![0x00]),
                        Constant::ByteString(vec![0x01]),
                    ]
                ))
            }
        )
    }

    #[test]
    fn parse_list_list_integers() {
        let uplc = "(program 0.0.0 (con list<list<integer>> [[14,42], [1337]]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoList(
                    Type::List(Box::new(Type::Integer)),
                    vec![
                        Constant::ProtoList(
                            Type::Integer,
                            vec![Constant::Integer(14), Constant::Integer(42)]
                        ),
                        Constant::ProtoList(Type::Integer, vec![Constant::Integer(1337)])
                    ]
                ))
            }
        )
    }

    #[test]
    fn parse_list_multiline() {
        let uplc = r#"
        (program 0.0.0
            (con list
              <integer>
              [ 14
              , 42
              ]
            )
        )"#;
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoList(
                    Type::Integer,
                    vec![Constant::Integer(14), Constant::Integer(42)],
                ))
            }
        )
    }

    #[test]
    fn parse_pair_unit_unit() {
        let uplc = "(program 0.0.0 (con pair <unit,unit> [(),()]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoPair(
                    Type::Unit,
                    Type::Unit,
                    Box::new(Constant::Unit),
                    Box::new(Constant::Unit)
                ))
            }
        )
    }

    #[test]
    fn parse_pair_bool_pair_integer_bytestring() {
        let uplc = "(program 0.0.0 (con pair<bool, pair<integer, bytestring>> [True, [14, #42]]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoPair(
                    Type::Bool,
                    Type::Pair(Box::new(Type::Integer), Box::new(Type::ByteString)),
                    Box::new(Constant::Bool(true)),
                    Box::new(Constant::ProtoPair(
                        Type::Integer,
                        Type::ByteString,
                        Box::new(Constant::Integer(14)),
                        Box::new(Constant::ByteString(vec![0x42])),
                    ))
                ))
            }
        )
    }

    #[test]
    fn parse_pair_string_list_integer() {
        let uplc = "(program 0.0.0 (con pair<string, list<integer>> [\"foo\", [14, 42]]))";
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoPair(
                    Type::String,
                    Type::List(Box::new(Type::Integer)),
                    Box::new(Constant::String(String::from("foo"))),
                    Box::new(Constant::ProtoList(
                        Type::Integer,
                        vec![Constant::Integer(14), Constant::Integer(42)],
                    ))
                ))
            }
        )
    }

    #[test]
    fn parse_pair_multiline() {
        let uplc = r#"
        (program 0.0.0
            (con pair
              <integer, integer>
              [14, 42]
            )
        )"#;
        assert_eq!(
            super::program(uplc).unwrap(),
            Program::<Name> {
                version: (0, 0, 0),
                term: Term::Constant(Constant::ProtoPair(
                    Type::Integer,
                    Type::Integer,
                    Box::new(Constant::Integer(14)),
                    Box::new(Constant::Integer(42))
                ))
            }
        )
    }

    #[test]
    fn parse_list_type_mismatch() {
        let uplc = "(program 0.0.0 (con list<integer> [True, False]))";
        assert!(super::program(uplc).is_err())
    }

    #[test]
    fn parse_list_mixed_types() {
        let uplc = "(program 0.0.0 (con list<integer> [14, False]))";
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
                        argument: Rc::new(Term::Constant(Constant::Integer(x))),
                    }),
                    argument: Rc::new(Term::Constant(Constant::Integer(y)))
                }
            }
        )
    }
}
