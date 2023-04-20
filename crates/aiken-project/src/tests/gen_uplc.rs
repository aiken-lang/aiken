use pretty_assertions::assert_eq;

use aiken_lang::ast::{Definition, Function};
use uplc::{
    ast::{Constant, DeBruijn, Name, Program, Term, Type},
    machine::cost_model::ExBudget,
    optimize, BigInt, Constr, PlutusData,
};

use crate::module::CheckedModules;

use super::TestProject;

fn assert_uplc(source_code: &str, expected: Term<Name>) {
    let mut project = TestProject::new();

    let modules = CheckedModules::singleton(project.check(project.parse(source_code)));
    let mut generator = modules.new_generator(
        &project.functions,
        &project.data_types,
        &project.module_types,
    );

    let Some(checked_module) = modules.values().next()
    else {
      unreachable!("There's got to be one right?")
    };

    let mut scripts = vec![];

    for def in checked_module.ast.definitions() {
        if let Definition::Test(func) = def {
            scripts.push((
                checked_module.input_path.clone(),
                checked_module.name.clone(),
                func,
            ));
        }
    }

    assert_eq!(scripts.len(), 1);

    let script = &scripts[0];

    let Function { body, .. } = script.2;

    let program = generator.generate_test(body);

    let debruijn_program: Program<DeBruijn> = program.try_into().unwrap();

    let expected = Program {
        version: (1, 0, 0),
        term: expected,
    };

    let expected = optimize::aiken_optimize_and_intern(expected);

    let expected: Program<DeBruijn> = expected.try_into().unwrap();

    assert_eq!(debruijn_program.to_pretty(), expected.to_pretty());

    let eval = debruijn_program.eval(ExBudget::default());

    assert!(!eval.failed())
}

#[test]
fn acceptance_test_1_length() {
    let src = r#"
        pub fn length(xs: List<a>) -> Int {
          when xs is {
            [] ->
              0
            [_, ..rest] ->
              1 + length(rest)
          }
        }
            
        test length_1() {
          length([1, 2, 3]) == 3
        }
    "#;

    assert_uplc(
        src,
        Term::equals_integer()
            .apply(
                Term::var("length")
                    .lambda("length")
                    .apply(Term::var("length").apply(Term::var("length")))
                    .lambda("length")
                    .apply(
                        Term::var("xs")
                            .delayed_choose_list(
                                Term::integer(0.into()),
                                Term::add_integer()
                                    .apply(Term::integer(1.into()))
                                    .apply(
                                        Term::var("length")
                                            .apply(Term::var("length"))
                                            .apply(Term::var("rest")),
                                    )
                                    .lambda("rest")
                                    .apply(Term::tail_list().apply(Term::var("xs"))),
                            )
                            .lambda("xs")
                            .lambda("length"),
                    )
                    .apply(Term::list_values(vec![
                        Constant::Data(PlutusData::BigInt(BigInt::Int(1.into()))),
                        Constant::Data(PlutusData::BigInt(BigInt::Int(2.into()))),
                        Constant::Data(PlutusData::BigInt(BigInt::Int(3.into()))),
                    ])),
            )
            .apply(Term::integer(3.into())),
    );
}

#[test]
fn acceptance_test_2_repeat() {
    let src = r#"
        pub fn repeat(x: a, n: Int) -> List<a> {
          if n <= 0 {
            []
          } else {
            [x, ..repeat(x, n - 1)]
          }
        }
          
        test repeat_1() {
          repeat("aiken", 2) == ["aiken", "aiken"]
        }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("repeat")
                        .lambda("repeat")
                        .apply(Term::var("repeat").apply(Term::var("repeat")))
                        .lambda("repeat")
                        .apply(
                            Term::less_than_equals_integer()
                                .apply(Term::var("n"))
                                .apply(Term::integer(0.into()))
                                .delayed_if_else(
                                    Term::empty_list(),
                                    Term::mk_cons()
                                        .apply(Term::b_data().apply(Term::var("x")))
                                        .apply(
                                            Term::var("repeat")
                                                .apply(Term::var("repeat"))
                                                .apply(Term::var("x"))
                                                .apply(
                                                    Term::sub_integer()
                                                        .apply(Term::var("n"))
                                                        .apply(Term::integer(1.into())),
                                                ),
                                        ),
                                )
                                .lambda("n")
                                .lambda("x")
                                .lambda("repeat"),
                        )
                        .apply(Term::byte_string("aiken".as_bytes().to_vec()))
                        .apply(Term::integer(2.into())),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(PlutusData::BoundedBytes("aiken".as_bytes().to_vec().into())),
                Constant::Data(PlutusData::BoundedBytes("aiken".as_bytes().to_vec().into())),
            ]))),
    );
}

#[test]
fn acceptance_test_3_concat() {
    let src = r#"
        pub fn foldr(xs: List<a>, f: fn(a, b) -> b, zero: b) -> b {
          when xs is {
            [] ->
              zero
            [x, ..rest] ->
              f(x, foldr(rest, f, zero))
          }
        }
          
        pub fn concat(left: List<a>, right: List<a>) -> List<a> {
          foldr(left, fn(x, xs) { [x, ..xs] }, right)
        }
          
        test concat_1() {
          concat([1, 2, 3], [4, 5, 6]) == [1, 2, 3, 4, 5, 6]
        }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("concat")
                        .lambda("concat")
                        .apply(
                            Term::var("foldr")
                                .apply(Term::var("left"))
                                .apply(
                                    Term::mk_cons()
                                        .apply(Term::i_data().apply(Term::var("x")))
                                        .apply(Term::var("xs"))
                                        .lambda("xs")
                                        .lambda("x"),
                                )
                                .apply(Term::var("right"))
                                .lambda("right")
                                .lambda("left"),
                        )
                        .lambda("foldr")
                        .apply(Term::var("foldr").apply(Term::var("foldr")))
                        .lambda("foldr")
                        .apply(
                            Term::var("xs")
                                .delayed_choose_list(
                                    Term::var("zero"),
                                    Term::var("f")
                                        .apply(Term::var("x"))
                                        .apply(
                                            Term::var("foldr")
                                                .apply(Term::var("foldr"))
                                                .apply(Term::var("rest"))
                                                .apply(Term::var("f"))
                                                .apply(Term::var("zero")),
                                        )
                                        .lambda("rest")
                                        .apply(Term::tail_list().apply(Term::var("xs")))
                                        .lambda("x")
                                        .apply(
                                            Term::un_i_data()
                                                .apply(Term::head_list().apply(Term::var("xs"))),
                                        ),
                                )
                                .lambda("zero")
                                .lambda("f")
                                .lambda("xs")
                                .lambda("foldr"),
                        )
                        .apply(Term::list_values(vec![
                            Constant::Data(PlutusData::BigInt(BigInt::Int(1.into()))),
                            Constant::Data(PlutusData::BigInt(BigInt::Int(2.into()))),
                            Constant::Data(PlutusData::BigInt(BigInt::Int(3.into()))),
                        ]))
                        .apply(Term::list_values(vec![
                            Constant::Data(PlutusData::BigInt(BigInt::Int(4.into()))),
                            Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))),
                            Constant::Data(PlutusData::BigInt(BigInt::Int(6.into()))),
                        ])),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(PlutusData::BigInt(BigInt::Int(1.into()))),
                Constant::Data(PlutusData::BigInt(BigInt::Int(2.into()))),
                Constant::Data(PlutusData::BigInt(BigInt::Int(3.into()))),
                Constant::Data(PlutusData::BigInt(BigInt::Int(4.into()))),
                Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))),
                Constant::Data(PlutusData::BigInt(BigInt::Int(6.into()))),
            ]))),
    );
}

#[test]
fn acceptance_test_4_concat_no_anon_func() {
    let src = r#"
        pub fn foldr(xs: List<a>, f: fn(a, b) -> b, zero: b) -> b {
          when xs is {
            [] ->
              zero
            [x, ..rest] ->
              f(x, foldr(rest, f, zero))
          }
        }
        
        pub fn prepend(x: a, xs: List<a>) -> List<a> {
          [x, ..xs]
        }
        
        pub fn concat(left: List<a>, right: List<a>) -> List<a> {
          foldr(left, prepend, right)
        }
        
        test concat_1() {
          concat([1, 2, 3], [4, 5, 6]) == [1, 2, 3, 4, 5, 6]
        }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("concat")
                        .lambda("concat")
                        .apply(
                            Term::var("foldr")
                                .apply(Term::var("left"))
                                .apply(Term::var("prepend"))
                                .apply(Term::var("right"))
                                .lambda("right")
                                .lambda("left"),
                        )
                        .lambda("prepend")
                        .apply(
                            Term::mk_cons()
                                .apply(Term::i_data().apply(Term::var("x")))
                                .apply(Term::var("xs"))
                                .lambda("xs")
                                .lambda("x"),
                        )
                        .lambda("foldr")
                        .apply(Term::var("foldr").apply(Term::var("foldr")))
                        .lambda("foldr")
                        .apply(
                            Term::var("xs")
                                .delayed_choose_list(
                                    Term::var("zero"),
                                    Term::var("f")
                                        .apply(Term::var("x"))
                                        .apply(
                                            Term::var("foldr")
                                                .apply(Term::var("foldr"))
                                                .apply(Term::var("rest"))
                                                .apply(Term::var("f"))
                                                .apply(Term::var("zero")),
                                        )
                                        .lambda("rest")
                                        .apply(Term::tail_list().apply(Term::var("xs")))
                                        .lambda("x")
                                        .apply(
                                            Term::un_i_data()
                                                .apply(Term::head_list().apply(Term::var("xs"))),
                                        ),
                                )
                                .lambda("zero")
                                .lambda("f")
                                .lambda("xs")
                                .lambda("foldr"),
                        )
                        .apply(Term::list_values(vec![
                            Constant::Data(PlutusData::BigInt(BigInt::Int(1.into()))),
                            Constant::Data(PlutusData::BigInt(BigInt::Int(2.into()))),
                            Constant::Data(PlutusData::BigInt(BigInt::Int(3.into()))),
                        ]))
                        .apply(Term::list_values(vec![
                            Constant::Data(PlutusData::BigInt(BigInt::Int(4.into()))),
                            Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))),
                            Constant::Data(PlutusData::BigInt(BigInt::Int(6.into()))),
                        ])),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(PlutusData::BigInt(BigInt::Int(1.into()))),
                Constant::Data(PlutusData::BigInt(BigInt::Int(2.into()))),
                Constant::Data(PlutusData::BigInt(BigInt::Int(3.into()))),
                Constant::Data(PlutusData::BigInt(BigInt::Int(4.into()))),
                Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))),
                Constant::Data(PlutusData::BigInt(BigInt::Int(6.into()))),
            ]))),
    );
}

#[test]
fn acceptance_test_5_head_not_empty() {
    let src = r#"
        use aiken/builtin.{head_list}

        pub fn head(xs: List<a>) -> Option<a> {
          when xs is {
            [] -> None
            _ -> Some(head_list(xs))
          }
        }
        
        test head_1() {
          head([1, 2, 3]) == Some(1)
        }     
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::var("head")
                    .lambda("head")
                    .apply(
                        Term::var("xs")
                            .delayed_choose_list(
                                Term::Constant(
                                    Constant::Data(PlutusData::Constr(Constr {
                                        tag: 122,
                                        any_constructor: None,
                                        fields: vec![],
                                    }))
                                    .into(),
                                ),
                                Term::constr_data().apply(Term::integer(0.into())).apply(
                                    Term::mk_cons()
                                        .apply(Term::head_list().apply(Term::var("xs")))
                                        .apply(Term::empty_list()),
                                ),
                            )
                            .lambda("xs"),
                    )
                    .apply(Term::list_values(vec![
                        Constant::Data(PlutusData::BigInt(BigInt::Int(1.into()))),
                        Constant::Data(PlutusData::BigInt(BigInt::Int(2.into()))),
                        Constant::Data(PlutusData::BigInt(BigInt::Int(3.into()))),
                    ])),
            )
            .apply(Term::Constant(
                Constant::Data(PlutusData::Constr(Constr {
                    tag: 121,
                    any_constructor: None,
                    fields: vec![PlutusData::BigInt(BigInt::Int(1.into()))],
                }))
                .into(),
            )),
    );
}

#[test]
fn acceptance_test_5_head_empty() {
    let src = r#"
        use aiken/builtin.{head_list}

        pub fn head(xs: List<a>) -> Option<a> {
          when xs is {
            [] -> None
            _ -> Some(head_list(xs))
          }
        }
        
        test head_1() {
          head([]) == None
        }     
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::var("head")
                    .lambda("head")
                    .apply(
                        Term::var("xs")
                            .delayed_choose_list(
                                Term::Constant(
                                    Constant::Data(PlutusData::Constr(Constr {
                                        tag: 122,
                                        any_constructor: None,
                                        fields: vec![],
                                    }))
                                    .into(),
                                ),
                                Term::constr_data().apply(Term::integer(0.into())).apply(
                                    Term::mk_cons()
                                        .apply(Term::head_list().apply(Term::var("xs")))
                                        .apply(Term::empty_list()),
                                ),
                            )
                            .lambda("xs"),
                    )
                    .apply(Term::list_values(vec![])),
            )
            .apply(Term::Constant(
                Constant::Data(PlutusData::Constr(Constr {
                    tag: 122,
                    any_constructor: None,
                    fields: vec![],
                }))
                .into(),
            )),
    );
}

#[test]
fn acceptance_test_6_if_else() {
    let src = r#"
        test bar() {
          let x = 1
          if x == 1 {
            True
          } else {
            False
          }
        }
    "#;

    assert_uplc(
        src,
        Term::equals_integer()
            .apply(Term::integer(1.into()))
            .apply(Term::integer(1.into()))
            .delayed_if_else(Term::bool(true), Term::bool(false)),
    );
}

#[test]
fn acceptance_test_6_equals() {
    let src = r#"
        test foo() {
          (1, []) == (1, [])
        }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::map_data().apply(
                    Term::mk_cons()
                        .apply(Term::Constant(
                            Constant::ProtoPair(
                                Type::Data,
                                Type::Data,
                                Constant::Data(PlutusData::BigInt(BigInt::Int(1.into()))).into(),
                                Constant::Data(PlutusData::Array(vec![])).into(),
                            )
                            .into(),
                        ))
                        .apply(Term::empty_map()),
                ),
            )
            .apply(
                Term::map_data().apply(
                    Term::mk_cons()
                        .apply(Term::Constant(
                            Constant::ProtoPair(
                                Type::Data,
                                Type::Data,
                                Constant::Data(PlutusData::BigInt(BigInt::Int(1.into()))).into(),
                                Constant::Data(PlutusData::Array(vec![])).into(),
                            )
                            .into(),
                        ))
                        .apply(Term::empty_map()),
                ),
            ),
    );
}

#[test]
fn acceptance_test_7_unzip() {
    let src = r#"
      pub fn unzip(xs: List<(a, b)>) -> (List<a>, List<b>) {
        when xs is {
          [] -> ([], [])
          [(a, b), ..rest] -> {
            let (a_tail, b_tail) = unzip(rest)
            ([a, ..a_tail], [b, ..b_tail])
          }
        }
      }
      
      test unzip1() {
        let x = [(3, #"55"), (4, #"7799")]
      
        unzip(x) == ([3, 4], [#"55", #"7799"])
      }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::map_data().apply(
                    Term::mk_cons()
                        .apply(
                            Term::var("unzip")
                                .lambda("unzip")
                                .apply(Term::var("unzip").apply(Term::var("unzip")))
                                .lambda("unzip")
                                .apply(
                                    Term::var("xs")
                                        .delayed_choose_list(
                                            Term::Constant(
                                                Constant::ProtoPair(
                                                    Type::Data,
                                                    Type::Data,
                                                    Constant::Data(PlutusData::Array(vec![]))
                                                        .into(),
                                                    Constant::Data(PlutusData::Array(vec![]))
                                                        .into(),
                                                )
                                                .into(),
                                            ),
                                            Term::mk_pair_data()
                                                .apply(
                                                    Term::list_data().apply(
                                                        Term::mk_cons()
                                                            .apply(
                                                                Term::i_data()
                                                                    .apply(Term::var("a")),
                                                            )
                                                            .apply(Term::var("a_tail")),
                                                    ),
                                                )
                                                .apply(
                                                    Term::list_data().apply(
                                                        Term::mk_cons()
                                                            .apply(
                                                                Term::b_data()
                                                                    .apply(Term::var("b")),
                                                            )
                                                            .apply(Term::var("b_tail")),
                                                    ),
                                                )
                                                .lambda("b_tail")
                                                .apply(Term::unlist_data().apply(
                                                    Term::snd_pair().apply(Term::var("tail_pair")),
                                                ))
                                                .lambda("a_tail")
                                                .apply(Term::unlist_data().apply(
                                                    Term::fst_pair().apply(Term::var("tail_pair")),
                                                ))
                                                .lambda("tail_pair")
                                                .apply(
                                                    Term::var("unzip")
                                                        .apply(Term::var("unzip"))
                                                        .apply(Term::var("rest")),
                                                )
                                                .lambda("a")
                                                .apply(Term::un_i_data().apply(
                                                    Term::fst_pair().apply(Term::var("head_pair")),
                                                ))
                                                .lambda("b")
                                                .apply(Term::un_b_data().apply(
                                                    Term::snd_pair().apply(Term::var("head_pair")),
                                                ))
                                                .lambda("rest")
                                                .apply(Term::tail_list().apply(Term::var("xs")))
                                                .lambda("head_pair")
                                                .apply(Term::head_list().apply(Term::var("xs"))),
                                        )
                                        .lambda("xs")
                                        .lambda("unzip"),
                                )
                                .apply(Term::var("x")),
                        )
                        .apply(Term::empty_map()),
                ),
            )
            .apply(
                Term::map_data().apply(
                    Term::mk_cons()
                        .apply(Term::Constant(
                            Constant::ProtoPair(
                                Type::Data,
                                Type::Data,
                                Constant::Data(PlutusData::Array(vec![
                                    PlutusData::BigInt(BigInt::Int(3.into())),
                                    PlutusData::BigInt(BigInt::Int(4.into())),
                                ]))
                                .into(),
                                Constant::Data(PlutusData::Array(vec![
                                    PlutusData::BoundedBytes(vec![85].into()),
                                    PlutusData::BoundedBytes(vec![119, 153].into()),
                                ]))
                                .into(),
                            )
                            .into(),
                        ))
                        .apply(Term::empty_map()),
                ),
            )
            .lambda("x")
            .apply(Term::Constant(
                Constant::ProtoList(
                    Type::Pair(Type::Data.into(), Type::Data.into()),
                    vec![
                        Constant::ProtoPair(
                            Type::Data,
                            Type::Data,
                            Constant::Data(PlutusData::BigInt(BigInt::Int(3.into()))).into(),
                            Constant::Data(PlutusData::BoundedBytes(vec![85].into())).into(),
                        ),
                        Constant::ProtoPair(
                            Type::Data,
                            Type::Data,
                            Constant::Data(PlutusData::BigInt(BigInt::Int(4.into()))).into(),
                            Constant::Data(PlutusData::BoundedBytes(vec![119, 153].into())).into(),
                        ),
                    ],
                )
                .into(),
            )),
    );
}

#[test]
fn acceptance_test_8_is_empty() {
    let src = r#"
      use aiken/builtin

      pub fn is_empty(bytes: ByteArray) -> Bool {
        builtin.length_of_bytearray(bytes) == 0
      }
    
      test is_empty_1() {
        is_empty(#"") == True
      }
    "#;

    assert_uplc(
        src,
        Term::var("is_empty")
            .lambda("is_empty")
            .apply(
                Term::equals_integer()
                    .apply(Term::length_of_bytearray().apply(Term::var("bytes")))
                    .apply(Term::integer(0.into()))
                    .lambda("bytes"),
            )
            .apply(Term::byte_string(vec![]))
            .delayed_if_else(
                Term::bool(true),
                Term::bool(true).if_else(Term::bool(false), Term::bool(true)),
            ),
    );
}
