use std::sync::Arc;

use pretty_assertions::assert_eq;

use aiken_lang::{
    ast::{Definition, Function, Validator},
    expr::TypedExpr,
    tipo::Type as AikenType,
};
use uplc::{
    ast::{Constant, Data, DeBruijn, Name, Program, Term, Type},
    builder::CONSTR_GET_FIELD,
    machine::cost_model::ExBudget,
    optimize,
};

use crate::module::CheckedModules;

use super::TestProject;

enum TestType {
    Func(Function<Arc<AikenType>, TypedExpr>),
    Validator(Validator<Arc<AikenType>, TypedExpr>),
}

fn assert_uplc(source_code: &str, expected: Term<Name>, should_fail: bool) {
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
                TestType::Func(func.clone()),
            ));
        } else if let Definition::Validator(func) = def {
            scripts.push((
                checked_module.input_path.clone(),
                checked_module.name.clone(),
                TestType::Validator(func.clone()),
            ));
        }
    }

    assert_eq!(scripts.len(), 1);

    let script = &scripts[0];

    match &script.2 {
        TestType::Func(Function { body: func, .. }) => {
            let program = generator.generate_test(func);

            let debruijn_program: Program<DeBruijn> = program.try_into().unwrap();

            let expected = Program {
                version: (1, 0, 0),
                term: expected,
            };

            let expected = optimize::aiken_optimize_and_intern(expected);

            let expected: Program<DeBruijn> = expected.try_into().unwrap();

            assert_eq!(debruijn_program.to_pretty(), expected.to_pretty());

            let mut eval = debruijn_program.eval(ExBudget::default());

            assert_eq!(
                eval.failed(),
                should_fail,
                "logs - {}\n",
                format!("{:#?}", eval.logs())
            );

            if !should_fail {
                assert_eq!(eval.result().unwrap(), Term::bool(true));
            }
        }
        TestType::Validator(func) => {
            let program = generator.generate(func);

            let debruijn_program: Program<DeBruijn> = program.try_into().unwrap();

            let expected = Program {
                version: (1, 0, 0),
                term: expected,
            };

            let expected = optimize::aiken_optimize_and_intern(expected);

            let expected: Program<DeBruijn> = expected.try_into().unwrap();

            assert_eq!(debruijn_program.to_pretty(), expected.to_pretty());
        }
    }
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
                        Constant::Data(Data::integer(1.into())),
                        Constant::Data(Data::integer(2.into())),
                        Constant::Data(Data::integer(3.into())),
                    ])),
            )
            .apply(Term::integer(3.into())),
        false,
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
                Constant::Data(Data::bytestring("aiken".as_bytes().to_vec())),
                Constant::Data(Data::bytestring("aiken".as_bytes().to_vec())),
            ]))),
        false,
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
                            Constant::Data(Data::integer(1.into())),
                            Constant::Data(Data::integer(2.into())),
                            Constant::Data(Data::integer(3.into())),
                        ]))
                        .apply(Term::list_values(vec![
                            Constant::Data(Data::integer(4.into())),
                            Constant::Data(Data::integer(5.into())),
                            Constant::Data(Data::integer(6.into())),
                        ])),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(3.into())),
                Constant::Data(Data::integer(4.into())),
                Constant::Data(Data::integer(5.into())),
                Constant::Data(Data::integer(6.into())),
            ]))),
        false,
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
                            Constant::Data(Data::integer(1.into())),
                            Constant::Data(Data::integer(2.into())),
                            Constant::Data(Data::integer(3.into())),
                        ]))
                        .apply(Term::list_values(vec![
                            Constant::Data(Data::integer(4.into())),
                            Constant::Data(Data::integer(5.into())),
                            Constant::Data(Data::integer(6.into())),
                        ])),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(3.into())),
                Constant::Data(Data::integer(4.into())),
                Constant::Data(Data::integer(5.into())),
                Constant::Data(Data::integer(6.into())),
            ]))),
        false,
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
                                Term::Constant(Constant::Data(Data::constr(1, vec![])).into()),
                                Term::constr_data().apply(Term::integer(0.into())).apply(
                                    Term::mk_cons()
                                        .apply(Term::head_list().apply(Term::var("xs")))
                                        .apply(Term::empty_list()),
                                ),
                            )
                            .lambda("xs"),
                    )
                    .apply(Term::list_values(vec![
                        Constant::Data(Data::integer(1.into())),
                        Constant::Data(Data::integer(2.into())),
                        Constant::Data(Data::integer(3.into())),
                    ])),
            )
            .apply(Term::Constant(
                Constant::Data(Data::constr(0, vec![Data::integer(1.into())])).into(),
            )),
        false,
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
                                Term::Constant(Constant::Data(Data::constr(1, vec![])).into()),
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
                Constant::Data(Data::constr(1, vec![])).into(),
            )),
        false,
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
        false,
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
                                Constant::Data(Data::integer(1.into())).into(),
                                Constant::Data(Data::list(vec![])).into(),
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
                                Constant::Data(Data::integer(1.into())).into(),
                                Constant::Data(Data::list(vec![])).into(),
                            )
                            .into(),
                        ))
                        .apply(Term::empty_map()),
                ),
            ),
        false,
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
                                            Term::pair_values(
                                                Constant::Data(Data::list(vec![])),
                                                Constant::Data(Data::list(vec![])),
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
                        .apply(Term::pair_values(
                            Constant::Data(Data::list(vec![
                                Data::integer(3.into()),
                                Data::integer(4.into()),
                            ])),
                            Constant::Data(Data::list(vec![
                                Data::bytestring(vec![85]),
                                Data::bytestring(vec![119, 153]),
                            ])),
                        ))
                        .apply(Term::empty_map()),
                ),
            )
            .lambda("x")
            .apply(Term::map_values(vec![
                Constant::ProtoPair(
                    Type::Data,
                    Type::Data,
                    Constant::Data(Data::integer(3.into())).into(),
                    Constant::Data(Data::bytestring(vec![85])).into(),
                ),
                Constant::ProtoPair(
                    Type::Data,
                    Type::Data,
                    Constant::Data(Data::integer(4.into())).into(),
                    Constant::Data(Data::bytestring(vec![119, 153])).into(),
                ),
            ])),
        false,
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
        false,
    );
}

#[test]
fn acceptance_test_8_is_not_empty() {
    let src = r#"
      use aiken/builtin

      pub fn is_empty(bytes: ByteArray) -> Bool {
        builtin.length_of_bytearray(bytes) == 0
      }
    
      test is_empty_1() {
        is_empty(#"01") == False
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
            .apply(Term::byte_string(vec![1]))
            .delayed_if_else(
                Term::bool(false),
                Term::bool(false).if_else(Term::bool(false), Term::bool(true)),
            ),
        false,
    );
}

#[test]
fn acceptance_test_9_is_empty() {
    let src = r#"
      use aiken/builtin.{length_of_bytearray}

      pub fn is_empty(bytes: ByteArray) -> Bool {
        length_of_bytearray(bytes) == 0
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
        false,
    );
}

#[test]
fn acceptance_test_10_map_none() {
    let src = r#"
      pub fn map(opt: Option<a>, f: fn(a) -> b) -> Option<b> {
        when opt is {
          None ->
            None
          Some(a) ->
            Some(f(a))
        }
      }
      
      fn add_one(n: Int) -> Int {
        n + 1
      }
      
      test map_1() {
        map(None, add_one) == None
      }      
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::var("map")
                    .lambda("map")
                    .apply(
                        Term::equals_integer()
                            .apply(Term::integer(1.into()))
                            .apply(Term::var("constr_index"))
                            .delayed_if_else(
                                Term::Constant(Constant::Data(Data::constr(1, vec![])).into()),
                                Term::constr_data()
                                    .apply(Term::integer(0.into()))
                                    .apply(
                                        Term::mk_cons()
                                            .apply(
                                                Term::i_data()
                                                    .apply(Term::var("f").apply(Term::var("a"))),
                                            )
                                            .apply(Term::empty_list()),
                                    )
                                    .lambda("a")
                                    .apply(
                                        Term::un_i_data().apply(
                                            Term::head_list().apply(Term::var("constr_fields")),
                                        ),
                                    )
                                    .lambda("constr_fields")
                                    .apply(
                                        Term::snd_pair()
                                            .apply(Term::unconstr_data().apply(Term::var("opt"))),
                                    ),
                            )
                            .lambda("constr_index")
                            .apply(
                                Term::fst_pair()
                                    .apply(Term::unconstr_data().apply(Term::var("opt"))),
                            )
                            .lambda("f")
                            .lambda("opt"),
                    )
                    .apply(Term::Constant(
                        Constant::Data(Data::constr(1, vec![])).into(),
                    ))
                    .apply(
                        Term::var("add_one").lambda("add_one").apply(
                            Term::add_integer()
                                .apply(Term::var("n"))
                                .apply(Term::integer(1.into()))
                                .lambda("n"),
                        ),
                    ),
            )
            .apply(Term::Constant(
                Constant::Data(Data::constr(1, vec![])).into(),
            ))
            .constr_get_field(),
        false,
    );
}

#[test]
fn acceptance_test_10_map_some() {
    let src = r#"
      pub fn map(opt: Option<a>, f: fn(a) -> b) -> Option<b> {
        when opt is {
          None ->
            None
          Some(a) ->
            Some(f(a))
        }
      }
      
      fn add_one(n: Int) -> Int {
        n + 1
      }
      
      test map_1() {
        map(Some(1), add_one) == Some(2)
      }      
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::var("map")
                    .lambda("map")
                    .apply(
                        Term::equals_integer()
                            .apply(Term::integer(1.into()))
                            .apply(Term::var("constr_index"))
                            .delayed_if_else(
                                Term::Constant(Constant::Data(Data::constr(1, vec![])).into()),
                                Term::constr_data()
                                    .apply(Term::integer(0.into()))
                                    .apply(
                                        Term::mk_cons()
                                            .apply(
                                                Term::i_data()
                                                    .apply(Term::var("f").apply(Term::var("a"))),
                                            )
                                            .apply(Term::empty_list()),
                                    )
                                    .lambda("a")
                                    .apply(
                                        Term::un_i_data().apply(
                                            Term::head_list().apply(Term::var("constr_fields")),
                                        ),
                                    )
                                    .lambda("constr_fields")
                                    .apply(
                                        Term::snd_pair()
                                            .apply(Term::unconstr_data().apply(Term::var("opt"))),
                                    ),
                            )
                            .lambda("constr_index")
                            .apply(
                                Term::fst_pair()
                                    .apply(Term::unconstr_data().apply(Term::var("opt"))),
                            )
                            .lambda("f")
                            .lambda("opt"),
                    )
                    .apply(Term::Constant(
                        Constant::Data(Data::constr(0, vec![Data::integer(1.into())])).into(),
                    ))
                    .apply(
                        Term::var("add_one").lambda("add_one").apply(
                            Term::add_integer()
                                .apply(Term::var("n"))
                                .apply(Term::integer(1.into()))
                                .lambda("n"),
                        ),
                    ),
            )
            .apply(Term::Constant(
                Constant::Data(Data::constr(0, vec![Data::integer(2.into())])).into(),
            ))
            .constr_get_field(),
        false,
    );
}

#[test]
fn expect_empty_list_on_filled_list() {
    let src = r#"  
      test empty_list1() {
        let x = [1,2]
        expect [] = x

        True
      }      
    "#;

    assert_uplc(
        src,
        Term::var("x")
            .delayed_choose_list(
                Term::bool(true),
                Term::Error.trace(Term::string("Expected no items for List")),
            )
            .lambda("x")
            .apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
            ])),
        true,
    );
}

#[test]
fn expect_empty_list_on_new_list() {
    let src = r#"  
      test empty_list1() {
        let x = []
        expect [] = x

        True
      }      
    "#;

    assert_uplc(
        src,
        Term::var("x")
            .delayed_choose_list(
                Term::bool(true),
                Term::Error.trace(Term::string("Expected no items for List")),
            )
            .lambda("x")
            .apply(Term::list_values(vec![])),
        false,
    );
}

#[test]
fn when_bool_is_true() {
    let src = r#"  
      test it() {
        when True is {
          True ->
            True
          False ->
            error
        }
      }    
    "#;

    assert_uplc(
        src,
        Term::var("subject")
            .delayed_if_else(Term::bool(true), Term::Error)
            .lambda("subject")
            .apply(Term::bool(true)),
        false,
    );
}

#[test]
fn when_bool_is_true_switched_cases() {
    let src = r#"  
      test it() {
        when True is {
          False ->
            error
          True ->
            True
        }
      } 
    "#;

    assert_uplc(
        src,
        Term::var("subject")
            .delayed_if_else(Term::bool(true), Term::Error)
            .lambda("subject")
            .apply(Term::bool(true)),
        false,
    );
}

#[test]
fn when_bool_is_false() {
    let src = r#"  
      test it() {
        when False is {
          False ->
            error
          True ->
            True
        }
      }  
    "#;

    assert_uplc(
        src,
        Term::var("subject")
            .delayed_if_else(Term::bool(true), Term::Error)
            .lambda("subject")
            .apply(Term::bool(false)),
        true,
    );
}

#[test]
fn acceptance_test_11_map_empty() {
    let src = r#"
      pub fn map(xs: List<a>, f: fn(a) -> result) -> List<result> {
        when xs is {
          [] ->
            []
          [x, ..rest] ->
            [f(x), ..map(rest, f)]
        }
      }
      
      test map_1() {
        map([], fn(n) { n + 1 }) == []
      }      
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("map")
                        .lambda("map")
                        .apply(Term::var("map").apply(Term::var("map")))
                        .lambda("map")
                        .apply(
                            Term::var("xs")
                                .delayed_choose_list(
                                    Term::empty_list(),
                                    Term::mk_cons()
                                        .apply(
                                            Term::i_data()
                                                .apply(Term::var("f").apply(Term::var("x"))),
                                        )
                                        .apply(
                                            Term::var("map")
                                                .apply(Term::var("map"))
                                                .apply(Term::var("rest"))
                                                .apply(Term::var("f")),
                                        )
                                        .lambda("rest")
                                        .apply(Term::tail_list().apply(Term::var("xs")))
                                        .lambda("x")
                                        .apply(
                                            Term::un_i_data()
                                                .apply(Term::head_list().apply(Term::var("xs"))),
                                        ),
                                )
                                .lambda("f")
                                .lambda("xs")
                                .lambda("map"),
                        )
                        .apply(Term::empty_list())
                        .apply(
                            Term::add_integer()
                                .apply(Term::var("n"))
                                .apply(Term::integer(1.into()))
                                .lambda("n"),
                        ),
                ),
            )
            .apply(Term::list_data().apply(Term::empty_list())),
        false,
    );
}

#[test]
fn acceptance_test_11_map_filled() {
    let src = r#"
      pub fn map(xs: List<a>, f: fn(a) -> result) -> List<result> {
        when xs is {
          [] ->
            []
          [x, ..rest] ->
            [f(x), ..map(rest, f)]
        }
      }
      
      test map_1() {
        map([6, 7, 8], fn(n) { n + 1 }) == [7, 8, 9]
      }      
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("map")
                        .lambda("map")
                        .apply(Term::var("map").apply(Term::var("map")))
                        .lambda("map")
                        .apply(
                            Term::var("xs")
                                .delayed_choose_list(
                                    Term::empty_list(),
                                    Term::mk_cons()
                                        .apply(
                                            Term::i_data()
                                                .apply(Term::var("f").apply(Term::var("x"))),
                                        )
                                        .apply(
                                            Term::var("map")
                                                .apply(Term::var("map"))
                                                .apply(Term::var("rest"))
                                                .apply(Term::var("f")),
                                        )
                                        .lambda("rest")
                                        .apply(Term::tail_list().apply(Term::var("xs")))
                                        .lambda("x")
                                        .apply(
                                            Term::un_i_data()
                                                .apply(Term::head_list().apply(Term::var("xs"))),
                                        ),
                                )
                                .lambda("f")
                                .lambda("xs")
                                .lambda("map"),
                        )
                        .apply(Term::list_values(vec![
                            Constant::Data(Data::integer(6.into())),
                            Constant::Data(Data::integer(7.into())),
                            Constant::Data(Data::integer(8.into())),
                        ]))
                        .apply(
                            Term::add_integer()
                                .apply(Term::var("n"))
                                .apply(Term::integer(1.into()))
                                .lambda("n"),
                        ),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(Data::integer(7.into())),
                Constant::Data(Data::integer(8.into())),
                Constant::Data(Data::integer(9.into())),
            ]))),
        false,
    );
}

#[test]
fn acceptance_test_12_filter_even() {
    let src = r#"
      use aiken/builtin

      pub fn filter(xs: List<a>, f: fn(a) -> Bool) -> List<a> {
        when xs is {
          [] ->
            []
          [x, ..rest] ->
            if f(x) {
              [x, ..filter(rest, f)]
            } else {
              filter(rest, f)
            }
        }
      }
    
      test filter_1() {
        filter([1, 2, 3, 4, 5, 6], fn(x) { builtin.mod_integer(x, 2) == 0 }) == [2, 4, 6]
      }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("filter")
                        .lambda("filter")
                        .apply(Term::var("filter").apply(Term::var("filter")))
                        .lambda("filter")
                        .apply(
                            Term::var("xs")
                                .delayed_choose_list(
                                    Term::empty_list(),
                                    Term::var("f")
                                        .apply(Term::var("x"))
                                        .delayed_if_else(
                                            Term::mk_cons()
                                                .apply(Term::i_data().apply(Term::var("x")))
                                                .apply(
                                                    Term::var("filter")
                                                        .apply(Term::var("filter"))
                                                        .apply(Term::var("rest"))
                                                        .apply(Term::var("f")),
                                                ),
                                            Term::var("filter")
                                                .apply(Term::var("filter"))
                                                .apply(Term::var("rest"))
                                                .apply(Term::var("f")),
                                        )
                                        .lambda("rest")
                                        .apply(Term::tail_list().apply(Term::var("xs")))
                                        .lambda("x")
                                        .apply(
                                            Term::un_i_data()
                                                .apply(Term::head_list().apply(Term::var("xs"))),
                                        ),
                                )
                                .lambda("f")
                                .lambda("xs")
                                .lambda("filter"),
                        )
                        .apply(Term::list_values(vec![
                            Constant::Data(Data::integer(1.into())),
                            Constant::Data(Data::integer(2.into())),
                            Constant::Data(Data::integer(3.into())),
                            Constant::Data(Data::integer(4.into())),
                            Constant::Data(Data::integer(5.into())),
                            Constant::Data(Data::integer(6.into())),
                        ]))
                        .apply(
                            Term::equals_integer()
                                .apply(
                                    Term::mod_integer()
                                        .apply(Term::var("x"))
                                        .apply(Term::integer(2.into())),
                                )
                                .apply(Term::integer(0.into()))
                                .lambda("x"),
                        ),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(4.into())),
                Constant::Data(Data::integer(6.into())),
            ]))),
        false,
    );
}

#[test]
fn acceptance_test_14_list_creation() {
    let src = r#"
      test foo() {
        [0 - 2, 0 - 1, 0] == [-2, -1, 0]
      }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::mk_cons()
                        .apply(
                            Term::i_data().apply(
                                Term::sub_integer()
                                    .apply(Term::integer(0.into()))
                                    .apply(Term::integer(2.into())),
                            ),
                        )
                        .apply(
                            Term::mk_cons()
                                .apply(
                                    Term::i_data().apply(
                                        Term::sub_integer()
                                            .apply(Term::integer(0.into()))
                                            .apply(Term::integer(1.into())),
                                    ),
                                )
                                .apply(
                                    Term::mk_cons()
                                        .apply(Term::i_data().apply(Term::integer(0.into())))
                                        .apply(Term::empty_list()),
                                ),
                        ),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(Data::integer((-2).into())),
                Constant::Data(Data::integer((-1).into())),
                Constant::Data(Data::integer(0.into())),
            ]))),
        false,
    );
}

#[test]
fn when_tuple_deconstruction() {
    let src = r#"
      type Thing {
        idx: Int,
      }

      type Datum {
        A(Thing)
        B
      }

      type RedSpend {
        Spend(Int)
        Buy
      }

      validator {
        fn spend(dat: Datum, red: RedSpend, ctx: Data) {
          when (dat, red) is {
            (A(a), Spend(x)) ->
              (a.idx == x)?
            (_, _) ->
              True
          }
        }
      }
    "#;

    assert_uplc(
        src,
        Term::equals_integer()
            .apply(Term::integer(0.into()))
            .apply(Term::var("constr_index_exposer").apply(Term::var("dat")))
            .if_else(
                Term::equals_integer()
                    .apply(Term::integer(0.into()))
                    .apply(Term::var("constr_index_exposer").apply(Term::var("red")))
                    .if_else(
                        Term::equals_integer()
                            .apply(
                                Term::un_i_data().apply(
                                    Term::var("constr_get_field")
                                        .apply(
                                            Term::var("constr_fields_exposer")
                                                .apply(Term::var("a")),
                                        )
                                        .apply(Term::integer(0.into())),
                                ),
                            )
                            .apply(Term::var("x"))
                            .lambda("x")
                            .apply(
                                Term::un_i_data()
                                    .apply(Term::head_list().apply(Term::var("red_constr_fields"))),
                            )
                            .lambda("red_constr_fields")
                            .apply(Term::var("constr_fields_exposer").apply(Term::var("red")))
                            .delay(),
                        Term::var("other_clauses"),
                    )
                    .force()
                    .lambda("a")
                    .apply(Term::head_list().apply(Term::var("dat_constr_fields")))
                    .lambda("dat_constr_fields")
                    .apply(Term::var("constr_fields_exposer").apply(Term::var("dat")))
                    .delay(),
                Term::var("other_clauses"),
            )
            .force()
            .lambda("other_clauses")
            .apply(Term::bool(true).delay())
            .lambda("dat")
            .apply(Term::fst_pair().apply(Term::var("pair_subject")))
            .lambda("red")
            .apply(Term::snd_pair().apply(Term::var("pair_subject")))
            .lambda("pair_subject")
            .apply(
                Term::mk_pair_data()
                    .apply(Term::var("dat"))
                    .apply(Term::var("red")),
            )
            .delayed_if_else(Term::unit(), Term::Error)
            .lambda("dat")
            .apply(
                Term::var("dat").lambda("_").apply(
                    Term::var("expect_Datum")
                        .lambda("expect_Datum")
                        .apply(
                            Term::equals_integer()
                                .apply(Term::integer(0.into()))
                                .apply(Term::var("subject"))
                                .delayed_if_else(
                                    Term::tail_list()
                                        .apply(Term::var("dat_constr_fields"))
                                        .delayed_choose_list(
                                            Term::unit().lambda("_").apply(
                                                Term::var("expect_Thing")
                                                    .apply(Term::var("field_1")),
                                            ),
                                            Term::Error.trace(Term::string("List/Tuple/Constr contains more items than expected")),
                                        )
                                        .lambda("field_1")
                                        .apply(
                                            Term::head_list().apply(Term::var("dat_constr_fields")),
                                        )
                                        .lambda("dat_constr_fields")
                                        .apply(
                                            Term::var("constr_fields_exposer")
                                                .apply(Term::var("dat")),
                                        ),
                                    Term::equals_integer()
                                        .apply(Term::integer(1.into()))
                                        .apply(Term::var("subject"))
                                        .delayed_if_else(
                                            Term::var("constr_fields_exposer")
                                                .apply(Term::var("dat"))
                                                .delayed_choose_list(
                                                    Term::unit(),
                                                    Term::Error
                                                        .trace(Term::string("Expected no fields for Constr"))
                                                ),
                                            Term::Error.trace(Term::string("Constr index did not match any type variant")),
                                        ),
                                )
                                .lambda("subject")
                                .apply(Term::var("constr_index_exposer").apply(Term::var("dat")))
                                .lambda("dat"),
                        )
                        .lambda("expect_Thing")
                        .apply(
                            Term::equals_integer()
                                .apply(Term::integer(0.into()))
                                .apply(Term::var("subject"))
                                .delayed_if_else(
                                    Term::tail_list()
                                        .apply(Term::var("field_1_constr_fields"))
                                        .delayed_choose_list(
                                            Term::unit(),
                                            Term::Error.trace(Term::string("List/Tuple/Constr contains more items than expected")),
                                        )
                                        .lambda("idx")
                                        .apply(
                                            Term::un_i_data().apply(
                                                Term::head_list()
                                                    .apply(Term::var("field_1_constr_fields")),
                                            ),
                                        )
                                        .lambda("field_1_constr_fields")
                                        .apply(
                                            Term::var("constr_fields_exposer")
                                                .apply(Term::var("field_1")),
                                        ),
                                    Term::Error.trace(Term::string(
                                        "Constr index did not match any type variant",
                                    )),
                                )
                                .lambda("subject")
                                .apply(
                                    Term::var("constr_index_exposer").apply(Term::var("field_1")),
                                )
                                .lambda("field_1"),
                        )
                        .apply(Term::var("dat")),
                ),
            )
            .lambda("red")
            .apply(
                Term::var("red").lambda("_").apply(
                    Term::var("expect_RedSpend")
                        .lambda("expect_RedSpend")
                        .apply(
                            Term::equals_integer()
                                .apply(Term::integer(0.into()))
                                .apply(Term::var("subject"))
                                .delayed_if_else(
                                    Term::tail_list()
                                        .apply(Term::var("red_constr_fields"))
                                        .delayed_choose_list(
                                            Term::unit(),
                                            Term::Error.trace(Term::string("List/Tuple/Constr contains more items than expected")),
                                        )
                                        .lambda("field_1")
                                        .apply(Term::un_i_data().apply(
                                            Term::head_list().apply(Term::var("red_constr_fields")),
                                        ))
                                        .lambda("red_constr_fields")
                                        .apply(
                                            Term::var("constr_fields_exposer")
                                                .apply(Term::var("red")),
                                        ),
                                    Term::equals_integer()
                                        .apply(Term::integer(1.into()))
                                        .apply(Term::var("subject"))
                                        .delayed_if_else(
                                            Term::var("constr_fields_exposer")
                                                .apply(Term::var("red"))
                                                .delayed_choose_list(
                                                    Term::unit(),
                                                    Term::Error
                                                        .trace(Term::string("Expected no fields for Constr"))
                                                ),
                                            Term::Error.trace(Term::string("Constr index did not match any type variant")),
                                        ),
                                )
                                .lambda("subject")
                                .apply(Term::var("constr_index_exposer").apply(Term::var("red")))
                                .lambda("red"),
                        )
                        .apply(Term::var("red")),
                ),
            )
            .lambda("ctx")
            .lambda("red")
            .lambda("dat")
            .lambda("constr_get_field")
            .apply(
                Term::var("constr_get_field")
                    .apply(Term::var("constr_get_field"))
                    .apply(Term::integer(0.into())),
            )
            .lambda("constr_get_field")
            .apply(
                Term::equals_integer()
                    .apply(Term::var("__wanted_arg".to_string()))
                    .apply(Term::var("__current_arg_number".to_string()))
                    .if_else(
                        Term::head_list(),
                        Term::var(CONSTR_GET_FIELD)
                            .apply(Term::var(CONSTR_GET_FIELD))
                            .apply(
                                Term::add_integer()
                                    .apply(Term::var("__current_arg_number"))
                                    .apply(Term::integer(1.into())),
                            )
                            .apply(
                                Term::tail_list().apply(Term::var("__current_list_of_constr_args")),
                            )
                            .apply(Term::var("__wanted_arg"))
                            .lambda("__current_list_of_constr_args"),
                    )
                    .apply(Term::var("__list_of_constr_args"))
                    .lambda("__wanted_arg")
                    .lambda("__list_of_constr_args")
                    .lambda("__current_arg_number")
                    .lambda(CONSTR_GET_FIELD),
            )
            .lambda("constr_fields_exposer")
            .apply(
                Term::snd_pair()
                    .apply(Term::unconstr_data().apply(Term::var("x")))
                    .lambda("x"),
            )
            .lambda("constr_index_exposer")
            .apply(
                Term::fst_pair()
                    .apply(Term::unconstr_data().apply(Term::var("x")))
                    .lambda("x"),
            ),
        false,
    );
}
