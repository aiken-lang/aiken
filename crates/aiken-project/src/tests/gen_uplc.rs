use super::TestProject;
use crate::module::CheckedModules;
use aiken_lang::ast::{Definition, Function, TraceLevel, Tracing, TypedTest, TypedValidator};
use pretty_assertions::assert_eq;
use std::rc::Rc;
use uplc::{
    ast::{Constant, Data, DeBruijn, Name, Program, Term, Type},
    builder::{CONSTR_FIELDS_EXPOSER, CONSTR_INDEX_EXPOSER, EXPECT_ON_LIST},
    machine::{cost_model::ExBudget, runtime::Compressable},
    optimize::{self},
};

enum TestType {
    Func(TypedTest),
    Validator(TypedValidator),
}

fn assert_uplc(source_code: &str, expected: Term<Name>, should_fail: bool, verbose_mode: bool) {
    let mut project = TestProject::new();

    let modules = CheckedModules::singleton(project.check(project.parse(source_code)));

    let mut generator = project.new_generator(if verbose_mode {
        Tracing::All(TraceLevel::Verbose)
    } else {
        Tracing::All(TraceLevel::Silent)
    });

    let Some(checked_module) = modules.values().next() else {
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
            let program = generator.generate_raw(func, &[], &script.1);

            let pretty_program = program.to_pretty();

            let debruijn_program: Program<DeBruijn> = program.try_into().unwrap();

            let expected = Program {
                version: (1, 1, 0),
                term: expected,
            };

            let expected = optimize::aiken_optimize_and_intern(expected);

            let pretty_expected = expected.to_pretty();

            let expected: Program<DeBruijn> = expected.try_into().unwrap();

            assert!(
                debruijn_program.to_pretty() == expected.to_pretty(),
                "=============== generated:\n{}\n\n=============== expected:\n{}",
                pretty_program,
                pretty_expected,
            );

            let mut eval = debruijn_program.eval(ExBudget::default());

            assert_eq!(
                eval.failed(false),
                should_fail,
                "logs - {}\n",
                format!("{:#?}", eval.logs())
            );

            assert!(if should_fail {
                eval.failed(false)
            } else {
                !eval.failed(false)
            });
        }
        TestType::Validator(func) => {
            let program = generator.generate(func, &script.1);

            let pretty_program = program.to_pretty();

            let debruijn_program: Program<DeBruijn> = program.try_into().unwrap();

            let expected = Program {
                version: (1, 1, 0),
                term: expected,
            };

            let expected = optimize::aiken_optimize_and_intern(expected);

            let pretty_expected = expected.to_pretty();

            let expected: Program<DeBruijn> = expected.try_into().unwrap();

            assert!(
                debruijn_program.to_pretty() == expected.to_pretty(),
                "=============== generated:\n{}\n\n=============== expected:\n{}",
                pretty_program,
                pretty_expected,
            );
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

    let uplc = Term::equals_integer()
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
        .apply(Term::integer(3.into()));

    assert_uplc(src, uplc.clone(), false, true);

    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::equals_data()
        .apply(
            Term::list_data().apply(
                Term::var("repeat")
                    .lambda("repeat")
                    .apply(
                        Term::var("repeat")
                            .apply(Term::var("repeat"))
                            .apply(Term::var("n"))
                            .lambda("repeat")
                            .apply(
                                Term::less_than_equals_integer()
                                    .apply(Term::var("n"))
                                    .apply(Term::integer(0.into()))
                                    .delayed_if_then_else(
                                        Term::empty_list(),
                                        Term::mk_cons()
                                            .apply(Term::b_data().apply(Term::var("x")))
                                            .apply(
                                                Term::var("repeat")
                                                    .apply(Term::var("repeat"))
                                                    .apply(
                                                        Term::subtract_integer()
                                                            .apply(Term::var("n"))
                                                            .apply(Term::integer(1.into())),
                                                    ),
                                            ),
                                    )
                                    .lambda("n")
                                    .lambda("repeat"),
                            )
                            .lambda("n")
                            .lambda("x"),
                    )
                    .apply(Term::byte_string("aiken".as_bytes().to_vec()))
                    .apply(Term::integer(2.into())),
            ),
        )
        .apply(Term::list_data().apply(Term::list_values(vec![
            Constant::Data(Data::bytestring("aiken".as_bytes().to_vec())),
            Constant::Data(Data::bytestring("aiken".as_bytes().to_vec())),
        ])));

    assert_uplc(src, uplc.clone(), false, true);

    assert_uplc(src, uplc, false, false);
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

    let uplc =
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
                        .apply(
                            Term::var("foldr")
                                .apply(Term::var("foldr"))
                                .apply(Term::var("xs"))
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
                                                        .apply(Term::var("rest")),
                                                )
                                                .lambda("rest")
                                                .apply(Term::tail_list().apply(Term::var("xs")))
                                                .lambda("x")
                                                .apply(Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("xs")),
                                                )),
                                        )
                                        .lambda("xs")
                                        .lambda("foldr"),
                                )
                                .lambda("zero")
                                .lambda("f")
                                .lambda("xs"),
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
            ])));

    assert_uplc(src, uplc.clone(), false, true);

    assert_uplc(src, uplc, false, false);
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

    let uplc =
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
                        .apply(
                            Term::var("foldr")
                                .apply(Term::var("foldr"))
                                .apply(Term::var("xs"))
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
                                                        .apply(Term::var("rest")),
                                                )
                                                .lambda("rest")
                                                .apply(Term::tail_list().apply(Term::var("xs")))
                                                .lambda("x")
                                                .apply(Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("xs")),
                                                )),
                                        )
                                        .lambda("xs")
                                        .lambda("foldr"),
                                )
                                .lambda("zero")
                                .lambda("f")
                                .lambda("xs"),
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
            ])));

    assert_uplc(src, uplc.clone(), false, true);

    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_5_direct_head() {
    let src = r#"
        use aiken/builtin.{head_list}

        test head_1() {
          let head = fn(xs){
              when xs is {
                [] -> None
                _ -> Some(head_list(xs))
              }
            }

          head([1, 2, 3]) == Some(1)
        }
    "#;

    let uplc = Term::equals_data()
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
        ));
    assert_uplc(src, uplc.clone(), false, true);

    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_5_direct_2_heads() {
    let src = r#"
        use aiken/builtin.{head_list}

        test head_2() {
          let head = fn(xs: List<Int>){
              when xs is {
                [] -> None
                [a] -> Some(xs)
                [a, b, ..] -> Some([a,b])
              }
            }

          head([1, 2, 3]) == Some([1, 2])
        }
    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::var("head")
                .lambda("head")
                .apply(
                    Term::var("xs")
                        .delayed_choose_list(
                            Term::Constant(Constant::Data(Data::constr(1, vec![])).into()),
                            Term::var("tail_1")
                                .delayed_choose_list(
                                    Term::constr_data()
                                        .apply(Term::integer(0.into()))
                                        .apply(
                                            Term::mk_cons()
                                                .apply(Term::list_data().apply(Term::var("xs")))
                                                .apply(Term::empty_list()),
                                        )
                                        .lambda("a")
                                        .apply(
                                            Term::un_i_data()
                                                .apply(Term::head_list().apply(Term::var("xs"))),
                                        ),
                                    Term::constr_data()
                                        .apply(Term::integer(0.into()))
                                        .apply(
                                            Term::mk_cons()
                                                .apply(
                                                    Term::list_data().apply(
                                                        Term::mk_cons()
                                                            .apply(
                                                                Term::i_data()
                                                                    .apply(Term::var("a")),
                                                            )
                                                            .apply(
                                                                Term::mk_cons()
                                                                    .apply(
                                                                        Term::i_data()
                                                                            .apply(Term::var("b")),
                                                                    )
                                                                    .apply(Term::empty_list()),
                                                            ),
                                                    ),
                                                )
                                                .apply(Term::empty_list()),
                                        )
                                        .lambda("b")
                                        .apply(
                                            Term::un_i_data().apply(
                                                Term::head_list().apply(Term::var("tail_1")),
                                            ),
                                        )
                                        .lambda("a")
                                        .apply(
                                            Term::un_i_data()
                                                .apply(Term::head_list().apply(Term::var("xs"))),
                                        ),
                                )
                                .lambda("tail_1")
                                .apply(Term::tail_list().apply(Term::var("xs"))),
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
            Constant::Data(Data::constr(
                0,
                vec![Data::list(vec![
                    Data::integer(1.into()),
                    Data::integer(2.into()),
                ])],
            ))
            .into(),
        ));

    assert_uplc(src, uplc.clone(), false, true);

    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::equals_data()
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
        ));

    assert_uplc(src, uplc.clone(), false, true);

    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::equals_data()
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
        ));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::equals_integer()
        .apply(Term::integer(1.into()))
        .apply(Term::integer(1.into()))
        .delayed_if_then_else(Term::bool(true), Term::bool(false));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_6_equals_pair() {
    let src = r#"
        test foo() {
          Pair(1, []) == Pair(1, [])
        }
    "#;

    let uplc = Term::equals_data()
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
        );

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_6_equals_tuple() {
    let src = r#"
        test foo() {
          (1, []) == (1, [])
        }
    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::list_data().apply(Term::Constant(
                Constant::ProtoList(
                    Type::Data,
                    vec![
                        Constant::Data(Data::integer(1.into())),
                        Constant::Data(Data::list(vec![])),
                    ],
                )
                .into(),
            )),
        )
        .apply(
            Term::list_data().apply(Term::Constant(
                Constant::ProtoList(
                    Type::Data,
                    vec![
                        Constant::Data(Data::integer(1.into())),
                        Constant::Data(Data::list(vec![])),
                    ],
                )
                .into(),
            )),
        );

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_7_unzip_tuple() {
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

    let uplc =
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("unzip")
                        .lambda("unzip")
                        .apply(Term::var("unzip").apply(Term::var("unzip")))
                        .lambda("unzip")
                        .apply(
                            Term::var("xs")
                                .delayed_choose_list(
                                    Term::list_values(vec![
                                        Constant::Data(Data::list(vec![])),
                                        Constant::Data(Data::list(vec![])),
                                    ]),
                                    Term::mk_cons()
                                        .apply(
                                            Term::list_data().apply(
                                                Term::mk_cons()
                                                    .apply(Term::i_data().apply(Term::var("a")))
                                                    .apply(Term::var("a_tail")),
                                            ),
                                        )
                                        .apply(
                                            Term::mk_cons()
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
                                                .apply(Term::empty_list()),
                                        )
                                        .lambda("b_tail")
                                        .apply(Term::unlist_data().apply(Term::head_list().apply(
                                            Term::tail_list().apply(Term::var("tail_tuple")),
                                        )))
                                        .lambda("a_tail")
                                        .apply(Term::unlist_data().apply(
                                            Term::head_list().apply(Term::var("tail_tuple")),
                                        ))
                                        .lambda("tail_tuple")
                                        .apply(
                                            Term::var("unzip")
                                                .apply(Term::var("unzip"))
                                                .apply(Term::var("rest")),
                                        )
                                        .lambda("b")
                                        .apply(Term::un_b_data().apply(Term::head_list().apply(
                                            Term::tail_list().apply(Term::var("head_tuple")),
                                        )))
                                        .lambda("a")
                                        .apply(Term::un_i_data().apply(
                                            Term::head_list().apply(Term::var("head_tuple")),
                                        ))
                                        .lambda("rest")
                                        .apply(Term::tail_list().apply(Term::var("xs")))
                                        .lambda("head_tuple")
                                        .apply(
                                            Term::unlist_data()
                                                .apply(Term::head_list().apply(Term::var("xs"))),
                                        ),
                                )
                                .lambda("xs")
                                .lambda("unzip"),
                        )
                        .apply(Term::var("x")),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(Data::list(vec![
                    Data::integer(3.into()),
                    Data::integer(4.into()),
                ])),
                Constant::Data(Data::list(vec![
                    Data::bytestring(vec![85]),
                    Data::bytestring(vec![119, 153]),
                ])),
            ])))
            .lambda("x")
            .apply(Term::list_values(vec![
                Constant::Data(Data::list(vec![
                    Data::integer(3.into()),
                    Data::bytestring(vec![85]),
                ])),
                Constant::Data(Data::list(vec![
                    Data::integer(4.into()),
                    Data::bytestring(vec![119, 153]),
                ])),
            ]));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_7_unzip_pair() {
    let src = r#"
      type Pairs<a,b> = List<Pair<a,b>>

      pub fn unzip(xs: Pairs<a, b>) -> Pair<List<a>, List<b>> {
        when xs is {
          [] -> Pair([], [])
          [Pair(a, b), ..rest] -> {
            let Pair(a_tail, b_tail) = unzip(rest)
            Pair([a, ..a_tail], [b, ..b_tail])
          }
        }
      }

      test unzip1() {
        let x = [Pair(3, #"55"), Pair(4, #"7799")]

        unzip(x) == Pair([3, 4], [#"55", #"7799"])
      }
    "#;

    let uplc = Term::equals_data()
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
                                                        .apply(Term::i_data().apply(Term::var("a")))
                                                        .apply(Term::var("a_tail")),
                                                ),
                                            )
                                            .apply(
                                                Term::list_data().apply(
                                                    Term::mk_cons()
                                                        .apply(Term::b_data().apply(Term::var("b")))
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
                                            .lambda("b")
                                            .apply(Term::un_b_data().apply(
                                                Term::snd_pair().apply(Term::var("head_pair")),
                                            ))
                                            .lambda("a")
                                            .apply(Term::un_i_data().apply(
                                                Term::fst_pair().apply(Term::var("head_pair")),
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
        ]));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::var("is_empty")
        .lambda("is_empty")
        .apply(
            Term::equals_integer()
                .apply(Term::length_of_bytearray().apply(Term::var("bytes")))
                .apply(Term::integer(0.into()))
                .lambda("bytes"),
        )
        .apply(Term::byte_string(vec![]))
        .delayed_if_then_else(
            Term::bool(true),
            Term::bool(true).if_then_else(Term::bool(false), Term::bool(true)),
        );

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::var("is_empty")
        .lambda("is_empty")
        .apply(
            Term::equals_integer()
                .apply(Term::length_of_bytearray().apply(Term::var("bytes")))
                .apply(Term::integer(0.into()))
                .lambda("bytes"),
        )
        .apply(Term::byte_string(vec![1]))
        .delayed_if_then_else(
            Term::bool(false),
            Term::bool(false).if_then_else(Term::bool(false), Term::bool(true)),
        );

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::var("is_empty")
        .lambda("is_empty")
        .apply(
            Term::equals_integer()
                .apply(Term::length_of_bytearray().apply(Term::var("bytes")))
                .apply(Term::integer(0.into()))
                .lambda("bytes"),
        )
        .apply(Term::byte_string(vec![]))
        .delayed_if_then_else(
            Term::bool(true),
            Term::bool(true).if_then_else(Term::bool(false), Term::bool(true)),
        );

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::equals_data()
        .apply(
            Term::var("map")
                .lambda("map")
                .apply(
                    Term::equals_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("constr_index"))
                        .delayed_if_then_else(
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
                                    Term::un_i_data()
                                        .apply(Term::head_list().apply(Term::var("constr_fields"))),
                                )
                                .lambda("constr_fields")
                                .apply(
                                    Term::snd_pair()
                                        .apply(Term::unconstr_data().apply(Term::var("opt"))),
                                ),
                        )
                        .lambda("constr_index")
                        .apply(
                            Term::fst_pair().apply(Term::unconstr_data().apply(Term::var("opt"))),
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
        ));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::equals_data()
        .apply(
            Term::var("map")
                .lambda("map")
                .apply(
                    Term::equals_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("constr_index"))
                        .delayed_if_then_else(
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
                                    Term::un_i_data()
                                        .apply(Term::head_list().apply(Term::var("constr_fields"))),
                                )
                                .lambda("constr_fields")
                                .apply(
                                    Term::snd_pair()
                                        .apply(Term::unconstr_data().apply(Term::var("opt"))),
                                ),
                        )
                        .lambda("constr_index")
                        .apply(
                            Term::fst_pair().apply(Term::unconstr_data().apply(Term::var("opt"))),
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
        ));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::equals_data()
        .apply(
            Term::list_data().apply(
                Term::var("map")
                    .lambda("map")
                    .apply(
                        Term::var("map")
                            .apply(Term::var("map"))
                            .apply(Term::var("xs"))
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
                                                    .apply(Term::var("rest")),
                                            )
                                            .lambda("rest")
                                            .apply(Term::tail_list().apply(Term::var("xs")))
                                            .lambda("x")
                                            .apply(
                                                Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("xs")),
                                                ),
                                            ),
                                    )
                                    .lambda("xs")
                                    .lambda("map"),
                            )
                            .lambda("f")
                            .lambda("xs"),
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
        .apply(Term::list_data().apply(Term::empty_list()));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
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

    let uplc = Term::equals_data()
        .apply(
            Term::list_data().apply(
                Term::var("map")
                    .lambda("map")
                    .apply(
                        Term::var("map")
                            .apply(Term::var("map"))
                            .apply(Term::var("xs"))
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
                                                    .apply(Term::var("rest")),
                                            )
                                            .lambda("rest")
                                            .apply(Term::tail_list().apply(Term::var("xs")))
                                            .lambda("x")
                                            .apply(
                                                Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("xs")),
                                                ),
                                            ),
                                    )
                                    .lambda("xs")
                                    .lambda("map"),
                            )
                            .lambda("f")
                            .lambda("xs"),
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
        ])));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
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

    let uplc =
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("filter")
                        .lambda("filter")
                        .apply(
                            Term::var("filter")
                                .apply(Term::var("filter"))
                                .apply(Term::var("xs"))
                                .lambda("filter")
                                .apply(
                                    Term::var("xs")
                                        .delayed_choose_list(
                                            Term::empty_list(),
                                            Term::var("f")
                                                .apply(Term::var("x"))
                                                .delayed_if_then_else(
                                                    Term::mk_cons()
                                                        .apply(Term::i_data().apply(Term::var("x")))
                                                        .apply(
                                                            Term::var("filter")
                                                                .apply(Term::var("filter"))
                                                                .apply(Term::var("rest")),
                                                        ),
                                                    Term::var("filter")
                                                        .apply(Term::var("filter"))
                                                        .apply(Term::var("rest")),
                                                )
                                                .lambda("rest")
                                                .apply(Term::tail_list().apply(Term::var("xs")))
                                                .lambda("x")
                                                .apply(Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("xs")),
                                                )),
                                        )
                                        .lambda("xs")
                                        .lambda("filter"),
                                )
                                .lambda("f")
                                .lambda("xs"),
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
            ])));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_14_list_creation() {
    let src = r#"
      test foo() {
        [0 - 2, 0 - 1, 0] == [-2, -1, 0]
      }
    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::list_data().apply(
                Term::mk_cons()
                    .apply(
                        Term::i_data().apply(
                            Term::subtract_integer()
                                .apply(Term::integer(0.into()))
                                .apply(Term::integer(2.into())),
                        ),
                    )
                    .apply(
                        Term::mk_cons()
                            .apply(
                                Term::i_data().apply(
                                    Term::subtract_integer()
                                        .apply(Term::integer(0.into()))
                                        .apply(Term::integer(1.into())),
                                ),
                            )
                            .apply(
                                Term::mk_cons()
                                    .apply(Term::Constant(
                                        Constant::Data(Data::integer(0.into())).into(),
                                    ))
                                    .apply(Term::empty_list()),
                            ),
                    ),
            ),
        )
        .apply(Term::list_data().apply(Term::list_values(vec![
            Constant::Data(Data::integer((-2).into())),
            Constant::Data(Data::integer((-1).into())),
            Constant::Data(Data::integer(0.into())),
        ])));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_15_zero_arg() {
    let src = r#"
      pub opaque type Pairs<key, value> {
        inner: List<Pair<key, value>>,
      }

      pub fn new() {
        Pairs { inner: [] }
      }

      test new_1() {
        new() == Pairs { inner: [] }
      }
    "#;

    let uplc = Term::equals_data()
        .apply(Term::map_data().apply(Term::empty_map()))
        .apply(Term::map_data().apply(Term::empty_map()));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_16_drop() {
    let src = r#"
      use aiken/builtin

      pub fn slice(bytes: ByteArray, start: Int, end: Int) -> ByteArray {
        builtin.slice_bytearray(start, end, bytes)
      }

      pub fn length(bytes: ByteArray) -> Int {
        builtin.length_of_bytearray(bytes)
      }

      pub fn drop(bytes: ByteArray, n: Int) -> ByteArray {
        slice(bytes, n, length(bytes) - n)
      }

      test drop_1() {
        let x =
            #"01020304050607"
        drop(x, 2) == #"0304050607"
      }
    "#;

    let uplc = Term::equals_bytestring()
        .apply(
            Term::var("drop")
                .lambda("drop")
                .apply(
                    Term::var("slice")
                        .apply(Term::var("bytes"))
                        .apply(Term::var("n"))
                        .apply(
                            Term::subtract_integer()
                                .apply(Term::var("length").apply(Term::var("bytes")))
                                .apply(Term::var("n")),
                        )
                        .lambda("n")
                        .lambda("bytes"),
                )
                .lambda("slice")
                .apply(
                    Term::slice_bytearray()
                        .apply(Term::var("start"))
                        .apply(Term::var("end"))
                        .apply(Term::var("bytes"))
                        .lambda("end")
                        .lambda("start")
                        .lambda("bytes"),
                )
                .lambda("length")
                .apply(
                    Term::length_of_bytearray()
                        .apply(Term::var("bytes"))
                        .lambda("bytes"),
                )
                .apply(Term::var("x"))
                .apply(Term::integer(2.into())),
        )
        .apply(Term::byte_string(vec![3, 4, 5, 6, 7]))
        .lambda("x")
        .apply(Term::byte_string(vec![1, 2, 3, 4, 5, 6, 7]));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_17_take() {
    let src = r#"
      use aiken/builtin

      pub fn slice(bytes: ByteArray, start: Int, end: Int) -> ByteArray {
        builtin.slice_bytearray(start, end, bytes)
      }

      pub fn take(bytes: ByteArray, n: Int) -> ByteArray {
        slice(bytes, 0, n)
      }

      test take_1() {
        take(#"010203", 2) == #"0102"
      }
    "#;

    let uplc = Term::equals_bytestring()
        .apply(
            Term::var("take")
                .lambda("take")
                .apply(
                    Term::var("slice")
                        .apply(Term::var("bytes"))
                        .apply(Term::integer(0.into()))
                        .apply(Term::var("n"))
                        .lambda("n")
                        .lambda("bytes"),
                )
                .lambda("slice")
                .apply(
                    Term::slice_bytearray()
                        .apply(Term::var("start"))
                        .apply(Term::var("end"))
                        .apply(Term::var("bytes"))
                        .lambda("end")
                        .lambda("start")
                        .lambda("bytes"),
                )
                .apply(Term::byte_string(vec![1, 2, 3]))
                .apply(Term::integer(2.into())),
        )
        .apply(Term::byte_string(vec![1, 2]));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_18_or_else() {
    let src = r#"
      pub fn or_else(opt: Option<a>, default: a) -> a {
        when opt is {
            None ->
              default
            Some(a) ->
              a
        }
      }

      test or_else_2() {
        or_else(Some(42), 14) == 42
      }
    "#;

    let uplc = Term::equals_integer()
        .apply(
            Term::var("or_else")
                .lambda("or_else")
                .apply(
                    Term::equals_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("subject"))
                        .delayed_if_then_else(
                            Term::var("default"),
                            Term::var("a")
                                .lambda("a")
                                .apply(
                                    Term::un_i_data()
                                        .apply(Term::head_list().apply(Term::var("opt_fields"))),
                                )
                                .lambda("opt_fields")
                                .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("opt"))),
                        )
                        .lambda("subject")
                        .apply(
                            Term::fst_pair().apply(Term::unconstr_data().apply(Term::var("opt"))),
                        )
                        .lambda("default")
                        .lambda("opt"),
                )
                .apply(Term::data(Data::constr(0, vec![Data::integer(42.into())])))
                .apply(Term::integer(14.into())),
        )
        .apply(Term::integer(42.into()))
        .lambda(CONSTR_FIELDS_EXPOSER)
        .apply(
            Term::snd_pair()
                .apply(Term::unconstr_data().apply(Term::var("x")))
                .lambda("x"),
        );

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_19_map_none_wrap_int() {
    let src = r#"
      pub fn map(opt: Option<a>, f: fn(a) -> result) -> Option<result> {
        when opt is {
          None ->
            None
          Some(a) ->
            Some(f(a))
        }
      }

      test map_1() {
        map(None, fn(_) { 14 }) == None
      }
    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::var("map")
                .lambda("map")
                .apply(
                    Term::equals_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("subject"))
                        .delayed_if_then_else(
                            Term::data(Data::constr(1, vec![])),
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
                                // "a" generic is unbound in this case thus
                                // the aiken compiler does not unwrap the value to pass to f
                                .apply(Term::head_list().apply(Term::var("opt_fields")))
                                .lambda("opt_fields")
                                .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("opt"))),
                        )
                        .lambda("subject")
                        .apply(
                            Term::fst_pair().apply(Term::unconstr_data().apply(Term::var("opt"))),
                        )
                        .lambda("f")
                        .lambda("opt"),
                )
                .apply(Term::data(Data::constr(1, vec![])))
                .apply(Term::integer(14.into()).lambda("_")),
        )
        .apply(Term::data(Data::constr(1, vec![])))
        .lambda(CONSTR_FIELDS_EXPOSER)
        .apply(
            Term::snd_pair()
                .apply(Term::unconstr_data().apply(Term::var("x")))
                .lambda("x"),
        );

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_19_map_wrap_void() {
    let src = r#"
      pub fn map(opt: Option<a>, f: fn(a) -> result) -> Option<result> {
        when opt is {
          None ->
            None
          Some(a) ->
            Some(f(a))
        }
      }

      test map_1() {
        map(None, fn(_) { Void }) == None
      }
    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::var("map")
                .lambda("map")
                .apply(
                    Term::equals_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("subject"))
                        .delayed_if_then_else(
                            Term::data(Data::constr(1, vec![])),
                            Term::constr_data()
                                .apply(Term::integer(0.into()))
                                .apply(
                                    Term::mk_cons()
                                        .apply(
                                            Term::data(Data::constr(0, vec![]))
                                                .lambda("_")
                                                .apply(Term::var("f").apply(Term::var("a"))),
                                        )
                                        .apply(Term::empty_list()),
                                )
                                .lambda("a")
                                // "a" generic is unbound in this case thus
                                // the aiken compiler does not unwrap the value to pass to f
                                .apply(Term::head_list().apply(Term::var("opt_fields")))
                                .lambda("opt_fields")
                                .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("opt"))),
                        )
                        .lambda("subject")
                        .apply(
                            Term::fst_pair().apply(Term::unconstr_data().apply(Term::var("opt"))),
                        )
                        .lambda("f")
                        .lambda("opt"),
                )
                .apply(Term::data(Data::constr(1, vec![])))
                .apply(Term::unit().lambda("_")),
        )
        .apply(Term::data(Data::constr(1, vec![])))
        .lambda(CONSTR_FIELDS_EXPOSER)
        .apply(
            Term::snd_pair()
                .apply(Term::unconstr_data().apply(Term::var("x")))
                .lambda("x"),
        );

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_20_map_some() {
    let src = r#"
      pub fn map(opt: Option<a>, f: fn(a) -> result) -> Option<result> {
        when opt is {
          None ->
            None
          Some(a) ->
            Some(f(a))
        }
      }

      test map_1() {
        map(Some(14), fn(n){ n + 1 }) == Some(15)
      }
    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::var("map")
                .lambda("map")
                .apply(
                    Term::equals_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("constr_index"))
                        .delayed_if_then_else(
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
                                    Term::un_i_data()
                                        .apply(Term::head_list().apply(Term::var("constr_fields"))),
                                )
                                .lambda("constr_fields")
                                .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("opt"))),
                        )
                        .lambda("constr_index")
                        .apply(
                            Term::fst_pair().apply(Term::unconstr_data().apply(Term::var("opt"))),
                        )
                        .lambda("f")
                        .lambda("opt"),
                )
                .apply(Term::Constant(
                    Constant::Data(Data::constr(0, vec![Data::integer(14.into())])).into(),
                ))
                .apply(
                    Term::add_integer()
                        .apply(Term::var("n"))
                        .apply(Term::integer(1.into()))
                        .lambda("n"),
                ),
        )
        .apply(Term::Constant(
            Constant::Data(Data::constr(0, vec![Data::integer(15.into())])).into(),
        ))
        .constr_fields_exposer();

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_22_filter_map() {
    let src = r#"
          pub fn foldr(xs: List<a>, f: fn(a, b) -> b, zero: b) -> b {
            when xs is {
                [] ->
                  zero
                [x, ..rest] ->
                  f(x, foldr(rest, f, zero))
            }
          }

          pub fn filter_map(xs: List<a>, f: fn(a) -> Option<b>) -> List<b> {
            foldr(
                xs,
                fn(x, ys) {
                  when f(x) is {
                    None ->
                      ys
                    Some(y) ->
                      [y, ..ys]
                }
              },
              [],
            )
          }

          test filter_map_1() {
            filter_map([], fn(_) { Some(42) }) == []
          }
    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::list_data().apply(
                Term::var("filter_map")
                    .lambda("filter_map")
                    .apply(
                        Term::var("foldr")
                            .apply(Term::var("xs"))
                            .apply(
                                Term::equals_integer()
                                    .apply(Term::integer(1.into()))
                                    .apply(Term::var("subject_index"))
                                    .delayed_if_then_else(
                                        Term::var("ys"),
                                        Term::mk_cons()
                                            .apply(Term::i_data().apply(Term::var("y")))
                                            .apply(Term::var("ys"))
                                            .lambda("y")
                                            .apply(
                                                Term::un_i_data().apply(
                                                    Term::head_list()
                                                        .apply(Term::var("subject_fields")),
                                                ),
                                            )
                                            .lambda("subject_fields")
                                            .apply(
                                                Term::var(CONSTR_FIELDS_EXPOSER)
                                                    .apply(Term::var("subject")),
                                            ),
                                    )
                                    .lambda("subject_index")
                                    .apply(
                                        Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("subject")),
                                    )
                                    .lambda("subject")
                                    .apply(Term::var("f").apply(Term::var("x")))
                                    .lambda("ys")
                                    .lambda("x"),
                            )
                            .apply(Term::empty_list())
                            .lambda("f")
                            .lambda("xs"),
                    )
                    .lambda("foldr")
                    .apply(
                        Term::var("foldr")
                            .apply(Term::var("foldr"))
                            .apply(Term::var("xs"))
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
                                                    .apply(Term::var("rest")),
                                            )
                                            .lambda("rest")
                                            .apply(Term::tail_list().apply(Term::var("xs")))
                                            .lambda("x")
                                            .apply(Term::head_list().apply(Term::var("xs"))),
                                    )
                                    .lambda("xs")
                                    .lambda("foldr"),
                            )
                            .lambda("zero")
                            .lambda("f")
                            .lambda("xs"),
                    )
                    .apply(Term::empty_list())
                    .apply(Term::data(Data::constr(0, vec![Data::integer(42.into())])).lambda("_")),
            ),
        )
        .apply(Term::list_data().apply(Term::empty_list()))
        .constr_fields_exposer()
        .constr_index_exposer();

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_23_to_list() {
    let src = r#"
        pub type Pairs<key, value> =
            List<Pair<key, value>>

        pub opaque type AssocList<key, value> {
            inner: Pairs<key, value>,
        }

        pub fn new() -> AssocList<key, value> {
            AssocList { inner: [] }
        }

        pub fn to_list(m: AssocList<key, value>) -> Pairs<key, value> {
            m.inner
        }

        pub fn insert(
            in m: AssocList<key, value>,
            key k: key,
            value v: value,
        ) -> AssocList<key, value> {
            AssocList { inner: do_insert(m.inner, k, v) }
        }

        fn do_insert(elems: Pairs<key, value>, k: key, v: value) -> Pairs<key, value> {
            when elems is {
            [] ->
                [Pair(k, v)]
            [Pair(k2, v2), ..rest] ->
                if k == k2 {
                  [Pair(k, v), ..rest]
                } else {
                  [Pair(k2, v2), ..do_insert(rest, k, v)]
                }
            }
        }

        fn fixture_1() {
            new()
            |> insert("foo", 42)
            |> insert("bar", 14)
        }

        test to_list_2() {
            to_list(fixture_1()) == [Pair("foo", 42), Pair("bar", 14)]
        }
    "#;

    let do_insert = Term::var("elems")
        .delayed_choose_list(
            Term::mk_cons()
                .apply(
                    Term::mk_pair_data()
                        .apply(Term::b_data().apply(Term::var("k")))
                        .apply(Term::i_data().apply(Term::var("v"))),
                )
                .apply(Term::empty_map()),
            Term::head_list()
                .apply(Term::var("elems"))
                .as_var("elem_0", |elem_0| {
                    Term::tail_list()
                        .apply(Term::var("elems"))
                        .as_var("rest", |rest| {
                            Term::un_b_data()
                                .apply(Term::fst_pair().apply(Term::Var(elem_0.clone())))
                                .as_var("k2", |k2| {
                                    Term::un_i_data()
                                        .apply(Term::snd_pair().apply(Term::Var(elem_0.clone())))
                                        .as_var("v2", |v2| {
                                            Term::equals_bytestring()
                                                .apply(Term::var("k"))
                                                .apply(Term::Var(k2.clone()))
                                                .delayed_if_then_else(
                                                    Term::mk_cons()
                                                        .apply(
                                                            Term::mk_pair_data()
                                                                .apply(
                                                                    Term::b_data()
                                                                        .apply(Term::var("k")),
                                                                )
                                                                .apply(
                                                                    Term::i_data()
                                                                        .apply(Term::var("v")),
                                                                ),
                                                        )
                                                        .apply(Term::Var(rest.clone())),
                                                    Term::mk_cons()
                                                        .apply(
                                                            Term::mk_pair_data()
                                                                .apply(
                                                                    Term::b_data()
                                                                        .apply(Term::Var(k2)),
                                                                )
                                                                .apply(
                                                                    Term::i_data()
                                                                        .apply(Term::Var(v2)),
                                                                ),
                                                        )
                                                        .apply(
                                                            Term::var("do_insert")
                                                                .apply(Term::var("do_insert"))
                                                                .apply(Term::Var(rest)),
                                                        ),
                                                )
                                        })
                                })
                        })
                }),
        )
        .lambda("elems")
        .lambda("do_insert");

    let insert = do_insert
        .as_var("do_insert", |do_insert| {
            Term::Var(do_insert.clone())
                .apply(Term::Var(do_insert))
                .apply(Term::var("m"))
        })
        .lambda("v")
        .lambda("k")
        .lambda("m");

    let uplc = Term::equals_data()
        .apply(
            Term::map_data().apply(
                insert
                    .as_var("insert", |insert| {
                        Term::Var(insert.clone())
                            .apply(
                                Term::Var(insert)
                                    .apply(Term::empty_map())
                                    .apply(Term::byte_string("foo".as_bytes().to_vec()))
                                    .apply(Term::integer(42.into())),
                            )
                            .apply(Term::byte_string("bar".as_bytes().to_vec()))
                            .apply(Term::integer(14.into()))
                            .delay()
                    })
                    .force(),
            ),
        )
        .apply(Term::map_data().apply(Term::map_values(vec![
            Constant::ProtoPair(
                Type::Data,
                Type::Data,
                Constant::Data(Data::bytestring("foo".as_bytes().to_vec())).into(),
                Constant::Data(Data::integer(42.into())).into(),
            ),
            Constant::ProtoPair(
                Type::Data,
                Type::Data,
                Constant::Data(Data::bytestring("bar".as_bytes().to_vec())).into(),
                Constant::Data(Data::integer(14.into())).into(),
            ),
        ])));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_24_map_pair() {
    let src = r#"
      pub fn map2(
        opt_a: Option<a>,
        opt_b: Option<b>,
        f: fn(a, b) -> result,
      ) -> Option<result> {
        when opt_a is {
          None ->
            None
          Some(a) ->
            when opt_b is {
              None ->
                None
              Some(b) ->
                Some(f(a, b))
            }
        }
      }

      test map2_3() {
        map2(Some(14), Some(42), fn(a, b) { Pair(a, b) }) == Some(Pair(14, 42))
      }
    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::var("map2")
                .lambda("map2")
                .apply(
                    Term::equals_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("opt_a_index"))
                        .delayed_if_then_else(
                            Term::Constant(Constant::Data(Data::constr(1, vec![])).into()),
                            Term::equals_integer()
                                .apply(Term::integer(1.into()))
                                .apply(Term::var("opt_b_index"))
                                .delayed_if_then_else(
                                    Term::Constant(Constant::Data(Data::constr(1, vec![])).into()),
                                    Term::constr_data()
                                        .apply(Term::integer(0.into()))
                                        .apply(
                                            Term::mk_cons()
                                                .apply(
                                                    Term::list_data()
                                                        .apply(
                                                            Term::mk_cons()
                                                                .apply(
                                                                    Term::fst_pair()
                                                                        .apply(Term::var("pair")),
                                                                )
                                                                .apply(
                                                                    Term::mk_cons()
                                                                        .apply(
                                                                            Term::snd_pair().apply(
                                                                                Term::var("pair"),
                                                                            ),
                                                                        )
                                                                        .apply(Term::empty_list()),
                                                                ),
                                                        )
                                                        .lambda("pair")
                                                        .apply(
                                                            Term::var("f")
                                                                .apply(Term::var("a"))
                                                                .apply(Term::var("b")),
                                                        ),
                                                )
                                                .apply(Term::empty_list()),
                                        )
                                        .lambda("b")
                                        .apply(Term::un_i_data().apply(
                                            Term::head_list().apply(Term::var("opt_b_fields")),
                                        ))
                                        .lambda("opt_b_fields")
                                        .apply(
                                            Term::var(CONSTR_FIELDS_EXPOSER)
                                                .apply(Term::var("opt_b")),
                                        ),
                                )
                                .lambda("opt_b_index")
                                .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("opt_b")))
                                .lambda("a")
                                .apply(
                                    Term::un_i_data()
                                        .apply(Term::head_list().apply(Term::var("opt_a_fields"))),
                                )
                                .lambda("opt_a_fields")
                                .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("opt_a"))),
                        )
                        .lambda("opt_a_index")
                        .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("opt_a")))
                        .lambda("f")
                        .lambda("opt_b")
                        .lambda("opt_a"),
                )
                .apply(Term::Constant(
                    Constant::Data(Data::constr(0, vec![Data::integer(14.into())])).into(),
                ))
                .apply(Term::Constant(
                    Constant::Data(Data::constr(0, vec![Data::integer(42.into())])).into(),
                ))
                .apply(
                    Term::mk_pair_data()
                        .apply(Term::i_data().apply(Term::var("a")))
                        .apply(Term::i_data().apply(Term::var("b")))
                        .lambda("b")
                        .lambda("a"),
                ),
        )
        .apply(Term::Constant(
            Constant::Data(Data::constr(
                0,
                vec![Data::list(vec![
                    Data::integer(14.into()),
                    Data::integer(42.into()),
                ])],
            ))
            .into(),
        ))
        .constr_fields_exposer()
        .constr_index_exposer();

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_24_map2() {
    let src = r#"
      pub fn map2(
        opt_a: Option<a>,
        opt_b: Option<b>,
        f: fn(a, b) -> result,
      ) -> Option<result> {
        when opt_a is {
          None ->
            None
          Some(a) ->
            when opt_b is {
              None ->
                None
              Some(b) ->
                Some(f(a, b))
            }
        }
      }

      test map2_3() {
        map2(Some(14), Some(42), fn(a, b) { (a, b) }) == Some((14, 42))
      }
    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::var("map2")
                .lambda("map2")
                .apply(
                    Term::equals_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("opt_a_index"))
                        .delayed_if_then_else(
                            Term::Constant(Constant::Data(Data::constr(1, vec![])).into()),
                            Term::equals_integer()
                                .apply(Term::integer(1.into()))
                                .apply(Term::var("opt_b_index"))
                                .delayed_if_then_else(
                                    Term::Constant(Constant::Data(Data::constr(1, vec![])).into()),
                                    Term::constr_data()
                                        .apply(Term::integer(0.into()))
                                        .apply(
                                            Term::mk_cons()
                                                .apply(
                                                    Term::list_data().apply(
                                                        Term::var("f")
                                                            .apply(Term::var("a"))
                                                            .apply(Term::var("b")),
                                                    ),
                                                )
                                                .apply(Term::empty_list()),
                                        )
                                        .lambda("b")
                                        .apply(Term::un_i_data().apply(
                                            Term::head_list().apply(Term::var("opt_b_fields")),
                                        ))
                                        .lambda("opt_b_fields")
                                        .apply(
                                            Term::var(CONSTR_FIELDS_EXPOSER)
                                                .apply(Term::var("opt_b")),
                                        ),
                                )
                                .lambda("opt_b_index")
                                .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("opt_b")))
                                .lambda("a")
                                .apply(
                                    Term::un_i_data()
                                        .apply(Term::head_list().apply(Term::var("opt_a_fields"))),
                                )
                                .lambda("opt_a_fields")
                                .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("opt_a"))),
                        )
                        .lambda("opt_a_index")
                        .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("opt_a")))
                        .lambda("f")
                        .lambda("opt_b")
                        .lambda("opt_a"),
                )
                .apply(Term::Constant(
                    Constant::Data(Data::constr(0, vec![Data::integer(14.into())])).into(),
                ))
                .apply(Term::Constant(
                    Constant::Data(Data::constr(0, vec![Data::integer(42.into())])).into(),
                ))
                .apply(
                    Term::mk_cons()
                        .apply(Term::i_data().apply(Term::var("a")))
                        .apply(
                            Term::mk_cons()
                                .apply(Term::i_data().apply(Term::var("b")))
                                .apply(Term::empty_list()),
                        )
                        .lambda("b")
                        .lambda("a"),
                ),
        )
        .apply(Term::Constant(
            Constant::Data(Data::constr(
                0,
                vec![Data::list(vec![
                    Data::integer(14.into()),
                    Data::integer(42.into()),
                ])],
            ))
            .into(),
        ))
        .constr_fields_exposer()
        .constr_index_exposer();

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_25_void_equal() {
    let src = r#"
      test nil_1() {
        Void == Void
      }
    "#;

    let uplc = Term::unit().choose_unit(Term::unit().choose_unit(Term::bool(true)));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_26_foldr() {
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

      pub fn flat_map(xs: List<a>, f: fn(a) -> List<b>) -> List<b> {
        when xs is {
          [] ->
            []
          [x, ..rest] ->
            concat(f(x), flat_map(rest, f))
        }
      }

      test flat_map_2() {
        flat_map([1, 2, 3], fn(a) { [a, a] }) == [1, 1, 2, 2, 3, 3]
      }
    "#;

    let uplc =
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("flat_map")
                        .lambda("flat_map")
                        .apply(
                            Term::var("flat_map")
                                .apply(Term::var("flat_map"))
                                .apply(Term::var("xs"))
                                .lambda("flat_map")
                                .apply(
                                    Term::var("xs")
                                        .delayed_choose_list(
                                            Term::empty_list(),
                                            Term::var("concat")
                                                .apply(Term::var("f").apply(Term::var("x")))
                                                .apply(
                                                    Term::var("flat_map")
                                                        .apply(Term::var("flat_map"))
                                                        .apply(Term::var("rest")),
                                                )
                                                .lambda("rest")
                                                .apply(Term::tail_list().apply(Term::var("xs")))
                                                .lambda("x")
                                                .apply(Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("xs")),
                                                )),
                                        )
                                        .lambda("xs")
                                        .lambda("flat_map"),
                                )
                                .lambda("f")
                                .lambda("xs"),
                        )
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
                        .apply(
                            Term::var("foldr")
                                .apply(Term::var("foldr"))
                                .apply(Term::var("xs"))
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
                                                        .apply(Term::var("rest")),
                                                )
                                                .lambda("rest")
                                                .apply(Term::tail_list().apply(Term::var("xs")))
                                                .lambda("x")
                                                .apply(Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("xs")),
                                                )),
                                        )
                                        .lambda("xs")
                                        .lambda("foldr"),
                                )
                                .lambda("zero")
                                .lambda("f")
                                .lambda("xs"),
                        )
                        .apply(Term::list_values(vec![
                            Constant::Data(Data::integer(1.into())),
                            Constant::Data(Data::integer(2.into())),
                            Constant::Data(Data::integer(3.into())),
                        ]))
                        .apply(
                            Term::mk_cons()
                                .apply(Term::i_data().apply(Term::var("a")))
                                .apply(
                                    Term::mk_cons()
                                        .apply(Term::i_data().apply(Term::var("a")))
                                        .apply(Term::empty_list()),
                                )
                                .lambda("a"),
                        ),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(3.into())),
                Constant::Data(Data::integer(3.into())),
            ])));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_27_flat_map() {
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

      pub fn flat_map(xs: List<a>, f: fn(a) -> List<b>) -> List<b> {
        when xs is {
          [] ->
            []
          [x, ..rest] ->
            concat(f(x), flat_map(rest, f))
        }
      }

      test flat_map_2() {
        flat_map([1, 2, 3], fn(a) { [a, a] }) == [1, 1, 2, 2, 3, 3]
      }
    "#;

    let uplc =
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("flat_map")
                        .lambda("flat_map")
                        .apply(
                            Term::var("flat_map")
                                .apply(Term::var("flat_map"))
                                .apply(Term::var("xs"))
                                .lambda("flat_map")
                                .apply(
                                    Term::var("xs")
                                        .delayed_choose_list(
                                            Term::empty_list(),
                                            Term::var("concat")
                                                .apply(Term::var("f").apply(Term::var("x")))
                                                .apply(
                                                    Term::var("flat_map")
                                                        .apply(Term::var("flat_map"))
                                                        .apply(Term::var("rest")),
                                                )
                                                .lambda("rest")
                                                .apply(Term::tail_list().apply(Term::var("xs")))
                                                .lambda("x")
                                                .apply(Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("xs")),
                                                )),
                                        )
                                        .lambda("xs")
                                        .lambda("flat_map"),
                                )
                                .lambda("f")
                                .lambda("xs"),
                        )
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
                        .apply(
                            Term::var("foldr")
                                .apply(Term::var("foldr"))
                                .apply(Term::var("xs"))
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
                                                        .apply(Term::var("rest")),
                                                )
                                                .lambda("rest")
                                                .apply(Term::tail_list().apply(Term::var("xs")))
                                                .lambda("x")
                                                .apply(Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("xs")),
                                                )),
                                        )
                                        .lambda("xs")
                                        .lambda("foldr"),
                                )
                                .lambda("zero")
                                .lambda("f")
                                .lambda("xs"),
                        )
                        .apply(Term::list_values(vec![
                            Constant::Data(Data::integer(1.into())),
                            Constant::Data(Data::integer(2.into())),
                            Constant::Data(Data::integer(3.into())),
                        ]))
                        .apply(
                            Term::mk_cons()
                                .apply(Term::i_data().apply(Term::var("a")))
                                .apply(
                                    Term::mk_cons()
                                        .apply(Term::i_data().apply(Term::var("a")))
                                        .apply(Term::empty_list()),
                                )
                                .lambda("a"),
                        ),
                ),
            )
            .apply(Term::list_data().apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(3.into())),
                Constant::Data(Data::integer(3.into())),
            ])));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_28_unique_empty_list() {
    let src = r#"
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

      pub fn unique(xs: List<a>) -> List<a> {
        when xs is {
          [] ->
            []
          [x, ..rest] ->
            [x, ..unique(filter(rest, fn(y) { y != x }))]
        }
      }

      test unique_1() {
        unique([]) == []
      }
    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::list_data().apply(
                Term::var("unique")
                    .lambda("unique")
                    .apply(Term::var("unique").apply(Term::var("unique")))
                    .lambda("unique")
                    .apply(
                        Term::var("xs")
                            .delayed_choose_list(
                                Term::empty_list(),
                                Term::mk_cons()
                                    .apply(Term::var("x"))
                                    .apply(
                                        Term::var("unique").apply(Term::var("unique")).apply(
                                            Term::var("filter").apply(Term::var("rest")).apply(
                                                Term::equals_data()
                                                    .apply(Term::var("y"))
                                                    .apply(Term::var("x"))
                                                    .if_then_else(
                                                        Term::bool(false),
                                                        Term::bool(true),
                                                    )
                                                    .lambda("y"),
                                            ),
                                        ),
                                    )
                                    .lambda("rest")
                                    .apply(Term::tail_list().apply(Term::var("xs")))
                                    .lambda("x")
                                    .apply(Term::head_list().apply(Term::var("xs"))),
                            )
                            .lambda("xs")
                            .lambda("unique"),
                    )
                    .lambda("filter")
                    .apply(
                        Term::var("filter")
                            .apply(Term::var("filter"))
                            .apply(Term::var("xs"))
                            .lambda("filter")
                            .apply(
                                Term::var("xs")
                                    .delayed_choose_list(
                                        Term::empty_list(),
                                        Term::var("f")
                                            .apply(Term::var("x"))
                                            .delayed_if_then_else(
                                                Term::mk_cons().apply(Term::var("x")).apply(
                                                    Term::var("filter")
                                                        .apply(Term::var("filter"))
                                                        .apply(Term::var("rest")),
                                                ),
                                                Term::var("filter")
                                                    .apply(Term::var("filter"))
                                                    .apply(Term::var("rest")),
                                            )
                                            .lambda("rest")
                                            .apply(Term::tail_list().apply(Term::var("xs")))
                                            .lambda("x")
                                            .apply(Term::head_list().apply(Term::var("xs"))),
                                    )
                                    .lambda("xs")
                                    .lambda("filter"),
                            )
                            .lambda("f")
                            .lambda("xs"),
                    )
                    .apply(Term::empty_list()),
            ),
        )
        .apply(Term::data(Data::list(vec![])));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_28_unique_list() {
    let src = r#"
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

      pub fn unique(xs: List<a>) -> List<a> {
        when xs is {
          [] ->
            []
          [x, ..rest] ->
            [x, ..unique(filter(rest, fn(y) { y != x }))]
        }
      }

      test unique_1() {
        unique([1,2,3,1]) == [1,2,3]
      }
    "#;

    let uplc =
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("unique")
                        .lambda("unique")
                        .apply(Term::var("unique").apply(Term::var("unique")))
                        .lambda("unique")
                        .apply(
                            Term::var("xs")
                                .delayed_choose_list(
                                    Term::empty_list(),
                                    Term::mk_cons()
                                        .apply(Term::i_data().apply(Term::var("x")))
                                        .apply(
                                            Term::var("unique").apply(Term::var("unique")).apply(
                                                Term::var("filter").apply(Term::var("rest")).apply(
                                                    Term::equals_integer()
                                                        .apply(Term::var("y"))
                                                        .apply(Term::var("x"))
                                                        .if_then_else(
                                                            Term::bool(false),
                                                            Term::bool(true),
                                                        )
                                                        .lambda("y"),
                                                ),
                                            ),
                                        )
                                        .lambda("rest")
                                        .apply(Term::tail_list().apply(Term::var("xs")))
                                        .lambda("x")
                                        .apply(
                                            Term::un_i_data()
                                                .apply(Term::head_list().apply(Term::var("xs"))),
                                        ),
                                )
                                .lambda("xs")
                                .lambda("unique"),
                        )
                        .lambda("filter")
                        .apply(
                            Term::var("filter")
                                .apply(Term::var("filter"))
                                .apply(Term::var("xs"))
                                .lambda("filter")
                                .apply(
                                    Term::var("xs")
                                        .delayed_choose_list(
                                            Term::empty_list(),
                                            Term::var("f")
                                                .apply(Term::var("x"))
                                                .delayed_if_then_else(
                                                    Term::mk_cons()
                                                        .apply(Term::i_data().apply(Term::var("x")))
                                                        .apply(
                                                            Term::var("filter")
                                                                .apply(Term::var("filter"))
                                                                .apply(Term::var("rest")),
                                                        ),
                                                    Term::var("filter")
                                                        .apply(Term::var("filter"))
                                                        .apply(Term::var("rest")),
                                                )
                                                .lambda("rest")
                                                .apply(Term::tail_list().apply(Term::var("xs")))
                                                .lambda("x")
                                                .apply(Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("xs")),
                                                )),
                                        )
                                        .lambda("xs")
                                        .lambda("filter"),
                                )
                                .lambda("f")
                                .lambda("xs"),
                        )
                        .apply(Term::list_values(vec![
                            Constant::Data(Data::integer(1.into())),
                            Constant::Data(Data::integer(2.into())),
                            Constant::Data(Data::integer(3.into())),
                            Constant::Data(Data::integer(1.into())),
                        ])),
                ),
            )
            .apply(Term::data(Data::list(vec![
                Data::integer(1.into()),
                Data::integer(2.into()),
                Data::integer(3.into()),
            ])));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_29_union_pair() {
    let src = r#"
      pub opaque type AssocList<key, value> {
        inner: Pairs<key, value>,
      }

      const empty_list: AssocList<key, value> = AssocList { inner: [] }

      pub fn from_list(xs: Pairs<key, value>) -> AssocList<key, value> {
        AssocList { inner: do_from_list(xs) }
      }

      fn do_from_list(xs: Pairs<key, value>) -> Pairs<key, value> {
        when xs is {
          [] ->
            []
          [Pair(k, v), ..rest] ->
            do_insert(do_from_list(rest), k, v)
        }
      }

      pub fn insert(
        in m: AssocList<key, value>,
        key k: key,
        value v: value,
      ) -> AssocList<key, value> {
        AssocList { inner: do_insert(m.inner, k, v) }
      }

      fn do_insert(elems: Pairs<key, value>, k: key, v: value) -> Pairs<key, value> {
        when elems is {
          [] ->
            [Pair(k, v)]
          [Pair(k2, v2), ..rest] ->
            if k == k2 {
              [Pair(k, v), ..rest]
            } else {
              [Pair(k2, v2), ..do_insert(rest, k, v)]
            }
        }
      }

      pub fn union(
        left: AssocList<key, value>,
        right: AssocList<key, value>,
      ) -> AssocList<key, value> {
        AssocList { inner: do_union(left.inner, right.inner) }
      }

      fn do_union(
        left: Pairs<key, value>,
        right: Pairs<key, value>,
      ) -> Pairs<key, value> {
        when left is {
          [] ->
            right
          [Pair(k, v), ..rest] ->
            do_union(rest, do_insert(right, k, v))
        }
      }

      const fixture_1 = {
        empty_list
          |> insert("foo", 42)
          |> insert("bar", 14)
      }

      test union_1() {
        union(fixture_1, empty_list) == fixture_1
      }

    "#;

    let do_insert = Term::var("elems")
        .delayed_choose_list(
            Term::mk_cons()
                .apply(
                    Term::mk_pair_data()
                        .apply(Term::b_data().apply(Term::var("k")))
                        .apply(Term::i_data().apply(Term::var("v"))),
                )
                .apply(Term::empty_map()),
            Term::head_list()
                .apply(Term::var("elems"))
                .as_var("elem_0", |elem_0| {
                    Term::tail_list()
                        .apply(Term::var("elems"))
                        .as_var("rest", |rest| {
                            Term::un_b_data()
                                .apply(Term::fst_pair().apply(Term::Var(elem_0.clone())))
                                .as_var("k2", |k2| {
                                    Term::un_i_data()
                                        .apply(Term::snd_pair().apply(Term::Var(elem_0.clone())))
                                        .as_var("v2", |v2| {
                                            Term::equals_bytestring()
                                                .apply(Term::var("k"))
                                                .apply(Term::Var(k2.clone()))
                                                .delayed_if_then_else(
                                                    Term::mk_cons()
                                                        .apply(
                                                            Term::mk_pair_data()
                                                                .apply(
                                                                    Term::b_data()
                                                                        .apply(Term::var("k")),
                                                                )
                                                                .apply(
                                                                    Term::i_data()
                                                                        .apply(Term::var("v")),
                                                                ),
                                                        )
                                                        .apply(Term::Var(rest.clone())),
                                                    Term::mk_cons()
                                                        .apply(
                                                            Term::mk_pair_data()
                                                                .apply(
                                                                    Term::b_data()
                                                                        .apply(Term::Var(k2)),
                                                                )
                                                                .apply(
                                                                    Term::i_data()
                                                                        .apply(Term::Var(v2)),
                                                                ),
                                                        )
                                                        .apply(
                                                            Term::var("do_insert")
                                                                .apply(Term::var("do_insert"))
                                                                .apply(Term::Var(rest)),
                                                        ),
                                                )
                                        })
                                })
                        })
                }),
        )
        .lambda("elems")
        .lambda("do_insert");

    let do_insert_recurse = do_insert
        .as_var("do_insert", |do_insert| {
            Term::Var(do_insert.clone())
                .apply(Term::Var(do_insert))
                .apply(Term::var("elems"))
        })
        .lambda("v")
        .lambda("k")
        .lambda("elems");

    let insert = Term::var("do_insert")
        .apply(Term::var("m"))
        .apply(Term::var("k"))
        .apply(Term::var("v"))
        .lambda("v")
        .lambda("k")
        .lambda("m");

    let empty_list = Term::empty_map();

    let fixture = Term::data(Data::map(vec![
        (
            Data::bytestring(vec![0x66, 0x6f, 0x6f]),
            Data::integer(42.into()),
        ),
        (
            Data::bytestring(vec![0x62, 0x61, 0x72]),
            Data::integer(14.into()),
        ),
    ]));

    let fixture_unwrapped = Term::Constant(
        Constant::ProtoList(
            Type::Pair(Type::Data.into(), Type::Data.into()),
            vec![
                Constant::ProtoPair(
                    Type::Data,
                    Type::Data,
                    Constant::Data(Data::bytestring(vec![0x66, 0x6f, 0x6f])).into(),
                    Constant::Data(Data::integer(42.into())).into(),
                ),
                Constant::ProtoPair(
                    Type::Data,
                    Type::Data,
                    Constant::Data(Data::bytestring(vec![0x62, 0x61, 0x72])).into(),
                    Constant::Data(Data::integer(14.into())).into(),
                ),
            ],
        )
        .into(),
    );

    let do_union = Term::var("left")
        .delayed_choose_list(
            Term::var("right"),
            Term::head_list()
                .apply(Term::var("left"))
                .as_var("elem_0", |elem_0| {
                    Term::var("do_union")
                        .apply(Term::var("do_union"))
                        .apply(Term::tail_list().apply(Term::var("left")))
                        .apply(
                            Term::var("do_insert")
                                .apply(Term::var("right"))
                                .apply(
                                    Term::un_b_data()
                                        .apply(Term::fst_pair().apply(Term::Var(elem_0.clone()))),
                                )
                                .apply(
                                    Term::un_i_data()
                                        .apply(Term::snd_pair().apply(Term::Var(elem_0))),
                                ),
                        )
                }),
        )
        .lambda("right")
        .lambda("left")
        .lambda("do_union");

    let uplc = Term::equals_data()
        .apply(
            Term::map_data().apply(do_union.as_var("do_union", |do_union| {
                Term::Var(do_union.clone())
                    .apply(Term::Var(do_union))
                    .apply(fixture_unwrapped)
                    .apply(empty_list)
            })),
        )
        .apply(fixture)
        .lambda("insert")
        .apply(insert)
        .lambda("do_insert")
        .apply(do_insert_recurse);

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_29_union_tuple() {
    let src = r#"
      pub opaque type AssocList<key, value> {
        inner: List<(key, value)>,
      }

      const empty_list = AssocList { inner: [] }

      pub fn from_list(xs: List<(key, value)>) -> AssocList<key, value> {
        AssocList { inner: do_from_list(xs) }
      }

      fn do_from_list(xs: List<(key, value)>) -> List<(key, value)> {
        when xs is {
          [] ->
            []
          [(k, v), ..rest] ->
            do_insert(do_from_list(rest), k, v)
        }
      }

      pub fn insert(
        in m: AssocList<key, value>,
        key k: key,
        value v: value,
      ) -> AssocList<key, value> {
        AssocList { inner: do_insert(m.inner, k, v) }
      }

      fn do_insert(elems: List<(key, value)>, k: key, v: value) -> List<(key, value)> {
        when elems is {
          [] ->
            [(k, v)]
          [(k2, v2), ..rest] ->
            if k == k2 {
              [(k, v), ..rest]
            } else {
              [(k2, v2), ..do_insert(rest, k, v)]
            }
        }
      }

      pub fn union(
        left: AssocList<key, value>,
        right: AssocList<key, value>,
      ) -> AssocList<key, value> {
        AssocList { inner: do_union(left.inner, right.inner) }
      }

      fn do_union(
        left: List<(key, value)>,
        right: List<(key, value)>,
      ) -> List<(key, value)> {
        when left is {
          [] ->
            right
          [(k, v), ..rest] ->
            do_union(rest, do_insert(right, k, v))
        }
      }

      const fixture_1 = {
        empty_list
          |> insert("foo", 42)
          |> insert("bar", 14)
      }

      test union_1() {
        union(fixture_1, empty_list) == fixture_1
      }

    "#;

    let uplc = Term::equals_data()
        .apply(
            Term::list_data().apply(
                Term::var("union")
                    .lambda("union")
                    .apply(
                        Term::var("do_union")
                            .apply(Term::var("left"))
                            .apply(Term::var("right"))
                            .lambda("right")
                            .lambda("left"),
                    )
                    .lambda("do_union")
                    .apply(Term::var("do_union").apply(Term::var("do_union")))
                    .lambda("do_union")
                    .apply(
                        Term::var("left")
                            .delayed_choose_list(
                                Term::var("right"),
                                Term::var("do_union")
                                    .apply(Term::var("do_union"))
                                    .apply(Term::var("rest"))
                                    .apply(
                                        Term::var("do_insert")
                                            .apply(Term::var("right"))
                                            .apply(Term::var("k"))
                                            .apply(Term::var("v")),
                                    )
                                    .lambda("v")
                                    .apply(
                                        Term::un_i_data()
                                            .apply(Term::head_list().apply(
                                                Term::tail_list().apply(Term::var("tuple")),
                                            )),
                                    )
                                    .lambda("k")
                                    .apply(
                                        Term::un_b_data()
                                            .apply(Term::head_list().apply(Term::var("tuple"))),
                                    )
                                    .lambda("rest")
                                    .apply(Term::tail_list().apply(Term::var("left")))
                                    .lambda("tuple")
                                    .apply(
                                        Term::unlist_data()
                                            .apply(Term::head_list().apply(Term::var("left"))),
                                    ),
                            )
                            .lambda("right")
                            .lambda("left")
                            .lambda("do_union"),
                    )
                    .lambda("do_insert")
                    .apply(
                        Term::var("do_insert")
                            .apply(Term::var("do_insert"))
                            .apply(Term::var("elems"))
                            .lambda("do_insert")
                            .apply(
                                Term::var("elems")
                                    .delayed_choose_list(
                                        Term::mk_cons()
                                            .apply(
                                                Term::list_data().apply(
                                                    Term::mk_cons()
                                                        .apply(Term::b_data().apply(Term::var("k")))
                                                        .apply(
                                                            Term::mk_cons()
                                                                .apply(
                                                                    Term::i_data()
                                                                        .apply(Term::var("v")),
                                                                )
                                                                .apply(Term::empty_list()),
                                                        ),
                                                ),
                                            )
                                            .apply(Term::empty_list()),
                                        Term::equals_bytestring()
                                            .apply(Term::var("k"))
                                            .apply(Term::var("k2"))
                                            .delayed_if_then_else(
                                                Term::mk_cons()
                                                    .apply(
                                                        Term::list_data().apply(
                                                            Term::mk_cons()
                                                                .apply(
                                                                    Term::b_data()
                                                                        .apply(Term::var("k")),
                                                                )
                                                                .apply(
                                                                    Term::mk_cons()
                                                                        .apply(
                                                                            Term::i_data().apply(
                                                                                Term::var("v"),
                                                                            ),
                                                                        )
                                                                        .apply(Term::empty_list()),
                                                                ),
                                                        ),
                                                    )
                                                    .apply(Term::var("rest")),
                                                Term::mk_cons()
                                                    .apply(
                                                        Term::list_data().apply(
                                                            Term::mk_cons()
                                                                .apply(
                                                                    Term::b_data()
                                                                        .apply(Term::var("k2")),
                                                                )
                                                                .apply(
                                                                    Term::mk_cons()
                                                                        .apply(
                                                                            Term::i_data().apply(
                                                                                Term::var("v2"),
                                                                            ),
                                                                        )
                                                                        .apply(Term::empty_list()),
                                                                ),
                                                        ),
                                                    )
                                                    .apply(
                                                        Term::var("do_insert")
                                                            .apply(Term::var("do_insert"))
                                                            .apply(Term::var("rest")),
                                                    ),
                                            )
                                            .lambda("v2")
                                            .apply(Term::un_i_data().apply(
                                                Term::head_list().apply(
                                                    Term::tail_list().apply(Term::var("tuple")),
                                                ),
                                            ))
                                            .lambda("k2")
                                            .apply(
                                                Term::un_b_data().apply(
                                                    Term::head_list().apply(Term::var("tuple")),
                                                ),
                                            )
                                            .lambda("rest")
                                            .apply(Term::tail_list().apply(Term::var("elems")))
                                            .lambda("tuple")
                                            .apply(Term::unlist_data().apply(
                                                Term::head_list().apply(Term::var("elems")),
                                            )),
                                    )
                                    .lambda("elems")
                                    .lambda("do_insert"),
                            )
                            .lambda("v")
                            .lambda("k")
                            .lambda("elems"),
                    )
                    .apply(Term::list_values(vec![
                        Constant::Data(Data::list(vec![
                            Data::bytestring("foo".as_bytes().to_vec()),
                            Data::integer(42.into()),
                        ])),
                        Constant::Data(Data::list(vec![
                            Data::bytestring("bar".as_bytes().to_vec()),
                            Data::integer(14.into()),
                        ])),
                    ]))
                    .apply(Term::empty_list()),
            ),
        )
        .apply(Term::data(Data::list(vec![
            Data::list(vec![
                Data::bytestring("foo".as_bytes().to_vec()),
                Data::integer(42.into()),
            ]),
            Data::list(vec![
                Data::bytestring("bar".as_bytes().to_vec()),
                Data::integer(14.into()),
            ]),
        ])));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
}

#[test]
fn acceptance_test_30_abs() {
    let src = r#"
      pub fn abs(a: Int) -> Int {
        if a < 0 {
          -a
        } else {
          a
        }
      }

      test abs_1() {
        abs(-14) == 14
      }
    "#;

    let uplc = Term::equals_integer()
        .apply(
            Term::var("abs")
                .lambda("abs")
                .apply(
                    Term::less_than_integer()
                        .apply(Term::var("a"))
                        .apply(Term::integer(0.into()))
                        .delayed_if_then_else(
                            Term::subtract_integer()
                                .apply(Term::integer(0.into()))
                                .apply(Term::var("a")),
                            Term::var("a"),
                        )
                        .lambda("a"),
                )
                .apply(Term::integer((-14).into())),
        )
        .apply(Term::integer(14.into()));

    assert_uplc(src, uplc.clone(), false, true);
    assert_uplc(src, uplc, false, false);
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
                Term::Error.delayed_trace(Term::string("expect [] = x")),
            )
            .lambda("x")
            .apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
            ])),
        true,
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
                Term::Error.delayed_trace(Term::string("expect [] = x")),
            )
            .lambda("x")
            .apply(Term::list_values(vec![])),
        false,
        true,
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
            fail
        }
      }
    "#;

    assert_uplc(
        src,
        Term::var("subject")
            .delayed_if_then_else(Term::bool(true), Term::Error)
            .lambda("subject")
            .apply(Term::bool(true)),
        false,
        true,
    );
}

#[test]
fn when_bool_is_true_switched_cases() {
    let src = r#"
      test it() {
        when True is {
          False ->
            fail
          True ->
            True
        }
      }
    "#;

    assert_uplc(
        src,
        Term::var("subject")
            .delayed_if_then_else(Term::bool(true), Term::Error)
            .lambda("subject")
            .apply(Term::bool(true)),
        false,
        true,
    );
}

#[test]
fn when_bool_is_false() {
    let src = r#"
      test it() {
        when False is {
          False ->
            fail
          True ->
            True
        }
      }
    "#;

    assert_uplc(
        src,
        Term::var("subject")
            .delayed_if_then_else(Term::bool(true), Term::Error)
            .lambda("subject")
            .apply(Term::bool(false)),
        true,
        true,
    );
}

#[test]
fn always_true_validator() {
    let src = r#"
        validator foo {
            mint(_redeemer: Data, _policy_id: ByteArray, _transaction: Data) {
                True
            }
        }

    "#;

    let mint = |purpose: Rc<Name>| {
        Term::bool(true).lambda("__purpose_arg__").apply(
            Term::un_b_data()
                .apply(Term::head_list().apply(
                    Term::snd_pair().apply(Term::unconstr_data().apply(Term::Var(purpose))),
                )),
        )
    };

    let when = |purpose: Rc<Name>| {
        Term::equals_integer()
            .apply(Term::integer(0.into()))
            .apply(Term::fst_pair().apply(Term::unconstr_data().apply(Term::Var(purpose))))
    };

    let validator = {
        let context = "__context__";
        Term::snd_pair()
            .apply(Term::unconstr_data().apply(Term::var(context)))
            .as_var("tail_id_2", |tail_id_2| {
                Term::head_list()
                    .apply(Term::Var(tail_id_2.clone()))
                    .as_var("__transaction__", |_transaction| {
                        Term::tail_list().apply(Term::Var(tail_id_2)).as_var(
                            "tail_id_3",
                            |tail_id_3| {
                                Term::head_list()
                                    .apply(Term::Var(tail_id_3.clone()))
                                    .as_var("__redeemer__", |_redeemer| {
                                        Term::head_list()
                                            .apply(Term::tail_list().apply(Term::Var(tail_id_3)))
                                            .as_var("__purpose__", |purpose| {
                                                when(purpose.clone()).delayed_if_then_else(
                                                    mint(purpose),
                                                    Term::Error,
                                                )
                                            })
                                    })
                            },
                        )
                    })
            })
            .delayed_if_then_else(
                Term::unit(),
                Term::Error
                    .apply(Term::Error.force())
                    .delayed_trace(Term::string("Validator returned false")),
            )
            .lambda(context)
    };

    assert_uplc(src, validator, false, true);
}

#[test]
fn when_tuple_deconstruction() {
    let src = r#"
      pub type Thing {
        idx: Int,
      }

      pub type Datum {
        A(Thing)
        B
      }

      pub type RedSpend {
        Spend(Int)
        Buy
      }

      test foo() {
        when (A(Thing { idx: 42 }), Buy) is {
          (A(a), Spend(x)) ->
            (a.idx == x)?
          (_, _) ->
            True
        }
      }
    "#;

    let first_clause = |tuple_index_0: Rc<Name>, tuple_index_1: Rc<Name>, otherwise: Rc<Name>| {
        let match_a = Term::equals_integer().apply(Term::integer(0.into())).apply(
            Term::fst_pair().apply(Term::unconstr_data().apply(Term::Var(tuple_index_0.clone()))),
        );

        let extract_a = Term::head_list()
            .apply(Term::snd_pair().apply(Term::unconstr_data().apply(Term::Var(tuple_index_0))));

        match_a.delay_true_if_then_else(
            extract_a.as_var("a", |a| {
                let match_spend = Term::equals_integer().apply(Term::integer(0.into())).apply(
                    Term::fst_pair()
                        .apply(Term::unconstr_data().apply(Term::Var(tuple_index_1.clone()))),
                );

                match_spend.delay_true_if_then_else(
                    Term::equals_integer()
                        .apply(Term::un_i_data().apply(Term::head_list().apply(
                            Term::snd_pair().apply(Term::unconstr_data().apply(Term::Var(a))),
                        )))
                        .apply(
                            Term::un_i_data().apply(
                                Term::head_list().apply(
                                    Term::snd_pair().apply(
                                        Term::unconstr_data().apply(Term::Var(tuple_index_1)),
                                    ),
                                ),
                            ),
                        )
                        .delayed_if_then_else(
                            Term::bool(true),
                            Term::bool(false).delayed_trace(Term::string("a.idx == x ? False")),
                        ),
                    Term::Var(otherwise.clone()),
                )
            }),
            Term::Var(otherwise),
        )
    };

    let snd_clause = Term::bool(true);

    let subject = || {
        Term::list_values(vec![
            Constant::Data(Data::constr(
                0,
                vec![Data::constr(0, vec![Data::integer(42.into())])],
            )),
            Constant::Data(Data::constr(1, vec![])),
        ])
    };

    let test = Term::head_list()
        .apply(Term::tail_list().apply(subject()))
        .as_var("tuple_index_1_span_258_266", |tuple_index_1| {
            Term::head_list().apply(subject()).as_var(
                "__tuple_index_0_span_252_256",
                |tuple_index_0| {
                    snd_clause
                        .delay()
                        .as_var("__other_clause_delayed", |other_clause| {
                            first_clause(tuple_index_0, tuple_index_1, other_clause)
                        })
                },
            )
        });

    assert_uplc(src, test, false, true);
}

#[test]
fn when_tuple_empty_lists() {
    let src = r#"
    test hi() {
        let bucket1 = [1, 2, 3]
        let bucket2 = [1, 5, 6]
        let bye =
          when (bucket1, bucket2) is {
            ([], _) -> False
            (_, []) -> False
            ([a, ..], [b, ..]) -> a == b
          }
        bye
      }
    "#;

    assert_uplc(
        src,
        Term::var("bucket_tuple_fst")
            .delay_empty_choose_list(Term::bool(false), Term::var("delayed_clause"))
            .lambda("delayed_clause")
            .apply(
                Term::var("bucket_tuple_snd")
                    .delay_empty_choose_list(Term::bool(false), Term::var("delayed_clause"))
                    .lambda("delayed_clause")
                    .apply(
                        Term::equals_integer()
                            .apply(Term::var("a"))
                            .apply(Term::var("b"))
                            .lambda("b")
                            .apply(
                                Term::un_i_data()
                                    .apply(Term::head_list().apply(Term::var("bucket_tuple_snd"))),
                            )
                            .lambda("a")
                            .apply(
                                Term::un_i_data()
                                    .apply(Term::head_list().apply(Term::var("bucket_tuple_fst"))),
                            )
                            .delay(),
                    )
                    .lambda("bucket_tuple_snd")
                    .apply(Term::unlist_data().apply(
                        Term::head_list().apply(Term::tail_list().apply(Term::var("bucket_tuple"))),
                    ))
                    .delay(),
            )
            .lambda("bucket_tuple_fst")
            .apply(Term::unlist_data().apply(Term::head_list().apply(Term::var("bucket_tuple"))))
            .lambda("bucket_tuple")
            .apply(
                Term::mk_cons()
                    .apply(Term::list_data().apply(Term::var("bucket1")))
                    .apply(
                        Term::mk_cons()
                            .apply(Term::list_data().apply(Term::var("bucket2")))
                            .apply(Term::empty_list()),
                    ),
            )
            .lambda("bucket2")
            .apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(5.into())),
                Constant::Data(Data::integer(6.into())),
            ]))
            .lambda("bucket1")
            .apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(3.into())),
            ])),
        false,
        true,
    );
}

#[test]
fn generic_validator_type_test() {
    let src = r#"
      pub type A<x> {
        NoA
        SomeA(Void, x)
      }

      pub type B {
        something: Void,
      }

      validator err_example {
        spend(_datum: Option<Data>, r: A<B>, _output_ref: Data, _transaction: Data) -> Bool {
          when r is {
            NoA ->
              False
            SomeA(_, B(something)) ->
              something == Void
          }
        }
      }
    "#;

    let body = |redeemer: Rc<Name>| {
        Term::equals_integer()
            .apply(Term::integer(0.into()))
            .apply(Term::var("subject"))
            .delayed_if_then_else(
                Term::bool(false),
                Term::choose_unit(
                    Term::var("something"),
                    Term::choose_unit(Term::unit(), Term::bool(true)),
                )
                .lambda("something")
                .apply(
                    Term::unit()
                        .lambda("_")
                        .apply(Term::head_list().apply(Term::var("B_fields"))),
                )
                .lambda("B_fields")
                .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("field_B")))
                .lambda("field_B")
                .apply(Term::head_list().apply(Term::var("tail_1")))
                .lambda("tail_1")
                .apply(Term::tail_list().apply(Term::var("r_fields")))
                .lambda("r_fields")
                .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::Var(redeemer.clone()))),
            )
            .lambda("subject")
            .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::Var(redeemer)))
    };

    let expect_void = |target: Rc<Name>, then: Term<Name>| {
        Term::choose_data_constr(
            target.clone(),
            |_| {
                Term::equals_integer()
                    .apply(Term::integer(0.into()))
                    .apply(
                        Term::fst_pair()
                            .apply(Term::unconstr_data().apply(Term::Var(target.clone()))),
                    )
                    .delay_true_if_then_else(
                        Term::snd_pair()
                            .apply(Term::unconstr_data().apply(Term::Var(target)))
                            .delay_empty_choose_list(then, Term::var("r:A<B>")),
                        Term::var("r:A<B>"),
                    )
            },
            &Term::var("r:A<B>"),
        )
    };

    let expect_void_no_trace = |target: Term<Name>| {
        Term::equals_integer()
            .apply(Term::integer(0.into()))
            .apply(Term::fst_pair().apply(Term::unconstr_data().apply(target.clone())))
            .delayed_if_then_else(
                Term::snd_pair()
                    .apply(Term::unconstr_data().apply(target))
                    .delayed_choose_list(Term::unit(), Term::Error),
                Term::Error,
            )
    };

    let expect_no_a = |redeemer: Rc<Name>, then_delayed: Rc<Name>, trace: bool| {
        Term::snd_pair()
            .apply(Term::unconstr_data().apply(Term::Var(redeemer)))
            .delay_empty_choose_list(
                Term::Var(then_delayed.clone()).force(),
                if trace {
                    Term::var("r:A<B>")
                } else {
                    Term::Error.delay()
                },
            )
    };

    let expect_b = |target: Rc<Name>, then: Term<Name>, trace: bool| {
        let error_trace = if trace {
            Term::var("r:A<B>")
        } else {
            Term::Error.delay()
        };

        Term::equals_integer()
            .apply(Term::integer(0.into()))
            .apply(Term::fst_pair().apply(Term::unconstr_data().apply(Term::Var(target.clone()))))
            .delay_true_if_then_else(
                Term::snd_pair()
                    .apply(Term::unconstr_data().apply(Term::Var(target)))
                    .as_var("tail_id_8", |tail_id_8| {
                        if trace {
                            Term::Var(tail_id_8.clone()).delay_filled_choose_list(
                                error_trace.clone(),
                                Term::head_list()
                                    .apply(Term::Var(tail_id_8.clone()))
                                    .as_var("__val", |val| {
                                        expect_void(
                                            val,
                                            Term::tail_list()
                                                .apply(Term::Var(tail_id_8))
                                                .delay_empty_choose_list(
                                                    then.force(),
                                                    error_trace.clone(),
                                                ),
                                        )
                                    }),
                            )
                        } else {
                            Term::head_list()
                                .apply(Term::Var(tail_id_8.clone()))
                                .as_var("__val", |val| expect_void_no_trace(Term::Var(val)))
                                .as_var("something", |_| {
                                    Term::tail_list()
                                        .apply(Term::Var(tail_id_8))
                                        .delayed_choose_list(then.force(), Term::Error)
                                })
                        }
                    }),
                error_trace,
            )
    };

    let expect_some_a = |redeemer: Rc<Name>, then_delayed: Rc<Name>, trace: bool| {
        let trace_error = if trace {
            Term::var("r:A<B>")
        } else {
            Term::Error.delay()
        };

        Term::snd_pair()
            .apply(Term::unconstr_data().apply(Term::Var(redeemer)))
            .as_var("tail_id_5", |tail_id_5| {
                let inner = if trace {
                    Term::head_list().apply(Term::Var(tail_id_5.clone()))
                } else {
                    Term::head_list()
                        .apply(Term::Var(tail_id_5.clone()))
                        .as_var("val", |val| expect_void_no_trace(Term::Var(val)))
                }
                .as_var("__val", |val| {
                    if trace {
                        expect_void(
                            val,
                            Term::tail_list()
                                .apply(Term::Var(tail_id_5.clone()))
                                .as_var("tail_id_6", |tail_id_6| {
                                    Term::Var(tail_id_6.clone()).delay_filled_choose_list(
                                        trace_error.clone(),
                                        Term::head_list()
                                            .apply(Term::Var(tail_id_6.clone()))
                                            .as_var("__val", |val| {
                                                Term::choose_data_constr(
                                                    val.clone(),
                                                    |_| {
                                                        Term::tail_list()
                                                            .apply(Term::Var(tail_id_6))
                                                            .delay_empty_choose_list(
                                                                expect_b(
                                                                    val,
                                                                    Term::Var(then_delayed),
                                                                    trace,
                                                                ),
                                                                trace_error.clone(),
                                                            )
                                                    },
                                                    &trace_error,
                                                )
                                            }),
                                    )
                                }),
                        )
                    } else {
                        Term::tail_list()
                            .apply(Term::Var(tail_id_5.clone()))
                            .as_var("tail_id_6", |tail_id_6| {
                                Term::head_list()
                                    .apply(Term::Var(tail_id_6.clone()))
                                    .as_var("__val", |val| {
                                        Term::tail_list()
                                            .apply(Term::Var(tail_id_6))
                                            .delayed_choose_list(
                                                expect_b(val, Term::Var(then_delayed), trace),
                                                Term::Error,
                                            )
                                    })
                            })
                    }
                });

                if trace {
                    Term::Var(tail_id_5.clone())
                        .delay_filled_choose_list(trace_error.clone(), inner)
                } else {
                    inner
                }
            })
    };

    let when_constr_arity_2 =
        |redeemer: Rc<Name>, then_1st: Term<Name>, then_2nd: Term<Name>, trace: bool| {
            Term::fst_pair()
                .apply(Term::unconstr_data().apply(Term::Var(redeemer.clone())))
                .as_var("__subject_span_0_0", |subject_span_0_0| {
                    let when_constructor = |ix: usize| {
                        Term::equals_integer()
                            .apply(Term::integer(ix.into()))
                            .apply(Term::Var(subject_span_0_0.clone()))
                    };

                    when_constructor(0).delayed_if_then_else(
                        then_1st,
                        when_constructor(1).delay_true_if_then_else(
                            then_2nd,
                            if trace {
                                Term::var("r:A<B>")
                            } else {
                                Term::Error.delay()
                            },
                        ),
                    )
                })
        };

    let choose_purpose = |redeemer: Rc<Name>, purpose: Rc<Name>, trace: bool| {
        Term::equals_integer()
            .apply(Term::integer(1.into()))
            .apply(Term::fst_pair().apply(Term::unconstr_data().apply(Term::Var(purpose.clone()))))
            .delayed_if_then_else(
                Term::snd_pair()
                    .apply(Term::unconstr_data().apply(Term::Var(purpose)))
                    .as_var("tail_id_10", |tail_id_10| {
                        Term::head_list()
                            .apply(Term::Var(tail_id_10.clone()))
                            .as_var("__purpose_arg__", |_purpose_arg| {
                                Term::head_list()
                                    .apply(Term::tail_list().apply(Term::Var(tail_id_10.clone())))
                                    .as_var("__datum__", |_datum| {
                                        let body_part = body(redeemer.clone()).delay().as_var(
                                            "then_delayed",
                                            |then_delayed| {
                                                when_constr_arity_2(
                                                    redeemer.clone(),
                                                    expect_no_a(
                                                        redeemer.clone(),
                                                        then_delayed.clone(),
                                                        trace,
                                                    ),
                                                    expect_some_a(
                                                        redeemer.clone(),
                                                        then_delayed.clone(),
                                                        trace,
                                                    ),
                                                    trace,
                                                )
                                            },
                                        );

                                        if trace {
                                            Term::choose_data_constr(
                                                redeemer.clone(),
                                                |_| body_part,
                                                &Term::var("r:A<B>"),
                                            )
                                        } else {
                                            body_part
                                        }
                                    })
                            })
                    }),
                Term::Error,
            )
    };

    let validator = |trace: bool| {
        let context = "__context__";
        Term::snd_pair()
            .apply(Term::unconstr_data().apply(Term::var(context)))
            .as_var("tail_id_13", |tail_id_13| {
                Term::head_list()
                    .apply(Term::Var(tail_id_13.clone()))
                    .as_var("__transaction__", |_transaction| {
                        Term::tail_list().apply(Term::Var(tail_id_13)).as_var(
                            "tail_id_14",
                            |tail_id_14| {
                                Term::head_list()
                                    .apply(Term::Var(tail_id_14.clone()))
                                    .as_var("__redeemer__", |redeemer| {
                                        Term::head_list()
                                            .apply(Term::tail_list().apply(Term::Var(tail_id_14)))
                                            .as_var("__purpose__", |purpose| {
                                                choose_purpose(redeemer, purpose, trace)
                                            })
                                    })
                            },
                        )
                    })
            })
            .delayed_if_then_else(
                Term::unit(),
                if trace {
                    Term::Error
                        .apply(Term::Error.force())
                        .delayed_trace(Term::string("Validator returned false"))
                } else {
                    Term::Error.apply(Term::Error.force())
                },
            )
            .lambda(context)
    };

    assert_uplc(
        src,
        validator(true)
            .lambda("r:A<B>")
            .apply(Term::Error.delayed_trace(Term::string("r: A<B>")).delay()),
        false,
        true,
    );

    assert_uplc(src, validator(false), false, false);
}

#[test]
fn pass_constr_as_function() {
    let src = r#"
      type Make {
        a: Int,
        b: SubMake
      }

      type SubMake {
        c: Int
      }

      fn hi(sm: SubMake, to_make: fn (Int, SubMake) -> Make) -> Make {
        to_make(3, sm)
      }

      test cry() {
        Make(3, SubMake(1)) == hi(SubMake(1), Make)
      }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(Term::data(Data::constr(
                0,
                vec![
                    Data::integer(3.into()),
                    Data::constr(0, vec![Data::integer(1.into())]),
                ],
            )))
            .apply(
                Term::var("hi")
                    .lambda("hi")
                    .apply(
                        Term::var("to_make")
                            .apply(Term::integer(3.into()))
                            .apply(Term::var("sm"))
                            .lambda("to_make")
                            .lambda("sm"),
                    )
                    .apply(Term::data(Data::constr(0, vec![Data::integer(1.into())])))
                    .apply(
                        Term::constr_data()
                            .apply(Term::integer(0.into()))
                            .apply(
                                Term::mk_cons()
                                    .apply(Term::i_data().apply(Term::var("a")))
                                    .apply(
                                        Term::mk_cons()
                                            .apply(Term::var("b"))
                                            .apply(Term::empty_list()),
                                    ),
                            )
                            .lambda("b")
                            .lambda("a"),
                    ),
            ),
        false,
        true,
    );
}

#[test]
fn record_update_output_2_vals() {
    let src = r#"
      type Address {
        thing: ByteArray,
      }

      type Datum {
        NoDatum
        InlineDatum(Data)
      }

      type Output {
        address: Address,
        value: List<Pair<ByteArray, List<Pair<ByteArray, Int>>>>,
        datum: Datum,
        script_ref: Option<ByteArray>,
      }

      type MyDatum {
        a: Int,
      }

      test huh() {
        let prev_output =
          Output {
            address: Address { thing: "script_hash_0" },
            value: [],
            datum: InlineDatum(MyDatum{a: 3}),
            script_ref: None,
          }

        let next_output =
          Output { ..prev_output, value: [], datum: prev_output.datum }

        prev_output == next_output
      }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(Term::var("prev_output"))
            .apply(Term::var("next_output"))
            .lambda("next_output")
            .apply(
                Term::constr_data()
                    .apply(Term::integer(0.into()))
                    .apply(
                        Term::mk_cons()
                            .apply(Term::head_list().apply(Term::var("record_fields")))
                            .apply(
                                Term::mk_cons()
                                    .apply(Term::map_data().apply(Term::empty_map()))
                                    .apply(
                                        Term::mk_cons()
                                            .apply(
                                                Term::head_list()
                                                    .apply(
                                                        Term::tail_list().apply(
                                                            Term::tail_list()
                                                                .apply(Term::var("__fields")),
                                                        ),
                                                    )
                                                    .lambda("__fields")
                                                    .apply(
                                                        Term::var(CONSTR_FIELDS_EXPOSER)
                                                            .apply(Term::var("prev_output")),
                                                    ),
                                            )
                                            .apply(Term::var("tail_index_3")),
                                    ),
                            ),
                    )
                    .lambda("tail_index_3")
                    .apply(
                        Term::tail_list().apply(
                            Term::tail_list()
                                .apply(Term::tail_list().apply(Term::var("record_fields"))),
                        ),
                    )
                    .lambda("record_fields")
                    .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("prev_output"))),
            )
            .lambda("prev_output")
            .apply(Term::data(Data::constr(
                0,
                vec![
                    Data::constr(
                        0,
                        vec![Data::bytestring("script_hash_0".as_bytes().to_vec())],
                    ),
                    Data::map(vec![]),
                    Data::constr(1, vec![Data::constr(0, vec![Data::integer(3.into())])]),
                    Data::constr(1, vec![]),
                ],
            )))
            .constr_fields_exposer(),
        false,
        true,
    );
}

#[test]
fn record_update_output_1_val() {
    let src = r#"
      type Address {
        thing: ByteArray,
      }

      type Datum {
        NoDatum
        InlineDatum(Data)
      }

      type Output {
        address: Address,
        value: List<Pair<ByteArray, List<Pair<ByteArray, Int>>>>,
        datum: Datum,
        script_ref: Option<ByteArray>,
      }

      type MyDatum {
        a: Int,
      }

      test huh() {
        let prev_output =
          Output {
            address: Address { thing: "script_hash_0" },
            value: [],
            datum: InlineDatum(MyDatum{a: 3}),
            script_ref: None,
          }

        let next_output =
          Output { ..prev_output, datum: prev_output.datum }

        prev_output == next_output
      }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(Term::var("prev_output"))
            .apply(Term::var("next_output"))
            .lambda("next_output")
            .apply(
                Term::constr_data()
                    .apply(Term::integer(0.into()))
                    .apply(
                        Term::mk_cons()
                            .apply(Term::head_list().apply(Term::var("record_fields")))
                            .apply(
                                Term::mk_cons()
                                    .apply(Term::head_list().apply(Term::var("tail_index_1")))
                                    .apply(
                                        Term::mk_cons()
                                            .apply(
                                                Term::head_list()
                                                    .apply(
                                                        Term::tail_list().apply(
                                                            Term::tail_list()
                                                                .apply(Term::var("__fields")),
                                                        ),
                                                    )
                                                    .lambda("__fields")
                                                    .apply(
                                                        Term::var(CONSTR_FIELDS_EXPOSER)
                                                            .apply(Term::var("prev_output")),
                                                    ),
                                            )
                                            .apply(Term::var("tail_index_3")),
                                    ),
                            ),
                    )
                    .lambda("tail_index_3")
                    .apply(
                        Term::tail_list().apply(Term::tail_list().apply(Term::var("tail_index_1"))),
                    )
                    .lambda("tail_index_1")
                    .apply(Term::tail_list().apply(Term::var("record_fields")))
                    .lambda("record_fields")
                    .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("prev_output"))),
            )
            .lambda("prev_output")
            .apply(Term::data(Data::constr(
                0,
                vec![
                    Data::constr(
                        0,
                        vec![Data::bytestring("script_hash_0".as_bytes().to_vec())],
                    ),
                    Data::map(vec![]),
                    Data::constr(1, vec![Data::constr(0, vec![Data::integer(3.into())])]),
                    Data::constr(1, vec![]),
                ],
            )))
            .constr_fields_exposer(),
        false,
        true,
    );
}

#[test]
fn record_update_output_first_last_val() {
    let src = r#"
      type Address {
        thing: ByteArray,
      }

      type Datum {
        NoDatum
        InlineDatum(Data)
      }

      type Output {
        address: Address,
        value: List<Pair<ByteArray, List<Pair<ByteArray, Int>>>>,
        datum: Datum,
        script_ref: Option<ByteArray>,
      }

      type MyDatum {
        a: Int,
      }

      test huh() {
        let prev_output =
          Output {
            address: Address { thing: "script_hash_0" },
            value: [],
            datum: InlineDatum(MyDatum{a: 3}),
            script_ref: None,
          }

        let next_output =
          Output { ..prev_output, script_ref: None, address: Address{thing: "script_hash_0"} }

        prev_output == next_output
      }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(Term::var("prev_output"))
            .apply(Term::var("next_output"))
            .lambda("next_output")
            .apply(
                Term::constr_data()
                    .apply(Term::integer(0.into()))
                    .apply(
                        Term::mk_cons()
                            .apply(Term::data(Data::constr(
                                0,
                                vec![Data::bytestring("script_hash_0".as_bytes().to_vec())],
                            )))
                            .apply(
                                Term::mk_cons()
                                    .apply(Term::head_list().apply(Term::var("tail_index_1")))
                                    .apply(
                                        Term::mk_cons()
                                            .apply(
                                                Term::head_list().apply(Term::var("tail_index_2")),
                                            )
                                            .apply(
                                                Term::mk_cons()
                                                    .apply(Term::data(Data::constr(1, vec![])))
                                                    .apply(Term::var("tail_index_4")),
                                            ),
                                    ),
                            ),
                    )
                    .lambda("tail_index_4")
                    .apply(Term::empty_list())
                    .lambda("tail_index_2")
                    .apply(Term::tail_list().apply(Term::var("tail_index_1")))
                    .lambda("tail_index_1")
                    .apply(Term::tail_list().apply(Term::var("record_fields")))
                    .lambda("record_fields")
                    .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("prev_output"))),
            )
            .lambda("prev_output")
            .apply(Term::data(Data::constr(
                0,
                vec![
                    Data::constr(
                        0,
                        vec![Data::bytestring("script_hash_0".as_bytes().to_vec())],
                    ),
                    Data::map(vec![]),
                    Data::constr(1, vec![Data::constr(0, vec![Data::integer(3.into())])]),
                    Data::constr(1, vec![]),
                ],
            )))
            .constr_fields_exposer(),
        false,
        true,
    );
}

#[test]
fn list_fields_unwrap() {
    let src = r#"
      type Fields {
        a: ByteArray,
        b: Int,
      }

      fn data_fields(){
        [
            Fields{a: #"", b: 14},
            Fields{a: #"AA", b: 0}
        ]
      }

      test list_fields_unwr_0() {
        when data_fields() is {
          [Fields { b, .. }, ..] ->
            b > 0
          _ ->
            False
        } == True
      }
    "#;

    assert_uplc(
        src,
        Term::var("field_list")
            .delayed_choose_list(
                Term::bool(false),
                Term::less_than_integer()
                    .apply(Term::integer(0.into()))
                    .apply(Term::var("b"))
                    .lambda("b")
                    .apply(
                        Term::un_i_data()
                            .apply(Term::head_list().apply(Term::var("record_index_1"))),
                    )
                    .lambda("record_index_1")
                    .apply(Term::tail_list().apply(Term::var("item_1_fields")))
                    .lambda("item_1_fields")
                    .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("item_1")))
                    .lambda("item_1")
                    .apply(Term::head_list().apply(Term::var("field_list"))),
            )
            .lambda("field_list")
            .apply(Term::list_values(vec![
                Constant::Data(Data::constr(
                    0,
                    vec![Data::bytestring(vec![]), Data::integer(14.into())],
                )),
                Constant::Data(Data::constr(
                    0,
                    vec![Data::bytestring(vec![170]), Data::integer(0.into())],
                )),
            ]))
            .delayed_if_then_else(
                Term::bool(true),
                Term::bool(true).if_then_else(Term::bool(false), Term::bool(true)),
            )
            .constr_fields_exposer(),
        false,
        true,
    );
}

#[test]
fn foldl_type_mismatch() {
    let src = r#"

      type Address {
        payment_credential: ByteArray,
        stake_credential: Option<ByteArray>,
      }

      type Output {
        address: Address,
        value: List<Int>,
        datum: Option<Int>,
        reference_script: Option<Int>,
      }

      pub fn foldl(self: List<a>, with: fn(a, b) -> b, zero: b) -> b {
        when self is {
          [] -> zero
          [x, ..xs] -> foldl(xs, with, with(x, zero))
        }
      }

      test hi() {
        let addr1 = Address { payment_credential: "adff", stake_credential: None }

        let out =
          Output { address: addr1, value: [], datum: None, reference_script: None }

        let outputs: List<Output> =
          [out, out, out]
        let cry =
          foldl(
            outputs,
            fn(o: Output, mb_b: Option<Output>) -> Option<Output> {
              when mb_b is {
                None ->
                  if o.address == addr1 {
                    Some(o)
                  } else {
                    None
                  }
                otherwise -> otherwise
              }
            },
            None,
          )

        cry == cry
      }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(Term::var("cry"))
            .apply(Term::var("cry"))
            .lambda("cry")
            .apply(
                Term::var("foldl")
                    .lambda("foldl")
                    .apply(
                        Term::var("foldl")
                            .apply(Term::var("foldl"))
                            .apply(Term::var("self"))
                            .apply(Term::var("zero"))
                            .lambda("foldl")
                            .apply(
                                Term::var("self")
                                    .delayed_choose_list(
                                        Term::var("zero"),
                                        Term::var("foldl")
                                            .apply(Term::var("foldl"))
                                            .apply(Term::var("xs"))
                                            .apply(
                                                Term::var("with")
                                                    .apply(Term::var("x"))
                                                    .apply(Term::var("zero")),
                                            )
                                            .lambda("xs")
                                            .apply(Term::tail_list().apply(Term::var("self")))
                                            .lambda("x")
                                            .apply(Term::head_list().apply(Term::var("self"))),
                                    )
                                    .lambda("zero")
                                    .lambda("self")
                                    .lambda("foldl"),
                            )
                            .lambda("zero")
                            .lambda("with")
                            .lambda("self"),
                    )
                    .apply(Term::var("outputs"))
                    .apply(
                        Term::equals_integer()
                            .apply(Term::integer(1.into()))
                            .apply(Term::var("mb_b_index"))
                            .delayed_if_then_else(
                                Term::equals_data()
                                    .apply(Term::head_list().apply(
                                        Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("o")),
                                    ))
                                    .apply(Term::var("addr1"))
                                    .delayed_if_then_else(
                                        Term::constr_data().apply(Term::integer(0.into())).apply(
                                            Term::mk_cons()
                                                .apply(Term::var("o"))
                                                .apply(Term::empty_list()),
                                        ),
                                        Term::Constant(
                                            Constant::Data(Data::constr(1, vec![])).into(),
                                        ),
                                    ),
                                Term::var("mb_b"),
                            )
                            .lambda("mb_b_index")
                            .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("mb_b")))
                            .lambda("mb_b")
                            .lambda("o"),
                    )
                    .apply(Term::Constant(
                        Constant::Data(Data::constr(1, vec![])).into(),
                    )),
            )
            .lambda("outputs")
            .apply(
                Term::mk_cons().apply(Term::var("out")).apply(
                    Term::mk_cons().apply(Term::var("out")).apply(
                        Term::mk_cons()
                            .apply(Term::var("out"))
                            .apply(Term::empty_list()),
                    ),
                ),
            )
            .lambda("out")
            .apply(
                Term::constr_data().apply(Term::integer(0.into())).apply(
                    Term::mk_cons().apply(Term::var("addr1")).apply(
                        Term::mk_cons()
                            .apply(Term::list_data().apply(Term::empty_list()))
                            .apply(
                                Term::mk_cons()
                                    .apply(Term::Constant(
                                        Constant::Data(Data::constr(1, vec![])).into(),
                                    ))
                                    .apply(
                                        Term::mk_cons()
                                            .apply(Term::data(Data::constr(1, vec![])))
                                            .apply(Term::empty_list()),
                                    ),
                            ),
                    ),
                ),
            )
            .lambda("addr1")
            .apply(Term::data(Data::constr(
                0,
                vec![
                    Data::bytestring("adff".as_bytes().to_vec()),
                    Data::constr(1, vec![]),
                ],
            )))
            .constr_fields_exposer()
            .constr_index_exposer(),
        false,
        true,
    );
}

#[test]
fn expect_head_discard_tail() {
    let src = r#"
      test hi() {
        let a = [1, 2, 3]
        expect [h, ..] = a
        h == h
      }
    "#;

    assert_uplc(
        src,
        Term::var("a")
            .delayed_choose_list(
                Term::Error.delayed_trace(Term::var("expect[h,..]=a")),
                Term::equals_integer()
                    .apply(Term::var("h"))
                    .apply(Term::var("h"))
                    .lambda("h")
                    .apply(Term::un_i_data().apply(Term::head_list().apply(Term::var("a")))),
            )
            .lambda("a")
            .apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(3.into())),
            ]))
            .lambda("expect[h,..]=a")
            .apply(Term::string("expect [h, ..] = a")),
        false,
        true,
    );
}

#[test]
fn expect_head_no_tail() {
    let src = r#"
      test hi() {
        let a = [1, 2, 3]
        expect [h] = a
        h == h
      }
    "#;

    assert_uplc(
        src,
        Term::var("a")
            .delay_filled_choose_list(
                Term::var("expect[h]=a"),
                Term::tail_list()
                    .apply(Term::var("a"))
                    .delay_empty_choose_list(
                        Term::equals_integer()
                            .apply(Term::var("h"))
                            .apply(Term::var("h")),
                        Term::var("expect[h]=a"),
                    )
                    .lambda("h")
                    .apply(Term::un_i_data().apply(Term::head_list().apply(Term::var("a")))),
            )
            .lambda("a")
            .apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(3.into())),
            ]))
            .lambda("expect[h]=a")
            .apply(
                Term::Error
                    .delayed_trace(Term::string("expect [h] = a"))
                    .delay(),
            ),
        true,
        true,
    );
}

#[test]
fn expect_head3_no_tail() {
    let src = r#"
      test hi() {
        let a = [1, 2, 3]
        expect [h, i, j] = a
        (h == h && i == i) && j == j
      }
    "#;

    assert_uplc(
        src,
        Term::var("a")
            .delay_filled_choose_list(
                Term::var("expect[h,i,j]=a"),
                Term::var("tail_1")
                    .delay_filled_choose_list(
                        Term::var("expect[h,i,j]=a"),
                        Term::var("tail_2")
                            .delay_filled_choose_list(
                                Term::var("expect[h,i,j]=a"),
                                Term::tail_list()
                                    .apply(Term::var("tail_2"))
                                    .delay_empty_choose_list(
                                        Term::equals_integer()
                                            .apply(Term::var("h"))
                                            .apply(Term::var("h"))
                                            .delayed_if_then_else(
                                                Term::equals_integer()
                                                    .apply(Term::var("i"))
                                                    .apply(Term::var("i")),
                                                Term::bool(false),
                                            )
                                            .delayed_if_then_else(
                                                Term::equals_integer()
                                                    .apply(Term::var("j"))
                                                    .apply(Term::var("j")),
                                                Term::bool(false),
                                            ),
                                        Term::var("expect[h,i,j]=a"),
                                    )
                                    .lambda("j")
                                    .apply(
                                        Term::un_i_data()
                                            .apply(Term::head_list().apply(Term::var("tail_2"))),
                                    ),
                            )
                            .lambda("tail_2")
                            .apply(Term::tail_list().apply(Term::var("tail_1")))
                            .lambda("i")
                            .apply(
                                Term::un_i_data()
                                    .apply(Term::head_list().apply(Term::var("tail_1"))),
                            ),
                    )
                    .lambda("tail_1")
                    .apply(Term::tail_list().apply(Term::var("a")))
                    .lambda("h")
                    .apply(Term::un_i_data().apply(Term::head_list().apply(Term::var("a")))),
            )
            .lambda("a")
            .apply(Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(3.into())),
            ]))
            .lambda(Term::var("expect[h,i,j]=a"))
            .apply(
                Term::Error
                    .delayed_trace(Term::string("expect [h, i, j] = a"))
                    .delay(),
            ),
        false,
        true,
    );
}

#[test]
fn expect_head3_cast_data_no_tail() {
    let src = r#"
      test hi() {
        let a: Data = [1, 2, 3]
        expect [h, i, j]: List<Int> = a
        (h == h && i ==i) && j == j
      }
    "#;

    let otherwise_expect = &Term::var("expect[h,i,j]:List<Int>=a");

    let otherwise_msg: Term<Name> = Term::Error
        .delayed_trace(Term::string("expect [h, i, j]: List<Int> = a"))
        .delay();

    let then = Term::equals_integer()
        .apply(Term::var("h"))
        .apply(Term::var("h"))
        .delayed_if_then_else(
            Term::equals_integer()
                .apply(Term::var("i"))
                .apply(Term::var("i")),
            Term::bool(false),
        )
        .delayed_if_then_else(
            Term::equals_integer()
                .apply(Term::var("j"))
                .apply(Term::var("j")),
            Term::bool(false),
        );

    let unwrap_third = |head, tail: Term<Name>| {
        Term::choose_data_integer(
            head,
            |head_int| {
                head_int.as_var("j", |_| {
                    tail.delay_empty_choose_list(then, otherwise_expect.clone())
                })
            },
            otherwise_expect,
        )
    };

    let unwrap_second = |head, tail: Term<Name>| {
        Term::choose_data_integer(
            head,
            |head_int| {
                head_int.as_var("i", |_| {
                    tail.as_var("tail_id_2", |tail| {
                        Term::unwrap_tail_or(
                            tail.clone(),
                            |tail2| {
                                Term::head_list()
                                    .apply(Term::Var(tail))
                                    .as_var("head2", |head2| unwrap_third(head2, tail2))
                            },
                            otherwise_expect,
                        )
                    })
                })
            },
            otherwise_expect,
        )
    };

    let unwrap_first = |head, tail: Term<Name>| {
        Term::choose_data_integer(
            head,
            |head_int| {
                head_int.as_var("h", |_| {
                    tail.as_var("tail_id_1", |tail| {
                        Term::unwrap_tail_or(
                            tail.clone(),
                            |tail2| {
                                Term::head_list()
                                    .apply(Term::Var(tail))
                                    .as_var("head2", |head2| unwrap_second(head2, tail2))
                            },
                            otherwise_expect,
                        )
                    })
                })
            },
            otherwise_expect,
        )
    };

    let data_values = Term::data(Data::list(vec![
        Data::integer(1.into()),
        Data::integer(2.into()),
        Data::integer(3.into()),
    ]));

    assert_uplc(
        src,
        data_values
            .as_var("a", |a| {
                Term::choose_data_list(
                    a,
                    |list| {
                        list.as_var("list", |list_var| {
                            Term::unwrap_tail_or(
                                list_var.clone(),
                                |tail| {
                                    Term::head_list()
                                        .apply(Term::Var(list_var))
                                        .as_var("__val", |head| unwrap_first(head, tail))
                                },
                                otherwise_expect,
                            )
                        })
                    },
                    otherwise_expect,
                )
            })
            .lambda("expect[h,i,j]:List<Int>=a")
            .apply(otherwise_msg),
        false,
        true,
    );
}

#[test]
fn expect_head_cast_data_no_tail() {
    let src = r#"
      test hi() {
        let a: Data = [1, 2, 3]
        expect [h]: List<Int> = a
        h == h
      }
    "#;

    assert_uplc(
        src,
        Term::data(Data::list(vec![
            Data::integer(1.into()),
            Data::integer(2.into()),
            Data::integer(3.into()),
        ]))
        .choose_data(
            Term::var("expect[h]:List<Int>=a"),
            Term::var("expect[h]:List<Int>=a"),
            Term::list_values(vec![
                Constant::Data(Data::integer(1.into())),
                Constant::Data(Data::integer(2.into())),
                Constant::Data(Data::integer(3.into())),
            ])
            .delay_filled_choose_list(
                Term::var("expect[h]:List<Int>=a"),
                Term::var("__var")
                    .choose_data(
                        Term::var("expect[h]:List<Int>=a"),
                        Term::var("expect[h]:List<Int>=a"),
                        Term::var("expect[h]:List<Int>=a"),
                        Term::tail_list()
                            .apply(Term::list_values(vec![
                                Constant::Data(Data::integer(1.into())),
                                Constant::Data(Data::integer(2.into())),
                                Constant::Data(Data::integer(3.into())),
                            ]))
                            .delay_empty_choose_list(
                                Term::equals_integer()
                                    .apply(Term::var("h"))
                                    .apply(Term::var("h")),
                                Term::var("expect[h]:List<Int>=a"),
                            )
                            .lambda("h")
                            .apply(Term::un_i_data().apply(Term::var("__var")))
                            .delay(),
                        Term::var("expect[h]:List<Int>=a"),
                    )
                    .force()
                    .lambda("__var")
                    .apply(Term::head_list().apply(Term::list_values(vec![
                        Constant::Data(Data::integer(1.into())),
                        Constant::Data(Data::integer(2.into())),
                        Constant::Data(Data::integer(3.into())),
                    ]))),
            )
            .delay(),
            Term::var("expect[h]:List<Int>=a"),
            Term::var("expect[h]:List<Int>=a"),
        )
        .force()
        .lambda("expect[h]:List<Int>=a")
        .apply(
            Term::Error
                .delayed_trace(Term::string("expect [h]: List<Int> = a"))
                .delay(),
        ),
        true,
        true,
    );
}

#[test]
fn expect_head_cast_data_with_tail() {
    let src = r#"
      test hi() {
        let a: Data = [1, 2, 3]
        expect [h, j, ..]: List<Int> = a
        h == h && j == j
      }
    "#;

    let expect_on_list = Term::var(EXPECT_ON_LIST)
        .apply(Term::var(EXPECT_ON_LIST))
        .apply(Term::var("__list"))
        .lambda(EXPECT_ON_LIST)
        .apply(
            Term::var("check_with")
                .apply(Term::var("__list"))
                .apply(Term::var(EXPECT_ON_LIST).apply(Term::var(EXPECT_ON_LIST)))
                .lambda("__list")
                .lambda(EXPECT_ON_LIST)
                .lambda("__no_inline__"),
        )
        .lambda("check_with")
        .lambda("__list")
        .lambda("__no_inline__");

    let values = Term::list_values(vec![
        Constant::Data(Data::integer(1.into())),
        Constant::Data(Data::integer(2.into())),
        Constant::Data(Data::integer(3.into())),
    ]);

    let then = Term::equals_integer()
        .apply(Term::var("h"))
        .apply(Term::var("h"))
        .delayed_if_then_else(
            Term::equals_integer()
                .apply(Term::var("j"))
                .apply(Term::var("j")),
            Term::bool(false),
        );

    let check_with = Term::var("__list")
        .delayed_choose_list(
            then,
            Term::choose_data_integer(
                Name::text("__head").into(),
                |v| {
                    Term::var("__curried_expect_on_list")
                        .apply(Term::tail_list().apply(Term::var("__list")))
                        .lambda("_")
                        .apply(v)
                },
                &Term::var("expect[h,j,..]:List<Int>=a"),
            )
            .lambda("__head")
            .apply(Term::head_list().apply(Term::var("__list"))),
        )
        .lambda("__curried_expect_on_list")
        .lambda("__list")
        .lambda("__no_inline__");

    let on_list = values.clone().delay_filled_choose_list(
        Term::var("expect[h,j,..]:List<Int>=a"),
        Term::var("__val")
            .choose_data(
                Term::var("expect[h,j,..]:List<Int>=a"),
                Term::var("expect[h,j,..]:List<Int>=a"),
                Term::var("expect[h,j,..]:List<Int>=a"),
                Term::var("tail_1")
                    .delay_filled_choose_list(
                        Term::var("expect[h,j,..]:List<Int>=a"),
                        Term::var("__val")
                            .choose_data(
                                Term::var("expect[h,j,..]:List<Int>=a"),
                                Term::var("expect[h,j,..]:List<Int>=a"),
                                Term::var("expect[h,j,..]:List<Int>=a"),
                                Term::var(EXPECT_ON_LIST)
                                    .lambda(EXPECT_ON_LIST)
                                    .apply(expect_on_list)
                                    .apply(Term::tail_list().apply(Term::var("tail_1")))
                                    .apply(check_with)
                                    .lambda("j")
                                    .apply(Term::un_i_data().apply(Term::var("__val")))
                                    .delay(),
                                Term::var("expect[h,j,..]:List<Int>=a"),
                            )
                            .force()
                            .lambda("__val")
                            .apply(Term::head_list().apply(Term::var("tail_1"))),
                    )
                    .lambda("tail_1")
                    .apply(Term::tail_list().apply(values.clone()))
                    .lambda("h")
                    .apply(Term::un_i_data().apply(Term::var("__val")))
                    .delay(),
                Term::var("expect[h,j,..]:List<Int>=a"),
            )
            .force()
            .lambda("__val")
            .apply(Term::head_list().apply(values)),
    );

    assert_uplc(
        src,
        Term::choose_data_list(
            Name::text("__val").into(),
            |_| on_list,
            &Term::var("expect[h,j,..]:List<Int>=a"),
        )
        .lambda("__val")
        .apply(Term::data(Data::list(vec![
            Data::integer(1.into()),
            Data::integer(2.into()),
            Data::integer(3.into()),
        ])))
        .lambda("expect[h,j,..]:List<Int>=a")
        .apply(
            Term::Error
                .delayed_trace(Term::string("expect [h, j, ..]: List<Int> = a"))
                .delay(),
        ),
        false,
        true,
    );
}

#[test]
fn test_init_3() {
    let src = r#"

      pub fn init(self: List<a>) -> Option<List<a>> {
        when self is {
          [] -> None
          _ -> Some(do_init(self))
        }
      }

      fn do_init(self: List<a>) -> List<a> {
        when self is {
          [] -> fail @"unreachable"
          [_] ->
            []
          [x, ..xs] ->
            [x, ..do_init(xs)]
        }
      }

      test init_3() {
        init([1, 2, 3, 4]) == Some([1, 2, 3])
      }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::var("init")
                    .lambda("init")
                    .apply(
                        Term::var("self")
                            .delayed_choose_list(
                                Term::data(Data::constr(1, vec![])),
                                Term::constr_data().apply(Term::integer(0.into())).apply(
                                    Term::mk_cons()
                                        .apply(
                                            Term::list_data().apply(
                                                Term::var("do_init").apply(Term::var("self")),
                                            ),
                                        )
                                        .apply(Term::empty_list()),
                                ),
                            )
                            .lambda("self"),
                    )
                    .lambda("do_init")
                    .apply(Term::var("do_init").apply(Term::var("do_init")))
                    .lambda("do_init")
                    .apply(
                        Term::var("self")
                            .delayed_choose_list(
                                Term::Error.delayed_trace(Term::string("unreachable")),
                                Term::var("tail_1")
                                    .delayed_choose_list(
                                        Term::empty_list(),
                                        Term::mk_cons()
                                            .apply(Term::i_data().apply(Term::var("x")))
                                            .apply(
                                                Term::var("do_init")
                                                    .apply(Term::var("do_init"))
                                                    .apply(Term::var("xs")),
                                            )
                                            .lambda("xs")
                                            .apply(Term::tail_list().apply(Term::var("self")))
                                            .lambda("x")
                                            .apply(
                                                Term::un_i_data().apply(
                                                    Term::head_list().apply(Term::var("self")),
                                                ),
                                            ),
                                    )
                                    .lambda("tail_1")
                                    .apply(Term::tail_list().apply(Term::var("self"))),
                            )
                            .lambda("self")
                            .lambda("do_init"),
                    )
                    .apply(Term::list_values(vec![
                        Constant::Data(Data::integer(1.into())),
                        Constant::Data(Data::integer(2.into())),
                        Constant::Data(Data::integer(3.into())),
                        Constant::Data(Data::integer(4.into())),
                    ])),
            )
            .apply(Term::data(Data::constr(
                0,
                vec![Data::list(vec![
                    Data::integer(1.into()),
                    Data::integer(2.into()),
                    Data::integer(3.into()),
                ])],
            ))),
        false,
        true,
    );
}

#[test]
fn list_clause_with_assign() {
    let src = r#"
      fn do_init(self: List<Option<Int>>) -> List<Option<Int>> {
        when self is {
          [] -> fail @"unreachable"
          [_] as a ->
            a
          [Some(_) as n, x] -> {
            [n]
          }
          [a, x] -> {
            [x]
          }
          [a, b, ..c] -> {
            c
          }
        }
      }

      test init_3() {
        do_init([Some(1), None]) == [Some(1)]
      }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::var("do_init")
                        .lambda("do_init")
                        .apply(
                            Term::var("self")
                                .delayed_choose_list(
                                    Term::Error.delayed_trace(Term::string("unreachable")),
                                    Term::var("tail_1")
                                        .delayed_choose_list(
                                            Term::var("self"),
                                            Term::var("tail_2")
                                                .delay_empty_choose_list(
                                                    Term::equals_integer()
                                                        .apply(Term::integer(0.into()))
                                                        .apply(
                                                            Term::var(CONSTR_INDEX_EXPOSER)
                                                                .apply(Term::var("n")),
                                                        )
                                                        .delay_true_if_then_else(
                                                            Term::mk_cons()
                                                                .apply(Term::var("n"))
                                                                .apply(Term::empty_list()),
                                                            Term::var("clauses_delayed"),
                                                        )
                                                        .lambda("x")
                                                        .apply(
                                                            Term::head_list()
                                                                .apply(Term::var("tail_1")),
                                                        )
                                                        .lambda("n")
                                                        .apply(
                                                            Term::head_list()
                                                                .apply(Term::var("self")),
                                                        ),
                                                    Term::var("clauses_delayed"),
                                                )
                                                .lambda("clauses_delayed")
                                                .apply(
                                                    Term::var("tail_2")
                                                        .delayed_choose_list(
                                                            Term::mk_cons()
                                                                .apply(Term::var("x"))
                                                                .apply(Term::empty_list())
                                                                .lambda("x")
                                                                .apply(
                                                                    Term::head_list()
                                                                        .apply(Term::var("tail_1")),
                                                                )
                                                                .lambda("a")
                                                                .apply(
                                                                    Term::head_list()
                                                                        .apply(Term::var("self")),
                                                                ),
                                                            Term::var("c").lambda("c").apply(
                                                                Term::tail_list()
                                                                    .apply(Term::var("tail_1"))
                                                                    .lambda("b")
                                                                    .apply(
                                                                        Term::head_list().apply(
                                                                            Term::var("tail_1"),
                                                                        ),
                                                                    )
                                                                    .lambda("a")
                                                                    .apply(
                                                                        Term::head_list().apply(
                                                                            Term::var("self"),
                                                                        ),
                                                                    ),
                                                            ),
                                                        )
                                                        .delay(),
                                                )
                                                .lambda("tail_2")
                                                .apply(
                                                    Term::tail_list().apply(Term::var("tail_1")),
                                                ),
                                        )
                                        .lambda("tail_1")
                                        .apply(Term::tail_list().apply(Term::var("self"))),
                                )
                                .lambda("self"),
                        )
                        .apply(Term::list_values(vec![
                            Constant::Data(Data::constr(0, vec![Data::integer(1.into())])),
                            Constant::Data(Data::constr(1, vec![])),
                        ])),
                ),
            )
            .apply(Term::data(Data::list(vec![Data::constr(
                0,
                vec![Data::integer(1.into())],
            )])))
            .constr_index_exposer(),
        false,
        true,
    );
}

#[test]
fn opaque_value_in_test() {
    let src = r#"
        const dat: Dat = {
          let v = Value { inner: Dict { inner: [Pair("", [Pair(#"aa", 4)] |> Dict)] } }
          Dat {
            c: 0,
            a: v
          }
        }

        pub opaque type Value {
          inner: Dict<Dict<Int>>
        }

        pub opaque type Dict<v> {
          inner: List<Pair<ByteArray, v>>
        }

        pub type Dat {
          c: Int,
          a: Value
        }

        test spend() {
          let val = dat.a

          expect [Pair(_, amount)] = val.inner.inner

          let final_amount = [Pair(#"AA", 4)] |> Dict

          final_amount == amount
        }
  "#;

    let expect_otherwise = &Term::var("expect[Pair(_,amount)]=val.inner.inner");
    let expect_delay_error: Term<Name> = Term::Error
        .delayed_trace(Term::string("expect [Pair(_, amount)] = val.inner.inner"))
        .delay();

    let final_comparison = Term::equals_data()
        .apply(Term::map_data().apply(Term::var("final_amount")))
        .apply(Term::map_data().apply(Term::var("amount")));

    let assignments_body = Term::unmap_data()
        .apply(Term::head_list().apply(
            Term::tail_list().apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("dat"))),
        ))
        .as_var("val", |val| {
            Term::unwrap_tail_or(
                val.clone(),
                |tail| {
                    Term::head_list()
                        .apply(Term::Var(val))
                        .as_var("elem_0", |elem| {
                            tail.delay_empty_choose_list(
                                Term::unmap_data()
                                    .apply(Term::snd_pair().apply(Term::Var(elem)))
                                    .as_var("amount", |_| {
                                        Term::map_values(vec![Constant::ProtoPair(
                                            Type::Data,
                                            Type::Data,
                                            Constant::Data(Data::bytestring(vec![170])).into(),
                                            Constant::Data(Data::integer(4.into())).into(),
                                        )])
                                        .as_var("final_amount", |_| final_comparison)
                                    }),
                                expect_otherwise.clone(),
                            )
                        })
                },
                expect_otherwise,
            )
        });

    assert_uplc(
        src,
        assignments_body
            .lambda("dat")
            .apply(Term::data(Data::constr(
                0,
                vec![
                    Data::integer(0.into()),
                    Data::map(vec![(
                        Data::bytestring(vec![]),
                        Data::map(vec![(Data::bytestring(vec![170]), Data::integer(4.into()))]),
                    )]),
                ],
            )))
            .constr_fields_exposer()
            .lambda("expect[Pair(_,amount)]=val.inner.inner")
            .apply(expect_delay_error)
            .lambda("dat:Dat")
            .apply(Term::Error.delayed_trace(Term::string("dat: Dat")).delay()),
        false,
        true,
    );
}

#[test]
fn expect_none() {
    let src = r#"
        test exp_none() {
          let x = None
          expect None = x
          True

        }
  "#;

    assert_uplc(
        src,
        Term::equals_integer()
            .apply(Term::integer(1.into()))
            .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("x")))
            .delayed_if_then_else(
                Term::bool(true),
                Term::Error.delayed_trace(Term::string("expect None = x")),
            )
            .lambda("x")
            .apply(Term::Constant(
                Constant::Data(Data::constr(1, vec![])).into(),
            ))
            .constr_index_exposer(),
        false,
        true,
    );
}

#[test]
fn head_list_on_map() {
    let src = r#"
        use aiken/builtin

        test exp_none() {
          let x = [Pair(1, ""), Pair(2, #"aa")]
          builtin.head_list(x) == Pair(1, "")
        }
  "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::map_data().apply(
                    Term::mk_cons()
                        .apply(Term::head_list().apply(Term::var("x")))
                        .apply(Term::empty_map()),
                ),
            )
            .apply(
                Term::map_data().apply(
                    Term::mk_cons()
                        .apply(Term::pair_values(
                            Constant::Data(Data::integer(1.into())),
                            Constant::Data(Data::bytestring(vec![])),
                        ))
                        .apply(Term::empty_map()),
                ),
            )
            .lambda("x")
            .apply(Term::map_values(vec![
                Constant::ProtoPair(
                    Type::Data,
                    Type::Data,
                    Constant::Data(Data::integer(1.into())).into(),
                    Constant::Data(Data::bytestring(vec![])).into(),
                ),
                Constant::ProtoPair(
                    Type::Data,
                    Type::Data,
                    Constant::Data(Data::integer(2.into())).into(),
                    Constant::Data(Data::bytestring(vec![170])).into(),
                ),
            ])),
        false,
        true,
    );
}

#[test]
fn tuple_2_match() {
    let src = r#"
        type CurveInt {
            ECI { ec: (Int, Int)}
            Infinity
        }

        fn equivalence(ec1: CurveInt, ec2: CurveInt) -> Bool {
            let input = (ec1, ec2)
            when input is {
                (ECI { ec: (x1, y1)}, ECI { ec: (x2, y2)}) -> {
                    x2 - x1 == 0 && y2 - y1 == 0
                }
                (Infinity, Infinity) -> {
                    True
                }
                (Infinity, ECI { ec: _}) -> {
                    False
                }
                (ECI { ec: _}, Infinity) -> {
                    False
                }
            }
        }

        test equivalence1() {
            equivalence(Infinity, Infinity) == True
        }
  "#;

    assert_uplc(
        src,
        Term::var("equivalence")
            .lambda("equivalence")
            .apply(
                Term::equals_integer()
                    .apply(Term::integer(0.into()))
                    .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("tuple_index_0")))
                    .delay_true_if_then_else(
                        Term::equals_integer()
                            .apply(Term::integer(0.into()))
                            .apply(
                                Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("tuple_index_1")),
                            )
                            .delay_true_if_then_else(
                                Term::equals_integer()
                                    .apply(
                                        Term::subtract_integer()
                                            .apply(Term::var("x2"))
                                            .apply(Term::var("x1")),
                                    )
                                    .apply(Term::integer(0.into()))
                                    .delayed_if_then_else(
                                        Term::equals_integer()
                                            .apply(
                                                Term::subtract_integer()
                                                    .apply(Term::var("y2"))
                                                    .apply(Term::var("y1")),
                                            )
                                            .apply(Term::integer(0.into())),
                                        Term::bool(false),
                                    )
                                    .lambda("x2")
                                    .apply(
                                        Term::un_i_data().apply(
                                            Term::head_list().apply(Term::var("field_0_pair")),
                                        ),
                                    )
                                    .lambda("y2")
                                    .apply(Term::un_i_data().apply(
                                        Term::head_list().apply(
                                            Term::tail_list().apply(Term::var("field_0_pair")),
                                        ),
                                    ))
                                    .lambda("field_0_pair")
                                    .apply(Term::unlist_data().apply(
                                        Term::head_list().apply(Term::var("tuple_index_1_fields")),
                                    ))
                                    .lambda("tuple_index_1_fields")
                                    .apply(
                                        Term::var(CONSTR_FIELDS_EXPOSER)
                                            .apply(Term::var("tuple_index_1")),
                                    ),
                                Term::var("clauses_delayed"),
                            )
                            .lambda("x1")
                            .apply(
                                Term::un_i_data()
                                    .apply(Term::head_list().apply(Term::var("field_0_pair"))),
                            )
                            .lambda("y1")
                            .apply(
                                Term::un_i_data().apply(
                                    Term::head_list()
                                        .apply(Term::tail_list().apply(Term::var("field_0_pair"))),
                                ),
                            )
                            .lambda("field_0_pair")
                            .apply(
                                Term::unlist_data().apply(
                                    Term::head_list().apply(Term::var("tuple_index_0_fields")),
                                ),
                            )
                            .lambda("tuple_index_0_fields")
                            .apply(
                                Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("tuple_index_0")),
                            ),
                        Term::var("clauses_delayed"),
                    )
                    .lambda("clauses_delayed")
                    .apply(
                        Term::equals_integer()
                            .apply(Term::integer(1.into()))
                            .apply(
                                Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("tuple_index_0")),
                            )
                            .delay_true_if_then_else(
                                Term::equals_integer()
                                    .apply(Term::integer(1.into()))
                                    .apply(
                                        Term::var(CONSTR_INDEX_EXPOSER)
                                            .apply(Term::var("tuple_index_1")),
                                    )
                                    .delay_true_if_then_else(
                                        Term::bool(true),
                                        Term::var("clauses_delayed"),
                                    ),
                                Term::var("clauses_delayed"),
                            )
                            .lambda("clauses_delayed")
                            .apply(
                                Term::equals_integer()
                                    .apply(Term::integer(1.into()))
                                    .apply(
                                        Term::var(CONSTR_INDEX_EXPOSER)
                                            .apply(Term::var("tuple_index_0")),
                                    )
                                    .delay_true_if_then_else(
                                        Term::equals_integer()
                                            .apply(Term::integer(0.into()))
                                            .apply(
                                                Term::var(CONSTR_INDEX_EXPOSER)
                                                    .apply(Term::var("tuple_index_1")),
                                            )
                                            .delay_true_if_then_else(
                                                Term::bool(false),
                                                Term::var("clauses_delayed"),
                                            ),
                                        Term::var("clauses_delayed"),
                                    )
                                    .lambda("clauses_delayed")
                                    .apply(Term::bool(false).delay())
                                    .delay(),
                            )
                            .delay(),
                    )
                    .lambda("tuple_index_0")
                    .apply(Term::head_list().apply(Term::var("input")))
                    .lambda("tuple_index_1")
                    .apply(Term::head_list().apply(Term::tail_list().apply(Term::var("input"))))
                    .lambda("input")
                    .apply(
                        Term::mk_cons().apply(Term::var("ec1")).apply(
                            Term::mk_cons()
                                .apply(Term::var("ec2"))
                                .apply(Term::empty_list()),
                        ),
                    )
                    .lambda("ec2")
                    .lambda("ec1"),
            )
            .apply(Term::data(Data::constr(1, vec![])))
            .apply(Term::data(Data::constr(1, vec![])))
            .delayed_if_then_else(
                Term::bool(true),
                Term::bool(true).if_then_else(Term::bool(false), Term::bool(true)),
            )
            .constr_index_exposer()
            .constr_fields_exposer(),
        false,
        true,
    );
}

#[test]
fn bls12_381_elements_to_data_conversion() {
    let src = r#"
      pub type Proof {
        piA: G1Element,
        piB: G2Element,
      }

      test thing() {
        let pk =
          Proof {
            piA: #<Bls12_381, G1>"b28cb29bc282be68df977b35eb9d8e98b3a0a3fc7c372990bddc50419ca86693e491755338fed4fb42231a7c081252ce",
            piB: #<Bls12_381, G2>"b9215e5bc481ba6552384c89c23d45bd650b69462868248bfbb83aee7060579404dba41c781dec7c2bec5fccec06842e0e66ad6d86c7c76c468a32c9c0080eea0219d0953b44b1c4f5605afb1e5a3193264ff730222e94f55207628235f3b423",
         }

        pk == pk
      }
    "#;

    let constant = Term::Constant(
        Constant::Data(Data::constr(
            0,
            vec![
                Data::bytestring(vec![
                    0xb2, 0x8c, 0xb2, 0x9b, 0xc2, 0x82, 0xbe, 0x68, 0xdf, 0x97, 0x7b, 0x35, 0xeb,
                    0x9d, 0x8e, 0x98, 0xb3, 0xa0, 0xa3, 0xfc, 0x7c, 0x37, 0x29, 0x90, 0xbd, 0xdc,
                    0x50, 0x41, 0x9c, 0xa8, 0x66, 0x93, 0xe4, 0x91, 0x75, 0x53, 0x38, 0xfe, 0xd4,
                    0xfb, 0x42, 0x23, 0x1a, 0x7c, 0x08, 0x12, 0x52, 0xce,
                ]),
                Data::bytestring(vec![
                    0xb9, 0x21, 0x5e, 0x5b, 0xc4, 0x81, 0xba, 0x65, 0x52, 0x38, 0x4c, 0x89, 0xc2,
                    0x3d, 0x45, 0xbd, 0x65, 0x0b, 0x69, 0x46, 0x28, 0x68, 0x24, 0x8b, 0xfb, 0xb8,
                    0x3a, 0xee, 0x70, 0x60, 0x57, 0x94, 0x04, 0xdb, 0xa4, 0x1c, 0x78, 0x1d, 0xec,
                    0x7c, 0x2b, 0xec, 0x5f, 0xcc, 0xec, 0x06, 0x84, 0x2e, 0x0e, 0x66, 0xad, 0x6d,
                    0x86, 0xc7, 0xc7, 0x6c, 0x46, 0x8a, 0x32, 0xc9, 0xc0, 0x08, 0x0e, 0xea, 0x02,
                    0x19, 0xd0, 0x95, 0x3b, 0x44, 0xb1, 0xc4, 0xf5, 0x60, 0x5a, 0xfb, 0x1e, 0x5a,
                    0x31, 0x93, 0x26, 0x4f, 0xf7, 0x30, 0x22, 0x2e, 0x94, 0xf5, 0x52, 0x07, 0x62,
                    0x82, 0x35, 0xf3, 0xb4, 0x23,
                ]),
            ],
        ))
        .into(),
    );

    assert_uplc(
        src,
        Term::equals_data().apply(constant.clone()).apply(constant),
        false,
        true,
    )
}

#[test]
fn bls12_381_elements_from_data_conversion() {
    let src = r#"
      pub type Proof {
        piA: G1Element,
        piB: G2Element,
      }

      test thing() {
        let pk =
          Proof {
            piA: #<Bls12_381, G1>"b28cb29bc282be68df977b35eb9d8e98b3a0a3fc7c372990bddc50419ca86693e491755338fed4fb42231a7c081252ce",
            piB: #<Bls12_381, G2>"b9215e5bc481ba6552384c89c23d45bd650b69462868248bfbb83aee7060579404dba41c781dec7c2bec5fccec06842e0e66ad6d86c7c76c468a32c9c0080eea0219d0953b44b1c4f5605afb1e5a3193264ff730222e94f55207628235f3b423",
         }

        pk.piA == #<Bls12_381, G1>"b28cb29bc282be68df977b35eb9d8e98b3a0a3fc7c372990bddc50419ca86693e491755338fed4fb42231a7c081252ce"
      }
    "#;

    let bytes = vec![
        0xb2, 0x8c, 0xb2, 0x9b, 0xc2, 0x82, 0xbe, 0x68, 0xdf, 0x97, 0x7b, 0x35, 0xeb, 0x9d, 0x8e,
        0x98, 0xb3, 0xa0, 0xa3, 0xfc, 0x7c, 0x37, 0x29, 0x90, 0xbd, 0xdc, 0x50, 0x41, 0x9c, 0xa8,
        0x66, 0x93, 0xe4, 0x91, 0x75, 0x53, 0x38, 0xfe, 0xd4, 0xfb, 0x42, 0x23, 0x1a, 0x7c, 0x08,
        0x12, 0x52, 0xce,
    ];

    let g1 = Term::Constant(
        Constant::Bls12_381G1Element(blst::blst_p1::uncompress(&bytes).unwrap().into()).into(),
    );

    let constant = Term::Constant(
        Constant::Data(Data::constr(
            0,
            vec![
                Data::bytestring(bytes),
                Data::bytestring(vec![
                    0xb9, 0x21, 0x5e, 0x5b, 0xc4, 0x81, 0xba, 0x65, 0x52, 0x38, 0x4c, 0x89, 0xc2,
                    0x3d, 0x45, 0xbd, 0x65, 0x0b, 0x69, 0x46, 0x28, 0x68, 0x24, 0x8b, 0xfb, 0xb8,
                    0x3a, 0xee, 0x70, 0x60, 0x57, 0x94, 0x04, 0xdb, 0xa4, 0x1c, 0x78, 0x1d, 0xec,
                    0x7c, 0x2b, 0xec, 0x5f, 0xcc, 0xec, 0x06, 0x84, 0x2e, 0x0e, 0x66, 0xad, 0x6d,
                    0x86, 0xc7, 0xc7, 0x6c, 0x46, 0x8a, 0x32, 0xc9, 0xc0, 0x08, 0x0e, 0xea, 0x02,
                    0x19, 0xd0, 0x95, 0x3b, 0x44, 0xb1, 0xc4, 0xf5, 0x60, 0x5a, 0xfb, 0x1e, 0x5a,
                    0x31, 0x93, 0x26, 0x4f, 0xf7, 0x30, 0x22, 0x2e, 0x94, 0xf5, 0x52, 0x07, 0x62,
                    0x82, 0x35, 0xf3, 0xb4, 0x23,
                ]),
            ],
        ))
        .into(),
    );

    assert_uplc(
        src,
        Term::bls12_381_g1_equal()
            .apply(Term::bls12_381_g1_uncompress().apply(
                Term::un_b_data().apply(
                    Term::head_list().apply(
                        Term::snd_pair().apply(Term::unconstr_data().apply(constant.clone())),
                    ),
                ),
            ))
            .apply(g1),
        false,
        true,
    )
}

#[test]
fn qualified_prelude_functions() {
    let src = r#"
        use aiken

        test foo() {
            aiken.identity(True) && identity(True)
        }
    "#;

    let constant_true = Term::Constant(Constant::Bool(true).into());
    let constant_false = Term::Constant(Constant::Bool(false).into());

    assert_uplc(
        src,
        constant_true
            .clone()
            .delayed_if_then_else(constant_true, constant_false),
        false,
        true,
    )
}

#[test]
fn mk_cons_direct_invoke_1() {
    let src = r#"
        use aiken/builtin

        test mk_cons_1() {
            builtin.cons_list(1, []) == [1]
        }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::list_data().apply(
                    Term::mk_cons()
                        .apply(Term::data(Data::integer(1.into())))
                        .apply(Term::empty_list()),
                ),
            )
            .apply(Term::data(Data::list(vec![Data::integer(1.into())]))),
        false,
        true,
    )
}

#[test]
fn mk_cons_direct_invoke_2() {
    let src = r#"
        use aiken/builtin.{cons_list}

        test mk_cons_2() {
            cons_list(Some(42), [None]) == [Some(42), None]
        }
    "#;

    let none = Data::constr(1, Vec::new());
    let some = Data::constr(0, vec![Data::integer(42.into())]);

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::list_data().apply(Term::mk_cons().apply(Term::data(some.clone())).apply(
                    Term::Constant(
                        Constant::ProtoList(Type::Data, vec![Constant::Data(none.clone())]).into(),
                    ),
                )),
            )
            .apply(Term::data(Data::list(vec![some, none]))),
        false,
        true,
    )
}

#[test]
fn mk_cons_direct_invoke_3() {
    let src = r#"
        use aiken/builtin.{cons_list, i_data, new_pairs}

        test mk_cons_3() {
          cons_list(Pair(i_data(1), i_data(1)), new_pairs()) == [
            Pair(i_data(1), i_data(1)),
          ]
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
                                Constant::Data(Data::integer(1.into())).into(),
                            )
                            .into(),
                        ))
                        .apply(Term::mk_nil_pair_data().apply(Term::unit())),
                ),
            )
            .apply(Term::data(Data::map(vec![(
                Data::integer(1.into()),
                Data::integer(1.into()),
            )]))),
        false,
        true,
    )
}

#[test]
fn mk_nil_pair_data() {
    let src = r#"
        use aiken/builtin.{new_pairs}

        test nil_equals() {
            new_pairs() == new_pairs()
        }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(Term::map_data().apply(Term::mk_nil_pair_data().apply(Term::unit())))
            .apply(Term::map_data().apply(Term::mk_nil_pair_data().apply(Term::unit()))),
        false,
        true,
    )
}

#[test]
fn mk_nil_list_data() {
    let src = r#"
        use aiken/builtin.{new_list}

        test nil_equals() {
            new_list() == new_list()
        }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(Term::list_data().apply(Term::mk_nil_data().apply(Term::unit())))
            .apply(Term::list_data().apply(Term::mk_nil_data().apply(Term::unit()))),
        false,
        true,
    )
}

#[test]
fn mk_pair_data() {
    let src = r#"
        use aiken/builtin.{i_data}

        test nil_equals() {
            builtin.new_pair(i_data(1), i_data(2)).1st == i_data(1)
        }
    "#;

    assert_uplc(
        src,
        Term::equals_data()
            .apply(
                Term::fst_pair().apply(
                    Term::mk_pair_data()
                        .apply(Term::Constant(
                            Constant::Data(Data::integer(1.into())).into(),
                        ))
                        .apply(Term::Constant(
                            Constant::Data(Data::integer(2.into())).into(),
                        )),
                ),
            )
            .apply(Term::Constant(
                Constant::Data(Data::integer(1.into())).into(),
            )),
        false,
        true,
    )
}

#[test]
fn pattern_bytearray() {
    let src = r#"
        test pattern_bytearray() {
            let bytes = "foo"
            when bytes is {
                "bar" -> False
                #[0x66, 0x6f, 0x6f] -> True
                _ -> False
            }
        }
    "#;

    let snd_clause = Term::equals_bytestring()
        .apply(Term::byte_string(vec![0x66, 0x6f, 0x6f]))
        .apply(Term::var("__subject"))
        .delayed_if_then_else(Term::bool(true), Term::bool(false));

    let fst_clause = Term::equals_bytestring()
        .apply(Term::byte_string(vec![0x62, 0x61, 0x72]))
        .apply(Term::var("__subject"))
        .delayed_if_then_else(Term::bool(false), snd_clause);

    let when_clause = fst_clause
        .lambda("__subject")
        .apply(Term::var("__when_var"))
        .lambda("__when_var")
        .apply(Term::var("bytes"));

    let program = when_clause
        .lambda("bytes")
        .apply(Term::byte_string(vec![0x66, 0x6f, 0x6f]))
        // Not sure what this extra lambda is or do?
        .lambda("???")
        .apply(Term::Error.delay());

    assert_uplc(src, program, false, true)
}

#[test]
fn cast_never() {
    let src = r#"
        test never_ok_cast() {
          let none: Option<Void> = None
          let data: Data = none
          expect _: Never = data
        }
    "#;

    let none_or_never = || Term::Constant(Constant::Data(Data::constr(1, vec![])).into());

    let expect_otherwise = Term::Error
        .delayed_trace(Term::string("expect _: Never = data"))
        .delay();

    let assert_constr_index = Term::equals_integer()
        .apply(Term::integer(1.into()))
        .apply(Term::fst_pair().apply(Term::unconstr_data().apply(none_or_never())));

    let assert_empty_fields = |then: Term<Name>, expect_otherwise: Rc<Name>| {
        Term::snd_pair()
            .apply(Term::unconstr_data().apply(none_or_never()))
            .delay_empty_choose_list(then, Term::Var(expect_otherwise))
    };

    let program = expect_otherwise.as_var("expect_:Never=data", |expect_otherwise| {
        let otherwise = Term::Var(expect_otherwise.clone());

        let when_constr = assert_constr_index.delay_true_if_then_else(
            assert_empty_fields(Term::unit(), expect_otherwise.clone()),
            Term::Var(expect_otherwise),
        );

        none_or_never()
            .choose_data(
                when_constr.delay(),
                otherwise.clone(),
                otherwise.clone(),
                otherwise.clone(),
                otherwise,
            )
            .force()
    });

    assert_uplc(src, program, false, true)
}
