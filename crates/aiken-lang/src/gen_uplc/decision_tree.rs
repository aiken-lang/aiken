use std::{cmp::Ordering, rc::Rc};

use itertools::Itertools;

use crate::{
    ast::{Pattern, TypedClause, TypedPattern},
    expr::{PatternConstructor, Type, TypedExpr},
};

#[derive(Clone, Default, Copy)]
struct Occurrence {
    passed_wild_card: bool,
    amount: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Path {
    Pair(usize),
    Tuple(usize),
    List(usize),
}

#[derive(Clone, Debug)]
struct RowItem<'a> {
    path: Vec<Path>,
    pattern: &'a TypedPattern,
}

#[derive(Clone, Debug)]
pub struct Assign {
    path: Vec<Path>,
    assigned: String,
}

#[derive(Clone, Debug)]
struct Row<'a> {
    assigns: Vec<Assign>,
    columns: Vec<RowItem<'a>>,
    then: &'a TypedExpr,
}

#[derive(Clone, Debug)]
struct PatternMatrix<'a> {
    rows: Vec<Row<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CaseTest {
    Constr(PatternConstructor),
    Int(String),
    Bytes(Vec<u8>),
    List(usize, bool),
    Wild,
}

impl PartialOrd for CaseTest {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (CaseTest::Wild, CaseTest::Wild) => Some(Ordering::Equal),
            (CaseTest::Wild, _) => Some(Ordering::Greater),
            (_, CaseTest::Wild) => Some(Ordering::Less),
            (_, _) => Some(Ordering::Equal),
        }
    }
}

impl Ord for CaseTest {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (CaseTest::Wild, CaseTest::Wild) => Ordering::Equal,
            (CaseTest::Wild, _) => Ordering::Greater,
            (_, CaseTest::Wild) => Ordering::Less,
            (_, _) => Ordering::Equal,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DecisionTree<'a> {
    Switch {
        subject_name: String,
        subject_tipo: Rc<Type>,
        path: Vec<Path>,
        cases: Vec<(CaseTest, DecisionTree<'a>)>,
        default: Box<DecisionTree<'a>>,
    },
    Leaf(Vec<Assign>, &'a TypedExpr),
}

fn get_tipo_by_path(mut subject_tipo: Rc<Type>, mut path: &[Path]) -> Rc<Type> {
    while let Some((p, rest)) = path.split_first() {
        subject_tipo = match p {
            Path::Pair(index) | Path::Tuple(index) => {
                subject_tipo.get_inner_types().swap_remove(*index)
            }
            Path::List(_) => subject_tipo.get_inner_types().swap_remove(0),
        };

        path = rest
    }
    subject_tipo
}

fn map_pattern_to_row<'a>(
    pattern: &'a TypedPattern,
    subject_tipo: &Rc<Type>,
    path: Vec<Path>,
) -> (Vec<Assign>, Vec<RowItem<'a>>) {
    let current_tipo = get_tipo_by_path(subject_tipo.clone(), &path);

    let new_columns_added = if current_tipo.is_pair() {
        2
    } else if current_tipo.is_tuple() {
        let Type::Tuple { elems, .. } = current_tipo.as_ref() else {
            unreachable!()
        };
        elems.len()
    } else {
        1
    };

    match pattern {
        Pattern::Var { name, .. } => (
            vec![Assign {
                path: path.clone(),
                assigned: name.clone(),
            }],
            vec![RowItem {
                pattern,
                path: path.clone(),
            }]
            .into_iter()
            .cycle()
            .take(new_columns_added)
            .collect_vec(),
        ),

        Pattern::Assign { name, pattern, .. } => (
            vec![Assign {
                path: path.clone(),
                assigned: name.clone(),
            }],
            map_pattern_to_row(pattern, subject_tipo, path).1,
        ),
        Pattern::Int { .. }
        | Pattern::ByteArray { .. }
        | Pattern::Discard { .. }
        | Pattern::List { .. }
        | Pattern::Constructor { .. } => (
            vec![],
            vec![RowItem {
                pattern,
                path: path.clone(),
            }]
            .into_iter()
            .cycle()
            .take(new_columns_added)
            .collect_vec(),
        ),

        Pattern::Pair { fst, snd, .. } => {
            let mut fst_path = path.clone();
            fst_path.push(Path::Pair(0));
            let mut snd_path = path;
            snd_path.push(Path::Pair(1));

            let (mut assigns, mut patts) = map_pattern_to_row(fst, subject_tipo, fst_path);

            let (assign_snd, patt_snd) = map_pattern_to_row(snd, subject_tipo, snd_path);

            assigns.extend(assign_snd);

            patts.extend(patt_snd);

            (assigns, patts)
        }
        Pattern::Tuple { elems, .. } => {
            elems
                .iter()
                .enumerate()
                .fold((vec![], vec![]), |mut acc, (index, item)| {
                    let mut item_path = path.clone();

                    item_path.push(Path::Tuple(index));

                    let (assigns, patts) = map_pattern_to_row(item, subject_tipo, item_path);

                    acc.0.extend(assigns);
                    acc.1.extend(patts);

                    acc
                })
        }
    }
}

fn match_wild_card(pattern: &TypedPattern) -> bool {
    match pattern {
        Pattern::Var { .. } | Pattern::Discard { .. } => true,
        Pattern::Assign { pattern, .. } => match_wild_card(pattern),
        _ => false,
    }
}

pub fn build_tree<'a>(
    subject_name: &String,
    subject_tipo: &Rc<Type>,
    clauses: &'a [TypedClause],
) -> DecisionTree<'a> {
    let rows = clauses
        .iter()
        .map(|clause| {
            let (assign, row_items) = map_pattern_to_row(&clause.pattern, subject_tipo, vec![]);

            Row {
                assigns: assign.into_iter().collect_vec(),
                columns: row_items,
                then: &clause.then,
            }
        })
        .collect_vec();

    println!("INITIAL ROWS ARE {:#?}", rows);

    do_build_tree(subject_name, subject_tipo, PatternMatrix { rows }, None)
}

fn do_build_tree<'a>(
    subject_name: &String,
    subject_tipo: &Rc<Type>,
    matrix: PatternMatrix<'a>,
    fallback_option: Option<DecisionTree<'a>>,
) -> DecisionTree<'a> {
    let column_length = matrix.rows[0].columns.len();

    assert!(matrix
        .rows
        .iter()
        .all(|row| { row.columns.len() == column_length }));

    let occurrences = [Occurrence::default()].repeat(column_length);

    let occurrences =
        matrix
            .rows
            .iter()
            .fold(occurrences, |mut occurrences: Vec<Occurrence>, row| {
                row.columns
                    .iter()
                    .enumerate()
                    .for_each(|(column_index, row_item)| {
                        let Some(occurrence_col) = occurrences.get_mut(column_index) else {
                            unreachable!()
                        };
                        if !match_wild_card(row_item.pattern) && !occurrence_col.passed_wild_card {
                            occurrence_col.amount += 1;
                        } else {
                            occurrence_col.passed_wild_card = true;
                        }
                    });

                occurrences
            });

    // index and count
    let mut highest_occurrence = (0, 0);

    occurrences.iter().enumerate().for_each(|(index, occ)| {
        if occ.amount > highest_occurrence.1 {
            highest_occurrence.0 = index;
            highest_occurrence.1 = occ.amount;
        }
    });

    let (path, mut collection_vec) = matrix.rows.into_iter().fold(
        (vec![], vec![]),
        |mut collection_vec: (Vec<Path>, Vec<(CaseTest, Vec<Row<'a>>)>), mut item: Row<'a>| {
            if item.columns.is_empty() {
                collection_vec.1.push((CaseTest::Wild, vec![item]));
                return collection_vec;
            }

            let col = item.columns.remove(highest_occurrence.0);

            assert!(!matches!(col.pattern, Pattern::Assign { .. }));

            let (mapped_args, case) = match col.pattern {
                Pattern::Int { value, .. } => (vec![], CaseTest::Int(value.clone())),
                Pattern::ByteArray { value, .. } => (vec![], CaseTest::Bytes(value.clone())),
                Pattern::Var { .. } | Pattern::Discard { .. } => (vec![], CaseTest::Wild),
                Pattern::List { elements, tail, .. } => (
                    elements
                        .iter()
                        .chain(tail.as_ref().map(|tail| tail.as_ref()))
                        .enumerate()
                        .map(|(index, item)| {
                            let mut item_path = col.path.clone();

                            item_path.push(Path::List(index));

                            map_pattern_to_row(item, subject_tipo, item_path)
                        })
                        .collect_vec(),
                    CaseTest::List(elements.len(), tail.is_some()),
                ),

                Pattern::Constructor { .. } => {
                    todo!()
                }
                _ => unreachable!("{:#?}", col.pattern),
            };

            item.assigns
                .extend(mapped_args.iter().flat_map(|x| x.0.clone()));
            item.columns
                .extend(mapped_args.into_iter().flat_map(|x| x.1));

            assert!(
                collection_vec.0.is_empty()
                    || collection_vec.0 == col.path
                    || matches!(case, CaseTest::Wild)
            );

            if collection_vec.0.is_empty() {
                collection_vec.0 = col.path;
            }

            if let Some(entry) = collection_vec.1.iter_mut().find(|item| item.0 == case) {
                entry.1.push(item);
                collection_vec
            } else {
                collection_vec.1.push((case, vec![item]));

                collection_vec
            }
        },
    );

    collection_vec.sort_by(|a, b| a.0.cmp(&b.0));

    let mut collection_iter = collection_vec.into_iter().peekable();

    let cases = collection_iter
        .peeking_take_while(|a| !matches!(a.0, CaseTest::Wild))
        .collect_vec();

    if cases.is_empty() {
        let mut fallback = collection_iter.collect_vec();

        assert!(fallback.len() == 1);

        let mut remaining = fallback.swap_remove(0).1;

        assert!(remaining.len() == 1);

        let row = remaining.swap_remove(0);

        DecisionTree::Leaf(row.assigns, row.then)
    } else {
        let mut fallback = collection_iter
            .map(|x| {
                do_build_tree(
                    subject_name,
                    subject_tipo,
                    PatternMatrix { rows: x.1 },
                    None,
                )
            })
            .collect_vec();
        assert!(fallback.len() == 1 || fallback_option.is_some());

        let fallback = if !fallback.is_empty() {
            fallback.swap_remove(0)
        } else {
            fallback_option.unwrap()
        };

        DecisionTree::Switch {
            subject_name: subject_name.clone(),
            subject_tipo: get_tipo_by_path(subject_tipo.clone(), &path),
            path,
            cases: cases
                .into_iter()
                .map(|x| {
                    (
                        x.0,
                        do_build_tree(
                            subject_name,
                            subject_tipo,
                            PatternMatrix { rows: x.1 },
                            Some(fallback.clone()),
                        ),
                    )
                })
                .collect_vec(),
            default: fallback.into(),
        }
    }
}

#[cfg(test)]
mod tester {
    use std::collections::HashMap;

    use crate::{
        ast::{Definition, ModuleKind, TraceLevel, Tracing, TypedModule, UntypedModule},
        builtins,
        expr::{Type, TypedExpr},
        gen_uplc::decision_tree::build_tree,
        parser,
        tipo::error::{Error, Warning},
        IdGenerator,
    };

    fn parse(source_code: &str) -> UntypedModule {
        let kind = ModuleKind::Lib;
        let (ast, _) = parser::module(source_code, kind).expect("Failed to parse module");
        ast
    }

    fn check_module(
        ast: UntypedModule,
        extra: Vec<(String, UntypedModule)>,
        kind: ModuleKind,
        tracing: Tracing,
    ) -> Result<(Vec<Warning>, TypedModule), (Vec<Warning>, Error)> {
        let id_gen = IdGenerator::new();

        let mut warnings = vec![];

        let mut module_types = HashMap::new();
        module_types.insert("aiken".to_string(), builtins::prelude(&id_gen));
        module_types.insert("aiken/builtin".to_string(), builtins::plutus(&id_gen));

        for (package, module) in extra {
            let mut warnings = vec![];
            let typed_module = module
                .infer(
                    &id_gen,
                    kind,
                    &package,
                    &module_types,
                    Tracing::All(TraceLevel::Verbose),
                    &mut warnings,
                    None,
                )
                .expect("extra dependency did not compile");
            module_types.insert(package.clone(), typed_module.type_info.clone());
        }

        let result = ast.infer(
            &id_gen,
            kind,
            "test/project",
            &module_types,
            tracing,
            &mut warnings,
            None,
        );

        result
            .map(|o| (warnings.clone(), o))
            .map_err(|e| (warnings, e))
    }

    fn check(ast: UntypedModule) -> Result<(Vec<Warning>, TypedModule), (Vec<Warning>, Error)> {
        check_module(ast, Vec::new(), ModuleKind::Lib, Tracing::verbose())
    }

    #[test]
    fn thing() {
        let source_code = r#"
            test thing(){
                when [1, 2, 3] is {
                  [] -> False
                  [4] -> fail
                  [a, 2, b] -> True
                  _ -> False
                }
            }
        "#;

        let (_, ast) = check(parse(source_code)).unwrap();

        let Definition::Test(function) = &ast.definitions[0] else {
            panic!()
        };

        let TypedExpr::When { clauses, .. } = &function.body else {
            panic!()
        };

        let tree = build_tree(&"subject".to_string(), &Type::list(Type::int()), clauses);

        println!("TREE IS {:#?}", tree);
    }

    #[test]
    fn thing2() {
        let source_code = r#"
            test thing(){
                when (1,2,#"",[]) is {
                  (a,b,#"", []) -> True
                  (1,b,#"", [1]) -> False
                  (3,b,#"aa", _) -> 2 == 2
                  _ -> 1 == 1
                }
            }
        "#;

        let (_, ast) = check(parse(source_code)).unwrap();

        let Definition::Test(function) = &ast.definitions[0] else {
            panic!()
        };

        let TypedExpr::When { clauses, .. } = &function.body else {
            panic!()
        };

        let tree = build_tree(
            &"subject".to_string(),
            &Type::tuple(vec![
                Type::int(),
                Type::int(),
                Type::byte_array(),
                Type::list(Type::int()),
            ]),
            clauses,
        );

        println!("TREE IS {:#?}", tree);

        panic!()
    }
}
