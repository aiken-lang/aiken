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
enum Path {
    Pair(usize),
    Tuple(usize),
}

impl Path {
    pub fn get_index(&self) -> usize {
        match self {
            Path::Pair(u) | Path::Tuple(u) => *u,
        }
    }
}

#[derive(Clone, Debug)]
struct RowItem<'a> {
    path: Vec<Path>,
    pattern: &'a TypedPattern,
}

#[derive(Clone, Debug)]
struct Assign {
    path: Vec<Path>,
    assigned: String,
}

struct Row<'a> {
    assigns: Vec<Assign>,
    columns: Vec<RowItem<'a>>,
    then: &'a TypedExpr,
}

struct PatternMatrix<'a> {
    rows: Vec<Row<'a>>,
}

#[derive(Clone, Eq, PartialEq)]
pub enum CaseTest {
    Constr(PatternConstructor),
    Int(String),
    Bytes(Vec<u8>),
    List(usize),
    Wild,
}

impl PartialOrd for CaseTest {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (CaseTest::Wild, CaseTest::Wild) => Some(Ordering::Equal),
            (CaseTest::Wild, _) => Some(Ordering::Less),
            (_, CaseTest::Wild) => Some(Ordering::Greater),
            (_, _) => Some(Ordering::Equal),
        }
    }
}

impl Ord for CaseTest {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (CaseTest::Wild, CaseTest::Wild) => Ordering::Equal,
            (CaseTest::Wild, _) => Ordering::Less,
            (_, CaseTest::Wild) => Ordering::Greater,
            (_, _) => Ordering::Equal,
        }
    }
}

enum DecisionTree<'a> {
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
        let index = p.get_index();

        subject_tipo = subject_tipo.arg_types().unwrap().swap_remove(index);
        path = rest
    }
    subject_tipo
}

fn map_pattern_to_row<'a>(
    pattern: &'a TypedPattern,
    subject_name: &String,
    subject_tipo: &Rc<Type>,
    path: Vec<Path>,
) -> (Vec<Assign>, Vec<RowItem<'a>>) {
    let current_tipo = get_tipo_by_path(subject_tipo.clone(), &path);

    let new_columns_added = if current_tipo.is_pair() {
        2
    } else if current_tipo.is_tuple() {
        let Type::Tuple { elems, .. } = subject_tipo.as_ref() else {
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
            map_pattern_to_row(pattern, subject_name, subject_tipo, path).1,
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

            let (mut assigns, mut patts) =
                map_pattern_to_row(fst, subject_name, subject_tipo, fst_path);

            let (assign_snd, patt_snd) =
                map_pattern_to_row(snd, subject_name, subject_tipo, snd_path);

            assigns.extend(assign_snd.into_iter());

            patts.extend(patt_snd.into_iter());

            (assigns, patts)
        }
        Pattern::Tuple { elems, .. } => {
            elems
                .iter()
                .enumerate()
                .fold((vec![], vec![]), |mut acc, (index, item)| {
                    let mut item_path = path.clone();

                    item_path.push(Path::Tuple(index));

                    let (assigns, patts) =
                        map_pattern_to_row(item, subject_name, subject_tipo, item_path);

                    acc.0.extend(assigns.into_iter());
                    acc.1.extend(patts.into_iter());

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
    clauses: &'a Vec<TypedClause>,
) -> DecisionTree<'a> {
    let rows = clauses
        .iter()
        .map(|clause| {
            let (assign, row_items) =
                map_pattern_to_row(&clause.pattern, subject_name, subject_tipo, vec![]);

            Row {
                assigns: assign.into_iter().collect_vec(),
                columns: row_items,
                then: &clause.then,
            }
        })
        .collect_vec();

    do_build_tree(subject_name, &subject_tipo, PatternMatrix { rows })
}

pub fn do_build_tree<'a>(
    subject_name: &String,
    subject_tipo: &Rc<Type>,
    matrix: PatternMatrix<'a>,
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
            let col = item.columns.remove(highest_occurrence.0);

            assert!(!matches!(col.pattern, Pattern::Assign { .. }));

            let (mapped_args, case) = match col.pattern {
                Pattern::Int { value, .. } => (vec![], CaseTest::Int(value.clone())),
                Pattern::ByteArray { value, .. } => (vec![], CaseTest::Bytes(value.clone())),
                Pattern::Var { .. } | Pattern::Discard { .. } => (vec![], CaseTest::Wild),
                Pattern::List { elements, .. } => (
                    elements
                        .iter()
                        .enumerate()
                        .map(|(index, item)| {
                            let mut item_path = col.path.clone();

                            item_path.push(Path::Tuple(index));

                            map_pattern_to_row(item, subject_name, subject_tipo, item_path)
                        })
                        .collect_vec(),
                    CaseTest::List(elements.len()),
                ),

                Pattern::Constructor { .. } => {
                    todo!()
                }
                _ => unreachable!("{:#?}", col.pattern),
            };

            item.assigns
                .extend(mapped_args.iter().map(|x| x.0.clone()).flatten());
            item.columns
                .extend(mapped_args.into_iter().map(|x| x.1).flatten());

            assert!(collection_vec.0.is_empty() || collection_vec.0 == col.path);

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
        .map(|x| {
            (
                x.0,
                do_build_tree(subject_name, subject_tipo, PatternMatrix { rows: x.1 }),
            )
        })
        .collect_vec();

    if cases.is_empty() {
        let mut fallback = collection_iter.collect_vec();

        assert!(fallback.len() == 1);

        let mut remaining = fallback.swap_remove(0).1;

        assert!(remaining.len() == 1);

        let thing = remaining.swap_remove(0);

        DecisionTree::Leaf(thing.assigns, thing.then)
    } else {
        let mut fallback = collection_iter
            .map(|x| do_build_tree(subject_name, subject_tipo, PatternMatrix { rows: x.1 }).into())
            .collect_vec();

        assert!(fallback.len() == 1);

        DecisionTree::Switch {
            subject_name: subject_name.clone(),
            subject_tipo: subject_tipo.clone(),
            path,
            cases,
            default: fallback.swap_remove(0),
        }
    }
}
