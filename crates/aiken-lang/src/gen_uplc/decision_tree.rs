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

#[derive(Clone)]
struct RowItem<'a> {
    assign: Option<String>,
    pattern: &'a TypedPattern,
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
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (CaseTest::Wild, CaseTest::Wild) => Ordering::Equal,
            (CaseTest::Wild, _) => Ordering::Less,
            (_, CaseTest::Wild) => Ordering::Greater,
            (_, _) => Ordering::Equal,
        }
    }
}

struct Assign {
    subject_name: String,
    subject_tuple_index: Option<usize>,
    assigned: String,
}

enum DecisionTree {
    Switch {
        subject_name: String,
        subject_tuple_index: Option<usize>,
        subject_tipo: Rc<Type>,
        column_to_test: usize,
        cases: Vec<(CaseTest, DecisionTree)>,
        default: Box<DecisionTree>,
    },
    Leaf(TypedExpr),
}

struct Row<'a> {
    assigns: Vec<Assign>,
    columns: Vec<RowItem<'a>>,
    then: &'a TypedExpr,
}

struct PatternMatrix<'a> {
    rows: Vec<Row<'a>>,
}

fn map_to_row<'a>(
    pattern: &'a TypedPattern,
    subject_name: &String,
    column_count: usize,
) -> Vec<RowItem<'a>> {
    match pattern {
        Pattern::Var { name, .. } => vec![RowItem {
            assign: Some(name.clone()),
            pattern,
        }]
        .into_iter()
        .cycle()
        .take(column_count)
        .collect_vec(),

        Pattern::Assign { name, pattern, .. } => {
            let p = map_to_row(pattern, subject_name, column_count);
            p.into_iter()
                .map(|mut item| {
                    item.assign = Some(name.clone());
                    item
                })
                .collect_vec()
        }
        Pattern::Int { .. }
        | Pattern::ByteArray { .. }
        | Pattern::Discard { .. }
        | Pattern::List { .. }
        | Pattern::Constructor { .. } => vec![RowItem {
            assign: None,
            pattern,
        }]
        .into_iter()
        .cycle()
        .take(column_count)
        .collect_vec(),

        Pattern::Pair { fst, snd, .. } => vec![
            RowItem {
                assign: None,
                pattern: fst,
            },
            RowItem {
                assign: None,
                pattern: snd,
            },
        ],
        Pattern::Tuple { elems, .. } => elems
            .iter()
            .map(|elem| RowItem {
                assign: None,
                pattern: elem,
            })
            .collect_vec(),
    }
}

fn match_wild_card(pattern: &TypedPattern) -> bool {
    match pattern {
        Pattern::Var { .. } | Pattern::Discard { .. } => true,
        Pattern::Assign { pattern, .. } => match_wild_card(pattern),
        _ => false,
    }
}

pub fn build_tree(
    subject_name: &String,
    subject_tipo: Rc<Type>,
    clauses: &Vec<TypedClause>,
) -> DecisionTree {
    let column_count = if subject_tipo.is_pair() {
        2
    } else if subject_tipo.is_tuple() {
        let Type::Tuple { elems, .. } = subject_tipo.as_ref() else {
            unreachable!()
        };
        elems.len()
    } else {
        1
    };

    let rows = clauses
        .iter()
        .map(|clause| {
            let row_items = map_to_row(&clause.pattern, subject_name, column_count);

            Row {
                assigns: vec![],
                columns: row_items,
                then: &clause.then,
            }
        })
        .collect_vec();

    let subject_per_column = if column_count > 1 {
        (0..column_count)
            .map(|index| (subject_name.clone(), Some(index)))
            .collect_vec()
    } else {
        vec![(subject_name.clone(), None)]
    };

    do_build_tree(
        subject_name,
        subject_tipo,
        subject_per_column,
        PatternMatrix { rows },
    )
}

pub fn do_build_tree<'a>(
    subject_name: &String,
    subject_tipo: Rc<Type>,
    subject_per_column: Vec<(String, Option<usize>)>,
    matrix: PatternMatrix<'a>,
) -> DecisionTree {
    let column_count = if subject_tipo.is_pair() {
        2
    } else if subject_tipo.is_tuple() {
        let Type::Tuple { elems, .. } = subject_tipo.as_ref() else {
            unreachable!()
        };
        elems.len()
    } else {
        1
    };

    let occurrences = [Occurrence::default()].repeat(column_count);

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

    if column_count > 1 {
        DecisionTree::Switch {
            subject_name: subject_name.clone(),
            subject_tuple_index: None,
            subject_tipo: subject_tipo.clone(),
            column_to_test: highest_occurrence.0,
            cases: todo!(),
            default: todo!(),
        }
    } else {
        let mut collection_vec = matrix.rows.into_iter().fold(
            vec![],
            |mut collection_vec: Vec<(CaseTest, Vec<Row<'a>>)>, mut item: Row<'a>| {
                let col = item.columns.remove(highest_occurrence.0);
                let mut patt = col.pattern;

                if let Pattern::Assign { pattern, .. } = patt {
                    patt = pattern;
                }

                if let Some(assign) = col.assign {
                    item.assigns.push(Assign {
                        subject_name: subject_name.clone(),
                        subject_tuple_index: None,
                        assigned: assign,
                    });
                }

                let case = match patt {
                    Pattern::Int { value, .. } => CaseTest::Int(value.clone()),
                    Pattern::ByteArray { value, .. } => CaseTest::Bytes(value.clone()),
                    Pattern::Var { .. } | Pattern::Discard { .. } => CaseTest::Wild,
                    Pattern::List { elements, .. } => CaseTest::List(elements.len()),
                    Pattern::Constructor { constructor, .. } => {
                        CaseTest::Constr(constructor.clone())
                    }
                    Pattern::Pair { .. } => todo!(),
                    Pattern::Tuple { .. } => todo!(),
                    _ => unreachable!(),
                };

                if let Some(index) = collection_vec.iter().position(|item| item.0 == case) {
                    let entry = collection_vec.get_mut(index).unwrap();

                    entry.1.push(item);
                    collection_vec
                } else {
                    collection_vec.push((case, vec![item]));

                    collection_vec
                }
            },
        );

        collection_vec.sort_by(|a, b| a.0.cmp(&b.0));
        let mut collection_iter = collection_vec.into_iter().peekable();

        let cases = collection_iter
            .peeking_take_while(|a| !matches!(a.0, CaseTest::Wild))
            .collect_vec();

        let mut fallback = collection_iter.collect_vec();

        assert!(fallback.len() == 1);

        let fallback_matrix = PatternMatrix {
            rows: fallback.remove(0).1,
        };

        DecisionTree::Switch {
            subject_name: subject_name.clone(),
            subject_tuple_index: None,
            subject_tipo: subject_tipo.clone(),
            column_to_test: highest_occurrence.0,
            cases: todo!(),
            default: todo!(),
        }
    };

    todo!()
}
