use core::fmt;
use pretty::RcDoc;
use std::{fmt::Display, rc::Rc};

use indexmap::IndexMap;
use itertools::{Itertools, Position};

use crate::{
    ast::{DataTypeKey, Pattern, TypedClause, TypedDataType, TypedPattern},
    expr::{lookup_data_type_by_tipo, Type, TypedExpr},
};

use super::interner::AirInterner;

const PAIR_NEW_COLUMNS: usize = 2;

const MIN_NEW_COLUMNS: usize = 1;

#[derive(Clone, Default, Copy)]
struct Occurrence {
    passed_wild_card: bool,
    amount: usize,
}

#[derive(Clone, Debug)]
pub enum Path {
    Pair(usize),
    Tuple(usize),
    Constr(Rc<Type>, usize),
    List(usize),
    ListTail(usize),
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Path::Pair(i) => write!(f, "Pair({})", i),
            Path::Tuple(i) => write!(f, "Tuple({})", i),
            Path::Constr(_, i) => write!(f, "Constr({})", i),
            Path::List(i) => write!(f, "List({})", i),
            Path::ListTail(i) => write!(f, "ListTail({})", i),
        }
    }
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Path::Pair(a), Path::Pair(b))
            | (Path::Tuple(a), Path::Tuple(b))
            | (Path::Constr(_, a), Path::Constr(_, b))
            | (Path::List(a), Path::List(b))
            | (Path::ListTail(a), Path::ListTail(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Path {}

#[derive(Clone, Debug)]
pub struct Assigned {
    path: Vec<Path>,
    assigned: String,
}

#[derive(Clone, Debug)]
struct RowItem<'a> {
    path: Vec<Path>,
    pattern: &'a TypedPattern,
}

#[derive(Clone, Debug)]
struct Row<'a> {
    assigns: Vec<Assigned>,
    columns: Vec<RowItem<'a>>,
    then: String,
}

#[derive(Clone, Debug)]
struct PatternMatrix<'a> {
    rows: Vec<Row<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CaseTest {
    Constr(usize),
    Int(String),
    Bytes(Vec<u8>),
    List(usize),
    ListWithTail(usize),
    Wild,
}

impl Display for CaseTest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CaseTest::Constr(i) => write!(f, "Constr({})", i),
            CaseTest::Int(i) => write!(f, "Int({})", i),
            CaseTest::Bytes(vec) => write!(f, "Bytes({:?})", vec),
            CaseTest::List(i) => write!(f, "List({})", i),
            CaseTest::ListWithTail(i) => write!(f, "ListWithTail({})", i),
            CaseTest::Wild => write!(f, "Wild"),
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
        default: Option<Box<DecisionTree<'a>>>,
    },
    ListSwitch {
        subject_name: String,
        subject_tipo: Rc<Type>,
        path: Vec<Path>,
        cases: Vec<(CaseTest, DecisionTree<'a>)>,
        tail_cases: Vec<(CaseTest, DecisionTree<'a>)>,
        default: Option<Box<DecisionTree<'a>>>,
    },
    HoistedLeaf(String, Vec<Assigned>),
    HoistThen {
        name: String,
        assigns: Vec<Assigned>,
        pattern: Box<DecisionTree<'a>>,
        then: &'a TypedExpr,
    },
}

impl<'a> DecisionTree<'a> {
    pub fn to_pretty(&self) -> String {
        let mut w = Vec::new();

        self.to_doc().render(80, &mut w).unwrap();

        String::from_utf8(w)
            .unwrap()
            .lines()
            // This is a hack to deal with blank newlines
            // that end up with a bunch of useless whitespace
            // because of the nesting
            .map(|l| {
                if l.chars().all(|c| c.is_whitespace()) {
                    "".to_string()
                } else {
                    l.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn to_doc(&self) -> RcDoc<()> {
        match self {
            DecisionTree::Switch {
                path,
                cases,
                default,
                ..
            } => RcDoc::text("Switch(")
                .append(RcDoc::line())
                .append(
                    path.iter()
                        .fold(RcDoc::text("path("), |acc, p| {
                            acc.append(RcDoc::line())
                                .append(RcDoc::text(format!("{}", p)))
                        })
                        .append(RcDoc::line_())
                        .nest(2)
                        .append(RcDoc::text(")")),
                )
                .append(RcDoc::line())
                .append(
                    cases
                        .iter()
                        .fold(RcDoc::text("cases("), |acc, (con, tree)| {
                            acc.append(RcDoc::line())
                                .append(format!("({}): ", con))
                                .append(RcDoc::line())
                                .append(tree.to_doc().nest(2))
                        })
                        .append(RcDoc::line_())
                        .nest(2)
                        .append(RcDoc::text(")")),
                )
                .append(RcDoc::line())
                .append(
                    RcDoc::text("default : ")
                        .append(RcDoc::line())
                        .append(
                            default
                                .as_ref()
                                .map(|i| i.to_doc())
                                .unwrap_or(RcDoc::text("None")),
                        )
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            DecisionTree::ListSwitch {
                path,
                cases,
                tail_cases,
                default,
                ..
            } => RcDoc::text("ListSwitch(")
                .append(
                    path.iter()
                        .fold(RcDoc::text("path("), |acc, p| {
                            acc.append(RcDoc::line())
                                .append(RcDoc::text(format!("{}", p)))
                        })
                        .append(RcDoc::line_())
                        .nest(2)
                        .append(RcDoc::text(")")),
                )
                .append(
                    cases
                        .iter()
                        .fold(RcDoc::text("cases("), |acc, (con, tree)| {
                            acc.append(RcDoc::line())
                                .append(format!("({}): ", con))
                                .append(RcDoc::line())
                                .append(tree.to_doc().nest(2))
                        })
                        .append(RcDoc::line_())
                        .nest(2)
                        .append(RcDoc::text(")")),
                )
                .append(
                    tail_cases
                        .iter()
                        .fold(RcDoc::text("tail cases("), |acc, (con, tree)| {
                            acc.append(RcDoc::line())
                                .append(format!("({}): ", con))
                                .append(RcDoc::line())
                                .append(tree.to_doc().nest(2))
                        })
                        .append(RcDoc::line_())
                        .nest(2)
                        .append(RcDoc::text(")")),
                )
                .append(
                    RcDoc::text("default : ")
                        .append(RcDoc::line())
                        .append(
                            default
                                .as_ref()
                                .map(|i| i.to_doc())
                                .unwrap_or(RcDoc::text("None")),
                        )
                        .nest(2),
                )
                .append(RcDoc::line_())
                .append(RcDoc::text(")")),
            DecisionTree::HoistedLeaf(name, _) => RcDoc::text(format!("Leaf({})", name)),
            DecisionTree::HoistThen { .. } => todo!(),
        }
    }
}

impl<'a> Display for DecisionTree<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_pretty())
    }
}

pub struct TreeGen<'a, 'b> {
    interner: &'b mut AirInterner,
    data_types: &'b IndexMap<&'a DataTypeKey, &'a TypedDataType>,
    wild_card_pattern: RowItem<'a>,
}

impl<'a, 'b> TreeGen<'a, 'b> {
    pub fn new(
        interner: &'b mut AirInterner,
        data_types: &'b IndexMap<&'a DataTypeKey, &'a TypedDataType>,
        wild_card_pattern: &'a TypedPattern,
    ) -> Self {
        TreeGen {
            interner,
            data_types,
            wild_card_pattern: RowItem {
                path: vec![],
                pattern: wild_card_pattern,
            },
        }
    }

    pub fn build_tree(
        mut self,
        subject_name: &String,
        subject_tipo: &Rc<Type>,
        clauses: &'a [TypedClause],
    ) -> DecisionTree<'a> {
        let mut clause_then_map = IndexMap::new();

        let rows = clauses
            .iter()
            .enumerate()
            .map(|(index, clause)| {
                let (assign, row_items) =
                    self.map_pattern_to_row(&clause.pattern, subject_tipo, vec![]);

                self.interner.intern(format!("__clause_then_{}", index));
                let clause_then_name = self
                    .interner
                    .lookup_interned(&format!("__clause_then_{}", index));

                clause_then_map.insert(clause_then_name.clone(), (vec![], &clause.then));

                let row = Row {
                    assigns: assign.into_iter().collect_vec(),
                    columns: row_items,
                    then: clause_then_name,
                };

                self.interner.pop_text(format!("__clause_then_{}", index));

                row
            })
            .collect_vec();

        let tree = self.do_build_tree(
            subject_name,
            subject_tipo,
            PatternMatrix { rows },
            &mut clause_then_map,
        );

        // Do hoisting of thens here
        tree
    }

    fn do_build_tree(
        &mut self,
        subject_name: &String,
        subject_tipo: &Rc<Type>,
        matrix: PatternMatrix<'a>,
        then_map: &mut IndexMap<String, (Vec<Assigned>, &'a TypedExpr)>,
    ) -> DecisionTree<'a> {
        let column_length = matrix.rows[0].columns.len();
        // First step make sure all rows have same number of columns
        // or something went wrong
        assert!(matrix
            .rows
            .iter()
            .all(|row| { row.columns.len() == column_length }));

        let occurrence_col = highest_occurrence(&matrix, column_length);
        // Find which column has the most important pattern

        let Some(occurrence_col) = occurrence_col else {
            // No more patterns to match on so we grab the first default row and return that
            let mut fallback = matrix.rows;

            let row = fallback.swap_remove(0);

            let Some((assigns, _)) = then_map.get_mut(&row.then) else {
                unreachable!()
            };

            if assigns.is_empty() {
                *assigns = row.assigns.clone();
            }

            return DecisionTree::HoistedLeaf(row.then, row.assigns);
        };

        let mut longest_elems_no_tail = None;
        let mut longest_elems_with_tail = None;
        let mut has_list_pattern = false;

        // List patterns are special so we need more information on length
        matrix.rows.iter().for_each(|item| {
            let col = &item.columns[occurrence_col];

            match col.pattern {
                Pattern::List { elements, tail, .. } => {
                    has_list_pattern = true;
                    if tail.is_none() {
                        match longest_elems_no_tail {
                            Some(elems_count) => {
                                if elems_count < elements.len() {
                                    longest_elems_no_tail = Some(elements.len());
                                }
                            }
                            None => {
                                longest_elems_no_tail = Some(elements.len());
                            }
                        }
                    } else {
                        match longest_elems_with_tail {
                            Some(elems_count) => {
                                if elems_count < elements.len() {
                                    longest_elems_with_tail = Some(elements.len());
                                }
                            }
                            None => {
                                longest_elems_with_tail = Some(elements.len());
                            }
                        }
                    }
                }
                _ => (),
            }
        });

        let path = matrix
            .rows
            .get(0)
            .unwrap()
            .columns
            .get(occurrence_col)
            .map(|col| col.path.clone())
            .unwrap_or(vec![]);

        let specialized_tipo = get_tipo_by_path(subject_tipo.clone(), &path);

        let (default_matrix, specialized_matrices) = matrix.rows.into_iter().fold(
            (vec![], vec![]),
            |(mut default_matrix, mut case_matrices): (Vec<Row>, Vec<(CaseTest, Vec<Row>)>),
             mut row| {
                // For example in the case of matching on []
                if row.columns.is_empty() {
                    default_matrix.push(row);
                    return (default_matrix, case_matrices);
                }

                let col = row.columns.remove(occurrence_col);

                let (case, remaining_patts) = match col.pattern {
                    Pattern::Var { .. } | Pattern::Discard { .. } => (CaseTest::Wild, vec![]),
                    Pattern::Int { value, .. } => (CaseTest::Int(value.clone()), vec![]),
                    Pattern::ByteArray { value, .. } => (CaseTest::Bytes(value.clone()), vec![]),
                    Pattern::List { elements, tail, .. } => (
                        if tail.is_none() {
                            CaseTest::List(elements.len())
                        } else {
                            CaseTest::ListWithTail(elements.len())
                        },
                        elements
                            .iter()
                            .chain(tail.as_ref().map(|tail| tail.as_ref()))
                            .enumerate()
                            .with_position()
                            .map(|elem| match elem {
                                Position::First((index, element))
                                | Position::Middle((index, element))
                                | Position::Only((index, element)) => {
                                    let mut item_path = col.path.clone();

                                    item_path.push(Path::List(index));

                                    self.map_pattern_to_row(element, subject_tipo, item_path)
                                }

                                Position::Last((index, element)) => {
                                    if tail.is_none() {
                                        let mut item_path = col.path.clone();

                                        item_path.push(Path::List(index));

                                        self.map_pattern_to_row(element, subject_tipo, item_path)
                                    } else {
                                        let mut item_path = col.path.clone();

                                        item_path.push(Path::ListTail(index));

                                        self.map_pattern_to_row(element, subject_tipo, item_path)
                                    }
                                }
                            })
                            .collect_vec(),
                    ),

                    Pattern::Constructor {
                        name,
                        arguments,
                        tipo,
                        ..
                    } => {
                        let data_type =
                            lookup_data_type_by_tipo(&self.data_types, &specialized_tipo).unwrap();

                        let (constr_index, _) = data_type
                            .constructors
                            .iter()
                            .enumerate()
                            .find(|(_, dt)| &dt.name == name)
                            .unwrap();

                        (
                            CaseTest::Constr(constr_index),
                            arguments
                                .iter()
                                .enumerate()
                                .map(|(index, arg)| {
                                    let mut item_path = col.path.clone();

                                    item_path.push(Path::Constr(tipo.clone(), index));

                                    self.map_pattern_to_row(&arg.value, subject_tipo, item_path)
                                })
                                .collect_vec(),
                        )
                    }
                    Pattern::Tuple { .. } | Pattern::Pair { .. } | Pattern::Assign { .. } => {
                        unreachable!("{:#?}", col.pattern)
                    }
                };

                // Assert path is the same for each specialized row
                assert!(path == col.path || matches!(case, CaseTest::Wild));

                // expand assigns by newly added ones
                row.assigns
                    .extend(remaining_patts.iter().flat_map(|x| x.0.clone()));

                // Add inner patterns to existing row
                let mut new_cols = remaining_patts.into_iter().flat_map(|x| x.1).collect_vec();

                let added_columns = new_cols.len();

                // Pop off tail so that it aligns more easily with other list patterns
                if matches!(case, CaseTest::ListWithTail(_)) {
                    new_cols.pop();
                }

                new_cols.extend(row.columns);

                row.columns = new_cols;

                if let CaseTest::Wild = case {
                    let current_wild_cols = row.columns.len();
                    default_matrix.push(row.clone());

                    case_matrices.iter_mut().for_each(|(_, matrix)| {
                        let mut row = row.clone();
                        let total_cols = matrix[0].columns.len();

                        if total_cols != 0 {
                            let added_columns = total_cols - current_wild_cols;

                            for _ in 0..added_columns {
                                row.columns.insert(0, self.wild_card_pattern.clone());
                            }

                            matrix.push(row);
                        }
                    });
                } else if let CaseTest::ListWithTail(case_length) = case {
                    // For lists with tail it's a special case where we also add it to existing patterns
                    // all the way to the longest element. The reason being that each list size greater
                    // than the list with tail could also match with could also match depending on the inner pattern.
                    // See tests below for an example
                    if let Some(longest_elems_no_tail) = longest_elems_no_tail {
                        for elem_count in case_length..=longest_elems_no_tail {
                            let case = CaseTest::List(elem_count);

                            let mut row = row.clone();

                            for _ in 0..(elem_count - case_length) {
                                row.columns
                                    .insert(case_length, self.wild_card_pattern.clone());
                            }

                            self.insert_case(
                                &mut case_matrices,
                                case,
                                &default_matrix,
                                row,
                                added_columns,
                            );
                        }
                    }

                    let Some(longest_elems_with_tail) = longest_elems_with_tail else {
                        unreachable!()
                    };

                    for elem_count in case_length..=longest_elems_with_tail {
                        let case = CaseTest::ListWithTail(elem_count);

                        let mut row = row.clone();

                        for _ in 0..(elem_count - case_length) {
                            row.columns
                                .insert(case_length, self.wild_card_pattern.clone());
                        }

                        self.insert_case(
                            &mut case_matrices,
                            case,
                            &default_matrix,
                            row,
                            added_columns,
                        );
                    }
                } else {
                    self.insert_case(
                        &mut case_matrices,
                        case,
                        &default_matrix,
                        row,
                        added_columns,
                    );
                }

                (default_matrix, case_matrices)
            },
        );

        let default_matrix = PatternMatrix {
            rows: default_matrix,
        };

        if has_list_pattern {
            // Since the list_tail case might cover the rest of the possible matches extensively
            // then fallback is optional here
            let fallback_option = if default_matrix.rows.is_empty() {
                None
            } else {
                Some(
                    self.do_build_tree(
                        subject_name,
                        subject_tipo,
                        // Since everything after this point had a wild card on or above
                        // the row for the selected column in front. Then we ignore the
                        // cases and continue to check other columns.
                        default_matrix,
                        then_map,
                    )
                    .into(),
                )
            };

            let (tail_cases, cases): (Vec<_>, Vec<_>) = specialized_matrices
                .into_iter()
                .partition(|(case, _)| matches!(case, CaseTest::ListWithTail(_)));

            DecisionTree::ListSwitch {
                subject_name: subject_name.clone(),
                subject_tipo: specialized_tipo.clone(),
                path,
                cases: cases
                    .into_iter()
                    .map(|x| {
                        (
                            x.0,
                            self.do_build_tree(
                                subject_name,
                                subject_tipo,
                                PatternMatrix { rows: x.1 },
                                then_map,
                            ),
                        )
                    })
                    .collect_vec(),
                tail_cases: tail_cases
                    .into_iter()
                    .map(|x| {
                        (
                            x.0,
                            self.do_build_tree(
                                subject_name,
                                subject_tipo,
                                PatternMatrix { rows: x.1 },
                                then_map,
                            ),
                        )
                    })
                    .collect_vec(),
                default: fallback_option,
            }
        } else {
            let fallback_option = if default_matrix.rows.is_empty() {
                None
            } else {
                Some(
                    self.do_build_tree(
                        subject_name,
                        subject_tipo,
                        // Since everything after this point had a wild card on or above
                        // the row for the selected column in front. Then we ignore the
                        // cases and continue to check other columns.
                        default_matrix,
                        then_map,
                    )
                    .into(),
                )
            };

            DecisionTree::Switch {
                subject_name: subject_name.clone(),
                subject_tipo: specialized_tipo.clone(),
                path,
                cases: specialized_matrices
                    .into_iter()
                    .map(|x| {
                        (
                            x.0,
                            self.do_build_tree(
                                subject_name,
                                subject_tipo,
                                PatternMatrix { rows: x.1 },
                                then_map,
                            ),
                        )
                    })
                    .collect_vec(),
                default: fallback_option.into(),
            }
        }
    }

    fn map_pattern_to_row(
        &self,
        pattern: &'a TypedPattern,
        subject_tipo: &Rc<Type>,
        path: Vec<Path>,
    ) -> (Vec<Assigned>, Vec<RowItem<'a>>) {
        let current_tipo = get_tipo_by_path(subject_tipo.clone(), &path);

        let new_columns_added = if current_tipo.is_pair() {
            PAIR_NEW_COLUMNS
        } else if current_tipo.is_tuple() {
            let Type::Tuple { elems, .. } = current_tipo.as_ref() else {
                unreachable!()
            };
            elems.len()
        } else if let Some(data) = lookup_data_type_by_tipo(self.data_types, subject_tipo) {
            if data.constructors.len() == 1 {
                data.constructors[0].arguments.len()
            } else {
                MIN_NEW_COLUMNS
            }
        } else {
            MIN_NEW_COLUMNS
        };

        match pattern {
            Pattern::Var { name, .. } => (
                vec![Assigned {
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
                vec![Assigned {
                    path: path.clone(),
                    assigned: name.clone(),
                }],
                self.map_pattern_to_row(pattern, subject_tipo, path).1,
            ),
            Pattern::Int { .. }
            | Pattern::ByteArray { .. }
            | Pattern::Discard { .. }
            | Pattern::List { .. } => (
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

            Pattern::Constructor {
                arguments, tipo, ..
            } => {
                let data_type = lookup_data_type_by_tipo(self.data_types, &current_tipo).unwrap();

                if data_type.constructors.len() == 1 {
                    arguments
                        .iter()
                        .enumerate()
                        .fold((vec![], vec![]), |mut acc, (index, arg)| {
                            let arg_value = &arg.value;

                            let mut item_path = path.clone();

                            item_path.push(Path::Constr(tipo.clone(), index));

                            let (assigns, patts) =
                                self.map_pattern_to_row(arg_value, subject_tipo, item_path);

                            acc.0.extend(assigns);
                            acc.1.extend(patts);

                            acc
                        })
                } else {
                    (
                        vec![],
                        vec![RowItem {
                            pattern,
                            path: path.clone(),
                        }]
                        .into_iter()
                        .cycle()
                        .take(new_columns_added)
                        .collect_vec(),
                    )
                }
            }

            Pattern::Pair { fst, snd, .. } => {
                let mut fst_path = path.clone();
                fst_path.push(Path::Pair(0));
                let mut snd_path = path;
                snd_path.push(Path::Pair(1));

                let (mut assigns, mut patts) = self.map_pattern_to_row(fst, subject_tipo, fst_path);

                let (assign_snd, patt_snd) = self.map_pattern_to_row(snd, subject_tipo, snd_path);

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

                        let (assigns, patts) =
                            self.map_pattern_to_row(item, subject_tipo, item_path);

                        acc.0.extend(assigns);
                        acc.1.extend(patts);

                        acc
                    })
            }
        }
    }

    fn insert_case(
        &self,
        case_matrices: &mut Vec<(CaseTest, Vec<Row<'a>>)>,
        case: CaseTest,
        default_matrix: &Vec<Row<'a>>,
        new_row: Row<'a>,
        added_columns: usize,
    ) {
        if let Some(entry) = case_matrices.iter_mut().find(|item| item.0 == case) {
            entry.1.push(new_row);
        } else {
            let mut rows = default_matrix.clone();

            for _ in 0..added_columns {
                for row in &mut rows {
                    row.columns.insert(0, self.wild_card_pattern.clone());
                }
            }

            rows.push(new_row);
            case_matrices.push((case, rows));
        }
    }
}

fn get_tipo_by_path(mut subject_tipo: Rc<Type>, mut path: &[Path]) -> Rc<Type> {
    while let Some((p, rest)) = path.split_first() {
        subject_tipo = match p {
            Path::Pair(index) | Path::Tuple(index) => {
                subject_tipo.get_inner_types().swap_remove(*index)
            }
            Path::List(_) => subject_tipo.get_inner_types().swap_remove(0),
            Path::ListTail(_) => subject_tipo,
            Path::Constr(tipo, index) => tipo.arg_types().unwrap().swap_remove(*index),
        };

        path = rest
    }
    subject_tipo
}

fn match_wild_card(pattern: &TypedPattern) -> bool {
    match pattern {
        Pattern::Var { .. } | Pattern::Discard { .. } => true,
        Pattern::Assign { pattern, .. } => match_wild_card(pattern),
        _ => false,
    }
}

// A function to get which column has the most pattern matches before a wild card
// Returns none if all columns in the first row are wild cards
fn highest_occurrence(matrix: &PatternMatrix, column_length: usize) -> Option<usize> {
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

    if highest_occurrence.1 == 0 {
        None
    } else {
        Some(highest_occurrence.0)
    }
}

#[cfg(test)]
mod tester {
    use std::collections::HashMap;

    use indexmap::IndexMap;

    use crate::{
        ast::{
            Definition, ModuleKind, Span, TraceLevel, Tracing, TypedModule, TypedPattern,
            UntypedModule,
        },
        builtins,
        expr::{Type, TypedExpr},
        gen_uplc::{decision_tree::TreeGen, interner::AirInterner},
        parser,
        tipo::error::{Error, Warning},
        utils, IdGenerator,
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
        let mut air_interner = AirInterner::new();

        let data_types = IndexMap::new();

        let pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let tree_gen = TreeGen::new(&mut air_interner, &data_types, &pattern);

        let tree = tree_gen.build_tree(&"subject".to_string(), &Type::list(Type::int()), clauses);

        println!("{:#?}", tree);
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
        let mut air_interner = AirInterner::new();

        let data_types = IndexMap::new();

        let pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let tree_gen = TreeGen::new(&mut air_interner, &data_types, &pattern);

        let tree = tree_gen.build_tree(
            &"subject".to_string(),
            &Type::tuple(vec![
                Type::int(),
                Type::int(),
                Type::byte_array(),
                Type::list(Type::int()),
            ]),
            clauses,
        );

        println!("{:#?}", tree);
    }

    #[test]
    fn thing3() {
        let source_code = r#"
            test thing(){
                when (1,2,#"",[]) is {
                  (2,b,#"", []) -> 4 == 4
                  (a,b,#"", [2, ..y]) -> True
                  (1,b,#"", [a]) -> False
                  (3,b,#"aa", [x, y, ..z]) -> 2 == 2
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

        let mut air_interner = AirInterner::new();

        let data_types = IndexMap::new();

        let pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let tree_gen = TreeGen::new(&mut air_interner, &data_types, &pattern);

        let tree = tree_gen.build_tree(
            &"subject".to_string(),
            &Type::tuple(vec![
                Type::int(),
                Type::int(),
                Type::byte_array(),
                Type::list(Type::int()),
            ]),
            clauses,
        );

        println!("{:#?}", tree);
    }

    #[test]
    fn thing4() {
        let source_code = r#"
            test thing(){
                when (1,2,#"",[]) is {
                  (2,b,#"", []) -> 4 == 4
                  (a,b,#"", [2, ..y]) -> True
                  (1,b,#"", [a]) -> False
                  (3,b,#"aa", [x, y, ..z]) -> 2 == 2
                  (3,b, c, [x, 3 as q]) -> fail
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

        let mut air_interner = AirInterner::new();

        let data_types = IndexMap::new();

        let pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let tree_gen = TreeGen::new(&mut air_interner, &data_types, &pattern);

        let tree = tree_gen.build_tree(
            &"subject".to_string(),
            &Type::tuple(vec![
                Type::int(),
                Type::int(),
                Type::byte_array(),
                Type::list(Type::int()),
            ]),
            clauses,
        );

        println!("{}", tree);
        panic!("SUPPPPPPPPPPPPPPPPPPPPPPPER DOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOONE");
    }

    #[test]
    fn thing5() {
        let source_code = r#"
            test thing(){
                when (1,[],#"",None) is {
                  (2,b,#"", Some(Seeded { choices: #"", .. })) -> 4 == 4
                  (a,b,#"", None) -> True
                  (1,b,#"", Some(Seeded{ choices: #"", ..})) -> False
                  (3,b,#"aa", Some(Replayed(..))) -> 2 == 2
                  (3,b, c, y) -> fail
                  _ -> 1 == 1
                }
            }
        "#;

        let (_, ast) = check(parse(source_code)).unwrap();

        let Definition::Test(function) = &ast.definitions[0] else {
            panic!()
        };

        let TypedExpr::When { clauses, tipo, .. } = &function.body else {
            panic!()
        };

        let mut air_interner = AirInterner::new();

        let id_gen = IdGenerator::new();

        let data_types = builtins::prelude_data_types(&id_gen);

        let pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let data_types = utils::indexmap::as_ref_values(&data_types);

        let tree_gen = TreeGen::new(&mut air_interner, &data_types, &pattern);

        let tree = tree_gen.build_tree(
            &"subject".to_string(),
            &Type::tuple(vec![
                Type::int(),
                Type::int(),
                Type::byte_array(),
                Type::option(Type::prng()),
            ]),
            clauses,
        );

        println!("{}", tree);
        panic!("SUPPPPPPPPPPPPPPPPPPPPPPPER DOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOONE");
    }
}
