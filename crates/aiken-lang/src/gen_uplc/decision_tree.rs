use core::fmt;
use pretty::RcDoc;
use std::{cmp::Ordering, fmt::Display, rc::Rc};

use indexmap::IndexMap;
use itertools::{Either, Itertools, Position};

use crate::{
    ast::{DataTypeKey, Pattern, TypedClause, TypedDataType, TypedPattern},
    expr::{lookup_data_type_by_tipo, Type, TypeVar, TypedExpr},
};

use super::{interner::AirInterner, tree::AirTree};

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
    OpaqueConstr(Rc<Type>),
    List(usize),
    ListTail(usize),
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Path::Pair(i) => {
                write!(f, "pair_{}", i)
            }
            Path::Tuple(i) => {
                write!(f, "tuple_{}", i)
            }
            Path::Constr(_, i) => {
                write!(f, "constr_{}", i)
            }
            Path::OpaqueConstr(_) => write!(f, "opaqueconstr"),
            Path::List(i) => {
                write!(f, "list_{}", i)
            }
            Path::ListTail(i) => {
                write!(f, "listtail_{}", i)
            }
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
            (Path::OpaqueConstr(_), Path::OpaqueConstr(_)) => true,
            _ => false,
        }
    }
}

impl Eq for Path {}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Path {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Path::Pair(a), Path::Pair(b))
            | (Path::Tuple(a), Path::Tuple(b))
            | (Path::List(a), Path::List(b))
            | (Path::ListTail(a), Path::ListTail(b))
            | (Path::Constr(_, a), Path::Constr(_, b)) => a.cmp(b),
            (Path::OpaqueConstr(_), Path::OpaqueConstr(_)) => Ordering::Equal,
            _ => Ordering::Equal,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Assigned {
    pub path: Vec<Path>,
    pub assigned: String,
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

impl CaseTest {
    pub fn get_air_pattern(&self, current_type: Rc<Type>) -> AirTree {
        match self {
            CaseTest::Constr(i) => {
                if current_type.is_bool() {
                    AirTree::bool(1 == *i)
                } else {
                    AirTree::int(i)
                }
            }
            CaseTest::Int(i) => AirTree::int(i),
            CaseTest::Bytes(vec) => AirTree::byte_array(vec.clone()),
            CaseTest::List(_) => unreachable!(),
            CaseTest::ListWithTail(_) => unreachable!(),
            CaseTest::Wild => unreachable!(),
        }
    }
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
        path: Vec<Path>,
        cases: Vec<(CaseTest, DecisionTree<'a>)>,
        default: Option<Box<DecisionTree<'a>>>,
    },
    ListSwitch {
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

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub enum ScopePath {
    Case(usize),
    Fallback,
}

impl PartialOrd for ScopePath {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ScopePath {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (ScopePath::Case(a), ScopePath::Case(b)) => b.cmp(a),
            (ScopePath::Case(_), ScopePath::Fallback) => Ordering::Greater,
            (ScopePath::Fallback, ScopePath::Case(_)) => Ordering::Less,
            (ScopePath::Fallback, ScopePath::Fallback) => Ordering::Equal,
        }
    }
}

#[derive(Eq, Hash, PartialEq, Clone, Debug, Default, PartialOrd, Ord)]
pub struct Scope {
    scope: Vec<ScopePath>,
}

impl Scope {
    pub fn new() -> Self {
        Self { scope: vec![] }
    }

    pub fn push(&mut self, path: ScopePath) {
        self.scope.push(path);
    }

    pub fn pop(&mut self) {
        self.scope.pop();
    }

    pub fn common_ancestor(&mut self, other: &Scope) {
        let scope = std::mem::take(&mut self.scope);

        self.scope = scope
            .into_iter()
            .zip(other.scope.iter())
            .map_while(|(a, b)| if a == *b { Some(a) } else { None })
            .collect_vec()
    }

    pub fn len(&self) -> usize {
        self.scope.len()
    }

    pub fn is_empty(&self) -> bool {
        self.scope.is_empty()
    }
}

enum Marker<'a, 'b> {
    Pop,
    Push(ScopePath, &'b DecisionTree<'a>),
    PopPush(ScopePath, &'b DecisionTree<'a>),
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

    // Please help me with this the way nesting works
    // has me baffled
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            DecisionTree::Switch {
                path,
                cases,
                default,
                ..
            } => RcDoc::text("Switch(")
                .append(
                    path.iter()
                        .fold(RcDoc::line().append(RcDoc::text("path(")), |acc, p| {
                            acc.append(RcDoc::line().append(RcDoc::text(p.to_string()).nest(4)))
                        })
                        .append(RcDoc::line())
                        .append(RcDoc::text(")"))
                        .nest(4),
                )
                .append(
                    cases
                        .iter()
                        .fold(
                            RcDoc::line().append(RcDoc::text("cases(")),
                            |acc, (con, tree)| {
                                acc.append(RcDoc::line())
                                    .append(RcDoc::text(format!("({}): ", con)))
                                    .append(RcDoc::line())
                                    .append(tree.to_doc().nest(4))
                            },
                        )
                        .append(RcDoc::line())
                        .append(RcDoc::text(")"))
                        .nest(4),
                )
                .append(
                    RcDoc::line()
                        .append(RcDoc::text("default : "))
                        .append(RcDoc::line())
                        .append(
                            default
                                .as_ref()
                                .map(|i| i.to_doc())
                                .unwrap_or(RcDoc::text("None")),
                        )
                        .append(RcDoc::line())
                        .nest(4),
                )
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
                        .fold(RcDoc::line().append(RcDoc::text("path(")), |acc, p| {
                            acc.append(RcDoc::line().append(RcDoc::text(p.to_string()).nest(4)))
                        })
                        .append(RcDoc::line())
                        .append(RcDoc::text(")"))
                        .nest(4),
                )
                .append(
                    cases
                        .iter()
                        .fold(
                            RcDoc::line().append(RcDoc::text("cases(")),
                            |acc, (con, tree)| {
                                acc.append(RcDoc::line())
                                    .append(RcDoc::text(format!("({}): ", con)))
                                    .append(RcDoc::line())
                                    .append(tree.to_doc().nest(4))
                            },
                        )
                        .append(RcDoc::line())
                        .append(RcDoc::text(")"))
                        .nest(4),
                )
                .append(
                    tail_cases
                        .iter()
                        .fold(
                            RcDoc::line().append(RcDoc::text("tail_cases(")),
                            |acc, (con, tree)| {
                                acc.append(RcDoc::line())
                                    .append(RcDoc::text(format!("({}): ", con)))
                                    .append(RcDoc::line())
                                    .append(tree.to_doc().nest(4))
                            },
                        )
                        .append(RcDoc::line())
                        .append(RcDoc::text(")"))
                        .nest(4),
                )
                .append(
                    RcDoc::line()
                        .append(RcDoc::text("default : "))
                        .append(RcDoc::line())
                        .append(
                            default
                                .as_ref()
                                .map(|i| i.to_doc())
                                .unwrap_or(RcDoc::text("None")),
                        )
                        .append(RcDoc::line())
                        .nest(4),
                )
                .append(RcDoc::text(")")),
            DecisionTree::HoistedLeaf(name, _) => RcDoc::text(format!("Leaf({})", name)),
            DecisionTree::HoistThen { name, pattern, .. } => RcDoc::text("HoistThen(")
                .append(
                    RcDoc::line()
                        .append(RcDoc::text(format!("name : {}", name)))
                        .append(RcDoc::line())
                        .nest(4),
                )
                .append(
                    RcDoc::line()
                        .append(pattern.to_doc())
                        .append(RcDoc::line())
                        .nest(4),
                )
                .append(RcDoc::text(")")),
        }
    }

    /// For fun I decided to do this without recursion
    /// It doesn't look too bad lol
    fn get_hoist_paths<'b>(&self, names: Vec<&'b String>) -> IndexMap<&'b String, Scope> {
        let mut prev = vec![];

        let mut current_path = Scope::new();

        let mut tree = self;

        let mut scope_map: IndexMap<&String, Scope> =
            names.into_iter().map(|item| (item, Scope::new())).collect();

        loop {
            match tree {
                DecisionTree::Switch { cases, default, .. } => {
                    prev.push(Marker::Pop);

                    if let Some(def) = default {
                        prev.push(Marker::PopPush(ScopePath::Fallback, def.as_ref()));
                    }

                    cases
                        .iter()
                        .enumerate()
                        .rev()
                        .for_each(|(index, (_, detree))| {
                            if index == 0 {
                                prev.push(Marker::Push(ScopePath::Case(index), detree));
                            } else {
                                prev.push(Marker::PopPush(ScopePath::Case(index), detree));
                            }
                        });
                }

                DecisionTree::ListSwitch {
                    cases,
                    tail_cases,
                    default,
                    ..
                } => {
                    prev.push(Marker::Pop);

                    if let Some(def) = default {
                        prev.push(Marker::PopPush(ScopePath::Fallback, def.as_ref()));
                    }

                    tail_cases
                        .iter()
                        .enumerate()
                        .rev()
                        .for_each(|(index, (_, detree))| {
                            if index + cases.len() == 0 {
                                prev.push(Marker::Push(
                                    ScopePath::Case(index + cases.len()),
                                    detree,
                                ));
                            } else {
                                prev.push(Marker::PopPush(
                                    ScopePath::Case(index + cases.len()),
                                    detree,
                                ));
                            }
                        });

                    cases
                        .iter()
                        .enumerate()
                        .rev()
                        .for_each(|(index, (_, detree))| {
                            if index == 0 {
                                prev.push(Marker::Push(ScopePath::Case(index), detree));
                            } else {
                                prev.push(Marker::PopPush(ScopePath::Case(index), detree));
                            }
                        });
                }
                DecisionTree::HoistedLeaf(leaf_name, _) => {
                    let scope_for_name = scope_map
                        .get_mut(leaf_name)
                        .expect("Impossible, Leaf is based off of given names");

                    if scope_for_name.is_empty() {
                        *scope_for_name = current_path.clone();
                    } else {
                        scope_for_name.common_ancestor(&current_path);
                    }
                }
                // These are not generated by do_build_tree, but
                // added afterwards
                DecisionTree::HoistThen { .. } => unreachable!(),
            }

            match prev.pop() {
                Some(Marker::Pop) => {
                    current_path.pop();
                }
                Some(Marker::Push(p, dec_tree)) => {
                    current_path.push(p);

                    tree = dec_tree;
                }
                Some(Marker::PopPush(p, dec_tree)) => {
                    current_path.pop();

                    current_path.push(p);

                    tree = dec_tree;
                }
                // Break out of loop and return the map with all names properly
                // scoped
                None => break,
            };
        }

        scope_map
    }

    // I did recursion here since we need mutable pointers to modify the tree
    fn hoist_by_path(
        &mut self,
        current_path: &mut Scope,
        name_paths: &mut Vec<(String, Scope)>,
        hoistables: &mut IndexMap<String, (Vec<Assigned>, &'a TypedExpr)>,
    ) {
        match self {
            DecisionTree::Switch { cases, default, .. } => {
                cases.iter_mut().enumerate().for_each(|(index, (_, tree))| {
                    current_path.push(ScopePath::Case(index));
                    tree.hoist_by_path(current_path, name_paths, hoistables);
                    current_path.pop();
                });

                current_path.push(ScopePath::Fallback);
                if let Some(def) = default {
                    def.hoist_by_path(current_path, name_paths, hoistables);
                }
                current_path.pop();
            }
            DecisionTree::ListSwitch {
                cases,
                tail_cases,
                default,
                ..
            } => {
                cases.iter_mut().enumerate().for_each(|(index, (_, tree))| {
                    current_path.push(ScopePath::Case(index));
                    tree.hoist_by_path(current_path, name_paths, hoistables);
                    current_path.pop();
                });

                tail_cases
                    .iter_mut()
                    .enumerate()
                    .for_each(|(index, (_, tree))| {
                        current_path.push(ScopePath::Case(index + cases.len()));
                        tree.hoist_by_path(current_path, name_paths, hoistables);
                        current_path.pop();
                    });

                current_path.push(ScopePath::Fallback);
                if let Some(def) = default {
                    def.hoist_by_path(current_path, name_paths, hoistables);
                }
                current_path.pop();
            }
            DecisionTree::HoistedLeaf(_, _) => (),
            DecisionTree::HoistThen { .. } => unreachable!(),
        }

        // We sorted name_paths before passing it in.
        // This ensures we will visit each node in the order we would pop it off
        while let Some(name_path) = name_paths.pop() {
            if name_path.1 == *current_path {
                let (assigns, then) = hoistables.remove(&name_path.0).unwrap();
                let pattern =
                    std::mem::replace(self, DecisionTree::HoistedLeaf("".to_string(), vec![]));

                *self = DecisionTree::HoistThen {
                    name: name_path.0,
                    assigns,
                    pattern: pattern.into(),
                    then,
                };
            } else {
                name_paths.push(name_path);
                break;
            }
        }
    }
}

impl Display for DecisionTree<'_> {
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
        subject_tipo: &Rc<Type>,
        clauses: &'a [TypedClause],
    ) -> DecisionTree<'a> {
        let mut hoistables = IndexMap::new();

        let mut columns_added = vec![];

        let rows = {
            let rows_initial = clauses
                .iter()
                .enumerate()
                .map(|(index, clause)| {
                    // Assigns are split out from patterns so they can be handled
                    // outside of the tree algorithm
                    let (assign, row_items) =
                        self.map_pattern_to_row(&clause.pattern, subject_tipo, vec![]);

                    self.interner.intern(format!("__clause_then_{}", index));
                    let clause_then_name = self
                        .interner
                        .lookup_interned(&format!("__clause_then_{}", index));

                    hoistables.insert(clause_then_name.clone(), (vec![], &clause.then));

                    // Some good ol' mutation to track added columns per relevant path
                    // relevant path indicating a column that has a pattern to test at some point in
                    // one of the rows
                    row_items.iter().for_each(|col| {
                        if !columns_added.contains(&col.path) {
                            columns_added.push(col.path.clone());
                        }
                    });

                    let row = Row {
                        assigns: assign.into_iter().collect_vec(),
                        columns: row_items,
                        then: clause_then_name,
                    };

                    self.interner.pop_text(format!("__clause_then_{}", index));

                    row
                })
                .collect_vec();

            columns_added = columns_added
                .into_iter()
                .sorted_by(|a, b| {
                    let mut a = a.clone();
                    let mut b = b.clone();

                    // It's impossible for duplicates since we check before insertion
                    while let Ordering::Equal = a.first().cmp(&b.first()) {
                        a.remove(0);
                        b.remove(0);
                    }

                    a.first().cmp(&b.first())
                })
                .map(|col| {
                    // remove opaqueconstr paths since they are just type information
                    col.into_iter()
                        .filter(|a| !matches!(a, Path::OpaqueConstr(_)))
                        .collect_vec()
                })
                .collect_vec();

            rows_initial
                .into_iter()
                .map(|mut row| {
                    for (index, path) in columns_added.iter().enumerate() {
                        if !row
                            .columns
                            .get(index)
                            .map(|col| col.path == *path)
                            .unwrap_or(false)
                        {
                            row.columns.insert(index, self.wild_card_pattern.clone());
                        }
                    }

                    row
                })
                .collect_vec()
        };

        let mut tree = self.do_build_tree(subject_tipo, PatternMatrix { rows }, &mut hoistables);

        let scope_map = tree.get_hoist_paths(hoistables.keys().collect_vec());

        let mut name_paths = scope_map
            .into_iter()
            .sorted_by(|a, b| a.1.cmp(&b.1))
            .map(|(name, path)| (name.clone(), path))
            .collect_vec();

        tree.hoist_by_path(&mut Scope::new(), &mut name_paths, &mut hoistables);

        // Do hoisting of thens here
        tree
    }

    fn do_build_tree(
        &mut self,
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

        // Find which column has the most important pattern
        let occurrence_col = highest_occurrence(&matrix, column_length);

        let Some(occurrence_col) = occurrence_col else {
            // No more patterns to match on so we grab the first default row and return that
            let mut fallback = matrix.rows;

            let row = fallback.swap_remove(0);

            let Some((assigns, _)) = then_map.get_mut(&row.then) else {
                unreachable!()
            };

            // This is just to prevent repeated assigning clones for the same fallback
            // used in multiple places
            // So we could just overwrite it everytime too.
            if assigns.is_empty() {
                *assigns = row.assigns.clone();
            }

            return DecisionTree::HoistedLeaf(row.then, row.assigns);
        };

        let mut longest_elems_no_tail = None;
        let mut longest_elems_with_tail = None;
        let mut has_list_pattern = false;

        // List patterns are special so we need more information on length
        // of the longest pattern with and without a tail
        matrix.rows.iter().for_each(|item| {
            let col = &item.columns[occurrence_col];

            if let Pattern::List { elements, tail, .. } = col.pattern {
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
        });

        // Since occurrence_col is Some it means there is a
        // pattern to match on so we also must have a path to the object to test
        // for that pattern
        let path = matrix
            .rows
            .first()
            .unwrap()
            .columns
            .get(occurrence_col)
            .map(|col| col.path.clone())
            .unwrap();

        let specialized_tipo = get_tipo_by_path(subject_tipo.clone(), &path);
        let mut relevant_columns: Vec<(CaseTest, Vec<Vec<Path>>)> = vec![];

        // Time to split on the matrices based on case to test for or lack of
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
                                // Impossible to have a list pattern of only tail element
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
                            lookup_data_type_by_tipo(self.data_types, &specialized_tipo).unwrap();

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
                        // These patterns are fully expanded out when mapping pattern to row
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

                let new_paths = new_cols.iter().map(|col| col.path.clone()).collect_vec();

                if let Some(a) = relevant_columns.iter_mut().find(|a| a.0 == case) {
                    new_paths.iter().for_each(|col| {
                        if !a.1.contains(col) {
                            a.1.push(col.clone());
                        }
                    });
                } else {
                    relevant_columns.push((case.clone(), new_paths.clone()));
                };

                new_cols.extend(row.columns);

                row.columns = new_cols;

                if let CaseTest::Wild = case {
                    default_matrix.push(row.clone());

                    case_matrices.iter_mut().for_each(|(case, matrix)| {
                        let Some(a) = relevant_columns.iter_mut().find(|a| a.0 == *case) else {
                            unreachable!()
                        };

                        new_paths.iter().for_each(|col| {
                            if !a.1.contains(col) {
                                a.1.push(col.clone());
                            }
                        });

                        matrix.push(row.clone());
                    });
                } else if let CaseTest::ListWithTail(tail_case_length) = case {
                    // For lists with tail it's a special case where we also add it to existing patterns
                    // all the way to the longest list pattern with no tail. The reason being that each list greater
                    // than the list with tail pattern could also match with the with list that has x elements + any extra afterwards
                    // See tests below for an example

                    let longest_elems_with_tail = longest_elems_with_tail.unwrap();

                    // You can have a match with all list patterns having a tail or wild card
                    if let Some(longest_elems_no_tail) = longest_elems_no_tail {
                        for elem_count in tail_case_length..=longest_elems_no_tail {
                            let case = CaseTest::List(elem_count);

                            // paths first
                            if let Some(a) = relevant_columns.iter_mut().find(|a| a.0 == case) {
                                new_paths.iter().for_each(|col| {
                                    if !a.1.contains(col) {
                                        a.1.push(col.clone());
                                    }
                                });
                            } else {
                                relevant_columns.push((case.clone(), new_paths.clone()));
                            };

                            let row = row.clone();

                            // now insertion into the appropriate matrix
                            if let Some(entry) =
                                case_matrices.iter_mut().find(|item| item.0 == case)
                            {
                                entry.1.push(row);
                            } else {
                                let mut rows = default_matrix.to_vec();

                                rows.push(row);
                                case_matrices.push((case, rows));
                            }
                        }
                    }

                    // Comment above applies here
                    for elem_count in tail_case_length..=longest_elems_with_tail {
                        let case = CaseTest::ListWithTail(elem_count);

                        if let Some(a) = relevant_columns.iter_mut().find(|a| a.0 == case) {
                            new_paths.iter().for_each(|col| {
                                if !a.1.contains(col) {
                                    a.1.push(col.clone());
                                }
                            });
                        } else {
                            relevant_columns.push((case.clone(), new_paths.clone()));
                        };

                        let row = row.clone();

                        if let Some(entry) = case_matrices.iter_mut().find(|item| item.0 == case) {
                            entry.1.push(row);
                        } else {
                            let mut rows = default_matrix.clone();

                            rows.push(row);
                            case_matrices.push((case, rows));
                        }
                    }
                } else if let Some(entry) = case_matrices.iter_mut().find(|item| item.0 == case) {
                    entry.1.push(row);
                } else {
                    let mut rows = default_matrix.clone();

                    rows.push(row);
                    case_matrices.push((case, rows));
                }

                (default_matrix, case_matrices)
            },
        );

        let (default_relevant_cols, relevant_columns): (Vec<_>, Vec<_>) = relevant_columns
            .into_iter()
            .map(|(case, paths)| {
                (
                    case,
                    paths
                        .into_iter()
                        .sorted_by(|a, b| {
                            let mut a = a.clone();
                            let mut b = b.clone();

                            // It's impossible for duplicates since we check before insertion
                            while let Ordering::Equal = a.first().cmp(&b.first()) {
                                a.remove(0);
                                b.remove(0);
                            }

                            a.first().cmp(&b.first())
                        })
                        .map(|col| {
                            // remove opaqueconstr paths since they are just type information
                            col.into_iter()
                                .filter(|a| !matches!(a, Path::OpaqueConstr(_)))
                                .collect_vec()
                        })
                        .collect_vec(),
                )
            })
            .partition_map(|(case, paths)| match case {
                CaseTest::Wild => Either::Left(paths),
                _ => Either::Right((case, paths)),
            });

        let default_matrix = default_matrix
            .into_iter()
            .map(|mut row| {
                for (index, path) in default_relevant_cols[0].iter().enumerate() {
                    if !row
                        .columns
                        .get(index)
                        .map(|col| col.path == *path)
                        .unwrap_or(false)
                    {
                        row.columns.insert(index, self.wild_card_pattern.clone());
                    }
                }

                row
            })
            .collect_vec();

        let specialized_matrices = specialized_matrices
            .into_iter()
            .map(|(case, matrix)| {
                let Some((_, relevant_cols)) = relevant_columns
                    .iter()
                    .find(|(relevant_case, _)| relevant_case == &case)
                else {
                    unreachable!()
                };
                (
                    case,
                    matrix
                        .into_iter()
                        .map(|mut row| {
                            for (index, path) in relevant_cols.iter().enumerate() {
                                if !row
                                    .columns
                                    .get(index)
                                    .map(|col| col.path == *path)
                                    .unwrap_or(false)
                                {
                                    row.columns.insert(index, self.wild_card_pattern.clone());
                                }
                            }

                            row
                        })
                        .collect_vec(),
                )
            })
            .collect_vec();

        let default_matrix = PatternMatrix {
            rows: default_matrix,
        };

        let fallback_option = if default_matrix.rows.is_empty() {
            None
        } else {
            Some(
                self.do_build_tree(subject_tipo, default_matrix, then_map)
                    .into(),
            )
        };

        if has_list_pattern {
            let (tail_cases, cases): (Vec<_>, Vec<_>) = specialized_matrices
                .into_iter()
                .partition(|(case, _)| matches!(case, CaseTest::ListWithTail(_)));

            DecisionTree::ListSwitch {
                path,
                cases: cases
                    .into_iter()
                    .map(|x| {
                        (
                            x.0,
                            self.do_build_tree(subject_tipo, PatternMatrix { rows: x.1 }, then_map),
                        )
                    })
                    .collect_vec(),
                tail_cases: tail_cases
                    .into_iter()
                    .map(|x| {
                        (
                            x.0,
                            self.do_build_tree(subject_tipo, PatternMatrix { rows: x.1 }, then_map),
                        )
                    })
                    .collect_vec(),
                default: fallback_option,
            }
        } else {
            DecisionTree::Switch {
                path,
                cases: specialized_matrices
                    .into_iter()
                    .map(|x| {
                        (
                            x.0,
                            self.do_build_tree(subject_tipo, PatternMatrix { rows: x.1 }, then_map),
                        )
                    })
                    .collect_vec(),
                default: fallback_option,
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

        match pattern {
            Pattern::Var { name, .. } => (
                vec![Assigned {
                    path: path.clone(),
                    assigned: name.clone(),
                }],
                vec![],
            ),

            Pattern::Assign { name, pattern, .. } => {
                let (mut assigns, patts) =
                    self.map_pattern_to_row(pattern, subject_tipo, path.clone());

                assigns.insert(
                    0,
                    Assigned {
                        path,
                        assigned: name.clone(),
                    },
                );
                (assigns, patts)
            }
            Pattern::Discard { .. } => (vec![], vec![]),

            Pattern::Int { .. } | Pattern::ByteArray { .. } | Pattern::List { .. } => (
                vec![],
                vec![RowItem {
                    pattern,
                    path: path.clone(),
                }],
            ),

            Pattern::Constructor {
                arguments, tipo, ..
            } => {
                let data_type = lookup_data_type_by_tipo(self.data_types, &current_tipo).unwrap();

                let is_transparent =
                    data_type.opaque && data_type.constructors[0].arguments.len() == 1;

                if data_type.constructors.len() == 1 || data_type.is_never() {
                    arguments
                        .iter()
                        .enumerate()
                        .fold((vec![], vec![]), |mut acc, (index, arg)| {
                            let arg_value = &arg.value;

                            let mut item_path = path.clone();

                            if is_transparent {
                                item_path.push(Path::OpaqueConstr(tipo.clone()));
                            } else {
                                item_path.push(Path::Constr(tipo.clone(), index));
                            }

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
                        }],
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
}

pub fn get_tipo_by_path(mut subject_tipo: Rc<Type>, mut path: &[Path]) -> Rc<Type> {
    while let Some((p, rest)) = path.split_first() {
        subject_tipo = match p {
            Path::Pair(index) | Path::Tuple(index) => {
                subject_tipo.get_inner_types().swap_remove(*index)
            }
            Path::List(_) => subject_tipo.get_inner_types().swap_remove(0),
            Path::ListTail(_) => subject_tipo,
            Path::Constr(tipo, index) => tipo.arg_types().unwrap().swap_remove(*index),
            Path::OpaqueConstr(tipo) => tipo.arg_types().unwrap().swap_remove(0),
        };

        path = rest
    }
    match subject_tipo.as_ref() {
        Type::Var { tipo, .. } => match &*tipo.borrow() {
            TypeVar::Unbound { .. } | TypeVar::Generic { .. } => subject_tipo.clone(),
            TypeVar::Link { tipo } => get_tipo_by_path(tipo.clone(), &[]),
        },
        _ => subject_tipo,
    }
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

    // This condition is only true if and only if
    // all columns on the top row are wild cards
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
        expr::TypedExpr,
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

        let TypedExpr::When {
            clauses, subject, ..
        } = &function.body
        else {
            panic!()
        };
        let mut air_interner = AirInterner::new();

        let data_types = IndexMap::new();

        let pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let tree_gen = TreeGen::new(&mut air_interner, &data_types, &pattern);

        let tree = tree_gen.build_tree(&subject.tipo(), clauses);

        println!("{}", tree);
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

        let TypedExpr::When {
            clauses, subject, ..
        } = &function.body
        else {
            panic!()
        };
        let mut air_interner = AirInterner::new();

        let data_types = IndexMap::new();

        let pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let tree_gen = TreeGen::new(&mut air_interner, &data_types, &pattern);

        let tree = tree_gen.build_tree(&subject.tipo(), clauses);

        println!("{}", tree);
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

        let TypedExpr::When {
            clauses, subject, ..
        } = &function.body
        else {
            panic!()
        };

        let mut air_interner = AirInterner::new();

        let data_types = IndexMap::new();

        let pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let tree_gen = TreeGen::new(&mut air_interner, &data_types, &pattern);

        let tree = tree_gen.build_tree(&subject.tipo(), clauses);

        println!("{}", tree);
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

        let TypedExpr::When {
            clauses, subject, ..
        } = &function.body
        else {
            panic!()
        };

        let mut air_interner = AirInterner::new();

        let data_types = IndexMap::new();

        let pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let tree_gen = TreeGen::new(&mut air_interner, &data_types, &pattern);

        let tree = tree_gen.build_tree(&subject.tipo(), clauses);

        println!("{}", tree);
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

        let TypedExpr::When {
            clauses, subject, ..
        } = &function.body
        else {
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

        let tree = tree_gen.build_tree(&subject.tipo(), clauses);

        println!("{}", tree);
    }

    #[test]
    fn thing6() {
        let source_code = r#"
            test thing(){
                when [] is {
                  [] -> 4 == 4
                  [a, 1, ..c] -> True
                  [a, b, c, d, 5, ..f] -> False
                  _ -> 1 == 1
                }
            }
        "#;

        let (_, ast) = check(parse(source_code)).unwrap();

        let Definition::Test(function) = &ast.definitions[0] else {
            panic!()
        };

        let TypedExpr::When {
            clauses, subject, ..
        } = &function.body
        else {
            panic!()
        };

        let mut air_interner = AirInterner::new();

        let data_types = IndexMap::new();

        let pattern = TypedPattern::Discard {
            name: "_".to_string(),
            location: Span::empty(),
        };

        let tree_gen = TreeGen::new(&mut air_interner, &data_types, &pattern);

        let tree = tree_gen.build_tree(&subject.tipo(), clauses);

        println!("{}", tree);
    }
}
