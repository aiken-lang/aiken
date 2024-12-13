use std::{fmt::Display, rc::Rc};

use itertools::Itertools;
use uplc::{builder::CONSTR_FIELDS_EXPOSER, builtins::DefaultFunction};

use crate::expr::Type;

use super::{
    decision_tree::{get_tipo_by_path, CaseTest, Path},
    tree::AirTree,
};

#[derive(Clone, Debug)]
pub enum Builtin {
    HeadList(Rc<Type>),
    ExtractField(Rc<Type>),
    TailList,
    UnConstrFields,
    FstPair(Rc<Type>),
    SndPair(Rc<Type>),
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Builtin::HeadList(_), Builtin::HeadList(_))
                | (Builtin::ExtractField(_), Builtin::ExtractField(_))
                | (Builtin::TailList, Builtin::TailList)
                | (Builtin::UnConstrFields, Builtin::UnConstrFields)
                | (Builtin::FstPair(_), Builtin::FstPair(_))
                | (Builtin::SndPair(_), Builtin::SndPair(_))
        )
    }
}

impl Eq for Builtin {}

impl Builtin {
    fn produce_air(self, arg: AirTree) -> AirTree {
        match self {
            Builtin::HeadList(t) => AirTree::builtin(DefaultFunction::HeadList, t, vec![arg]),
            Builtin::ExtractField(t) => AirTree::extract_field(t, arg),
            Builtin::TailList => AirTree::builtin(
                DefaultFunction::TailList,
                Type::list(Type::data()),
                vec![arg],
            ),
            Builtin::UnConstrFields => AirTree::call(
                AirTree::local_var(
                    CONSTR_FIELDS_EXPOSER,
                    Type::function(vec![Type::data()], Type::list(Type::data())),
                ),
                Type::list(Type::data()),
                vec![arg],
            ),

            Builtin::FstPair(t) => AirTree::builtin(DefaultFunction::FstPair, t, vec![arg]),
            Builtin::SndPair(t) => AirTree::builtin(DefaultFunction::SndPair, t, vec![arg]),
        }
    }

    pub fn tipo(&self) -> Rc<Type> {
        match self {
            Builtin::HeadList(t) => t.clone(),
            Builtin::ExtractField(t) => t.clone(),
            Builtin::TailList => Type::list(Type::data()),
            Builtin::UnConstrFields => Type::list(Type::data()),
            Builtin::FstPair(t) => t.clone(),
            Builtin::SndPair(t) => t.clone(),
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Builtin::HeadList(_) => write!(f, "head"),
            Builtin::ExtractField(_) => write!(f, "extractfield"),
            Builtin::TailList => write!(f, "tail"),
            Builtin::UnConstrFields => write!(f, "unconstrfields"),
            Builtin::FstPair(_) => write!(f, "fst"),
            Builtin::SndPair(_) => write!(f, "snd"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Builtins {
    pub vec: Vec<Builtin>,
}

impl Default for Builtins {
    fn default() -> Self {
        Self::new()
    }
}

impl Builtins {
    pub fn new() -> Self {
        Builtins { vec: vec![] }
    }

    pub fn new_from_list_case(case: CaseTest) -> Self {
        Self {
            vec: match case {
                CaseTest::List(i) | CaseTest::ListWithTail(i) => {
                    (0..i).fold(vec![], |mut acc, _index| {
                        acc.push(Builtin::TailList);
                        acc
                    })
                }
                _ => unreachable!(),
            },
        }
    }

    pub fn new_from_path(subject_tipo: Rc<Type>, path: Vec<Path>) -> Self {
        Self {
            vec: path
                .into_iter()
                .fold((vec![], vec![]), |(mut builtins, mut rebuilt_path), i| {
                    rebuilt_path.push(i.clone());
                    let is_list = matches!(i, Path::List(_));

                    match i {
                        Path::Pair(i) => {
                            if i == 0 {
                                builtins.push(Builtin::FstPair(get_tipo_by_path(
                                    subject_tipo.clone(),
                                    &rebuilt_path,
                                )));
                            } else if i == 1 {
                                builtins.push(Builtin::SndPair(get_tipo_by_path(
                                    subject_tipo.clone(),
                                    &rebuilt_path,
                                )));
                            } else {
                                unreachable!()
                            }

                            (builtins, rebuilt_path)
                        }
                        Path::List(i) | Path::Tuple(i) => {
                            for _ in 0..i {
                                builtins.push(Builtin::TailList);
                            }

                            if is_list {
                                builtins.push(Builtin::HeadList(get_tipo_by_path(
                                    subject_tipo.clone(),
                                    &rebuilt_path,
                                )));
                            } else {
                                builtins.push(Builtin::ExtractField(get_tipo_by_path(
                                    subject_tipo.clone(),
                                    &rebuilt_path,
                                )));
                            }

                            (builtins, rebuilt_path)
                        }
                        Path::Constr(_rc, i) => {
                            builtins.push(Builtin::UnConstrFields);

                            for _ in 0..i {
                                builtins.push(Builtin::TailList);
                            }

                            builtins.push(Builtin::ExtractField(get_tipo_by_path(
                                subject_tipo.clone(),
                                &rebuilt_path,
                            )));

                            (builtins, rebuilt_path)
                        }

                        Path::ListTail(i) => {
                            for _ in 0..i {
                                builtins.push(Builtin::TailList);
                            }

                            (builtins, rebuilt_path)
                        }
                        Path::OpaqueConstr(_) => (builtins, rebuilt_path),
                    }
                })
                .0,
        }
    }

    pub fn pop(&mut self) {
        self.vec.pop();
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    pub fn merge(mut self, other: Self) -> Self {
        self.vec.extend(other.vec);
        self
    }

    pub fn produce_air(self, prev_name: String, subject_tipo: Rc<Type>, then: AirTree) -> AirTree {
        let (_, _, name_builtins) = self.vec.into_iter().fold(
            (prev_name, subject_tipo, vec![]),
            |(prev_name, prev_tipo, mut acc), item| {
                let next_name = format!("{}_{}", prev_name, item);
                let next_tipo = item.tipo();

                acc.push((prev_name, prev_tipo, next_name.clone(), item));

                (next_name, next_tipo, acc)
            },
        );

        name_builtins
            .into_iter()
            .rfold(then, |then, (prev_name, prev_tipo, next_name, builtin)| {
                AirTree::let_assignment(
                    next_name,
                    builtin.produce_air(AirTree::local_var(prev_name, prev_tipo)),
                    then,
                )
            })
    }
}

impl Display for Builtins {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.vec.iter().map(|i| i.to_string()).join("_"))
    }
}

#[derive(Clone)]
pub struct TreeSet {
    children: Vec<TreeNode>,
}

#[derive(Clone)]
pub struct TreeNode {
    node: Builtin,
    children: Vec<TreeNode>,
}

impl TreeNode {
    fn diff_union_builtins(&mut self, builtins: Builtins) -> Builtins {
        if let Some((first, rest)) = builtins.vec.split_first() {
            if let Some(item) = self.children.iter_mut().find(|item| first == &item.node) {
                item.diff_union_builtins(Builtins { vec: rest.to_vec() })
            } else {
                self.children
                    .extend(TreeSet::new_from_builtins(builtins.clone()).children);

                builtins
            }
        } else {
            builtins
        }
    }
}

impl Default for TreeSet {
    fn default() -> Self {
        Self::new()
    }
}

impl TreeSet {
    pub fn new() -> Self {
        TreeSet { children: vec![] }
    }

    pub fn new_from_builtins(builtins: Builtins) -> Self {
        TreeSet {
            children: builtins
                .vec
                .into_iter()
                .map(|item| TreeNode {
                    node: item,
                    children: vec![],
                })
                .rev()
                .reduce(|prev, mut current| {
                    current.children.push(prev);
                    current
                })
                .into_iter()
                .collect_vec(),
        }
    }

    pub fn diff_union_builtins(&mut self, builtins: Builtins) -> Builtins {
        if let Some((first, rest)) = builtins.vec.split_first() {
            if let Some(item) = self.children.iter_mut().find(|item| first == &item.node) {
                item.diff_union_builtins(Builtins { vec: rest.to_vec() })
            } else {
                self.children
                    .extend(TreeSet::new_from_builtins(builtins.clone()).children);

                builtins
            }
        } else {
            builtins
        }
    }
}
