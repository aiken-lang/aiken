use std::rc::Rc;

use itertools::Itertools;
use uplc::builtins::DefaultFunction;

use crate::expr::Type;

use super::{
    decision_tree::{get_tipo_by_path, Path},
    tree::AirTree,
};

#[derive(Clone)]
pub enum Builtin {
    HeadList(Rc<Type>),
    TailList,
    UnConstr,
    FstPair(Rc<Type>),
    SndPair(Rc<Type>),
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Builtin::HeadList(_), Builtin::HeadList(_)) => true,
            (Builtin::TailList, Builtin::TailList) => true,
            (Builtin::UnConstr, Builtin::UnConstr) => true,
            (Builtin::SndPair(_), Builtin::SndPair(_)) => true,
            _ => false,
        }
    }
}

impl Eq for Builtin {}

impl Builtin {
    fn to_air_call(self, arg: AirTree) -> AirTree {
        match self {
            Builtin::HeadList(t) => AirTree::builtin(DefaultFunction::HeadList, t, vec![arg]),
            Builtin::TailList => AirTree::builtin(
                DefaultFunction::TailList,
                Type::list(Type::data()),
                vec![arg],
            ),
            Builtin::UnConstr => AirTree::builtin(
                DefaultFunction::TailList,
                Type::pair(Type::int(), Type::list(Type::data())),
                vec![arg],
            ),
            Builtin::FstPair(t) => AirTree::builtin(DefaultFunction::FstPair, t, vec![arg]),
            Builtin::SndPair(t) => AirTree::builtin(DefaultFunction::SndPair, t, vec![arg]),
        }
    }

    pub fn tipo(&self) -> Rc<Type> {
        match self {
            Builtin::HeadList(t) => t.clone(),
            Builtin::TailList => Type::list(Type::data()),

            Builtin::UnConstr => Type::pair(Type::int(), Type::list(Type::data())),

            Builtin::FstPair(t) => t.clone(),
            Builtin::SndPair(t) => t.clone(),
        }
    }
}

impl ToString for Builtin {
    fn to_string(&self) -> String {
        match self {
            Builtin::HeadList(_) => "head".to_string(),
            Builtin::TailList => "tail".to_string(),
            Builtin::UnConstr => "unconstr".to_string(),
            Builtin::FstPair(_) => "fst".to_string(),
            Builtin::SndPair(_) => "snd".to_string(),
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct Builtins {
    pub vec: Vec<Builtin>,
}

impl Builtins {
    pub fn new() -> Self {
        Builtins { vec: vec![] }
    }

    pub fn new_from_path(subject_tipo: Rc<Type>, path: Vec<Path>) -> Self {
        Self {
            vec: path
                .into_iter()
                .fold((vec![], vec![]), |(mut builtins, mut rebuilt_path), i| {
                    rebuilt_path.push(i.clone());
                    match i {
                        Path::Pair(i) => {
                            if i == 0 {
                                builtins.push(Builtin::HeadList(get_tipo_by_path(
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

                            builtins.push(Builtin::HeadList(get_tipo_by_path(
                                subject_tipo.clone(),
                                &rebuilt_path,
                            )));

                            (builtins, rebuilt_path)
                        }
                        Path::Constr(_rc, i) => {
                            builtins.extend([
                                Builtin::UnConstr,
                                Builtin::SndPair(Type::list(Type::data())),
                            ]);

                            for _ in 0..i {
                                builtins.push(Builtin::TailList);
                            }

                            builtins.push(Builtin::HeadList(get_tipo_by_path(
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
                    }
                })
                .0,
        }
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    pub fn to_air(self, prev_name: String, subject_tipo: Rc<Type>, then: AirTree) -> AirTree {
        let (_, _, name_builtins) = self.vec.into_iter().fold(
            (prev_name, subject_tipo, vec![]),
            |(prev_name, prev_tipo, mut acc), item| {
                let next_name = format!("{}_{}", prev_name, item.to_string());
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
                    builtin.to_air_call(AirTree::local_var(prev_name, prev_tipo)),
                    then,
                )
            })
    }
}

impl ToString for Builtins {
    fn to_string(&self) -> String {
        self.vec.iter().map(|i| i.to_string()).join("_")
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

impl TreeNode {}

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
        if let Some((first, _rest)) = builtins.vec.split_first() {
            if self.children.iter().any(|item| first == &item.node) {
                todo!()
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
