use std::{cmp::Ordering, iter, ops::Neg, rc::Rc, vec};

use indexmap::IndexMap;
use itertools::Itertools;

use pallas_primitives::babbage::{BigInt, PlutusData};

use crate::{
    ast::{Constant, Data, Name, NamedDeBruijn, Program, Term, Type},
    builder::{CONSTR_FIELDS_EXPOSER, CONSTR_INDEX_EXPOSER},
    builtins::DefaultFunction,
};

use super::interner::CodeGenInterner;

#[derive(Eq, Hash, PartialEq, Clone, Debug, PartialOrd)]
pub enum ScopePath {
    FUNC,
    ARG,
}

#[derive(Eq, Hash, PartialEq, Clone, Debug, Default, PartialOrd)]
pub struct Scope {
    scope: Vec<ScopePath>,
}

impl Scope {
    pub fn new() -> Self {
        Self { scope: vec![] }
    }

    pub fn push(&self, path: ScopePath) -> Self {
        let mut new_scope = self.scope.clone();
        new_scope.push(path);
        Self { scope: new_scope }
    }

    pub fn pop(&self) -> Self {
        let mut new_scope = self.scope.clone();
        new_scope.pop();
        Self { scope: new_scope }
    }

    pub fn common_ancestor(&self, other: &Scope) -> Self {
        Self {
            scope: self
                .scope
                .iter()
                .zip(other.scope.iter())
                .map_while(|(a, b)| if a == b { Some(a) } else { None })
                .cloned()
                .collect_vec(),
        }
    }

    pub fn is_common_ancestor(&self, other: &Scope) -> bool {
        self == &self.common_ancestor(other)
    }

    pub fn len(&self) -> usize {
        self.scope.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

pub struct IdGen {
    id: usize,
}

impl IdGen {
    pub fn new() -> Self {
        Self { id: 0 }
    }

    pub fn next_id(&mut self) -> usize {
        self.id += 1;
        self.id
    }
}

impl Default for IdGen {
    fn default() -> Self {
        Self::new()
    }
}

pub const NO_INLINE: &str = "__no_inline__";

#[derive(PartialEq, PartialOrd, Default, Debug, Clone)]
pub struct VarLookup {
    found: bool,
    occurrences: isize,
    delays: isize,
    no_inline: bool,
}

impl VarLookup {
    pub fn new() -> Self {
        Self {
            found: false,
            occurrences: 0,
            delays: 0,
            no_inline: false,
        }
    }

    pub fn new_found() -> Self {
        Self {
            found: true,
            occurrences: 1,
            delays: 0,
            no_inline: false,
        }
    }

    pub fn combine(self, other: Self) -> Self {
        Self {
            found: self.found || other.found,
            occurrences: self.occurrences + other.occurrences,
            delays: self.delays + other.delays,
            no_inline: self.no_inline || other.no_inline,
        }
    }

    pub fn delay_if_found(self, delay_amount: isize) -> Self {
        if self.found {
            Self {
                found: self.found,
                occurrences: self.occurrences,
                delays: self.delays + delay_amount,
                no_inline: self.no_inline,
            }
        } else {
            self
        }
    }

    pub fn no_inline_if_found(self) -> Self {
        if self.found {
            Self {
                found: self.found,
                occurrences: self.occurrences,
                delays: self.delays,
                no_inline: true,
            }
        } else {
            self
        }
    }
}

impl DefaultFunction {
    pub fn is_order_agnostic_builtin(self) -> bool {
        matches!(
            self,
            DefaultFunction::AddInteger
                | DefaultFunction::MultiplyInteger
                | DefaultFunction::EqualsInteger
                | DefaultFunction::EqualsByteString
                | DefaultFunction::EqualsString
                | DefaultFunction::EqualsData
                | DefaultFunction::Bls12_381_G1_Equal
                | DefaultFunction::Bls12_381_G2_Equal
                | DefaultFunction::Bls12_381_G1_Add
                | DefaultFunction::Bls12_381_G2_Add
        )
    }
    /// For now all of the curry builtins are not forceable
    pub fn can_curry_builtin(self) -> bool {
        matches!(
            self,
            DefaultFunction::AddInteger
                | DefaultFunction::SubtractInteger
                | DefaultFunction::MultiplyInteger
                | DefaultFunction::DivideInteger
                | DefaultFunction::ModInteger
                | DefaultFunction::QuotientInteger
                | DefaultFunction::RemainderInteger
                | DefaultFunction::EqualsInteger
                | DefaultFunction::EqualsByteString
                | DefaultFunction::EqualsString
                | DefaultFunction::EqualsData
                | DefaultFunction::Bls12_381_G1_Equal
                | DefaultFunction::Bls12_381_G2_Equal
                | DefaultFunction::LessThanInteger
                | DefaultFunction::LessThanEqualsInteger
                | DefaultFunction::AppendByteString
                | DefaultFunction::ConsByteString
                | DefaultFunction::SliceByteString
                | DefaultFunction::IndexByteString
                | DefaultFunction::LessThanEqualsByteString
                | DefaultFunction::LessThanByteString
                | DefaultFunction::AppendString
                | DefaultFunction::Bls12_381_G1_Add
                | DefaultFunction::Bls12_381_G2_Add
                | DefaultFunction::ConstrData
        )
    }

    pub fn is_error_safe(self, arg_stack: &[&Term<Name>]) -> bool {
        match self {
            DefaultFunction::AddInteger
            | DefaultFunction::SubtractInteger
            | DefaultFunction::MultiplyInteger
            | DefaultFunction::EqualsInteger
            | DefaultFunction::LessThanInteger
            | DefaultFunction::LessThanEqualsInteger => arg_stack.iter().all(|arg| {
                if let Term::Constant(c) = arg {
                    matches!(c.as_ref(), Constant::Integer(_))
                } else {
                    false
                }
            }),
            DefaultFunction::DivideInteger
            | DefaultFunction::ModInteger
            | DefaultFunction::QuotientInteger
            | DefaultFunction::RemainderInteger => arg_stack.iter().all(|arg| {
                if let Term::Constant(c) = arg {
                    if let Constant::Integer(i) = c.as_ref() {
                        *i != 0.into()
                    } else {
                        false
                    }
                } else {
                    false
                }
            }),
            DefaultFunction::EqualsByteString
            | DefaultFunction::AppendByteString
            | DefaultFunction::LessThanEqualsByteString
            | DefaultFunction::LessThanByteString => arg_stack.iter().all(|arg| {
                if let Term::Constant(c) = arg {
                    matches!(c.as_ref(), Constant::ByteString(_))
                } else {
                    false
                }
            }),

            DefaultFunction::ConsByteString => {
                if let (Term::Constant(c), Term::Constant(c2)) = (&arg_stack[0], &arg_stack[1]) {
                    if let (Constant::Integer(i), Constant::ByteString(_)) =
                        (c.as_ref(), c2.as_ref())
                    {
                        i >= &0.into() && i < &256.into()
                    } else {
                        false
                    }
                } else {
                    false
                }
            }

            DefaultFunction::SliceByteString => {
                if let (Term::Constant(c), Term::Constant(c2), Term::Constant(c3)) =
                    (&arg_stack[0], &arg_stack[1], &arg_stack[2])
                {
                    matches!(
                        (c.as_ref(), c2.as_ref(), c3.as_ref()),
                        (
                            Constant::Integer(_),
                            Constant::Integer(_),
                            Constant::ByteString(_)
                        )
                    )
                } else {
                    false
                }
            }

            DefaultFunction::IndexByteString => {
                if let (Term::Constant(c), Term::Constant(c2)) = (&arg_stack[0], &arg_stack[1]) {
                    if let (Constant::ByteString(bs), Constant::Integer(i)) =
                        (c.as_ref(), c2.as_ref())
                    {
                        i >= &0.into() && i < &bs.len().into()
                    } else {
                        false
                    }
                } else {
                    false
                }
            }

            DefaultFunction::EqualsString | DefaultFunction::AppendString => {
                arg_stack.iter().all(|arg| {
                    if let Term::Constant(c) = arg {
                        matches!(c.as_ref(), Constant::String(_))
                    } else {
                        false
                    }
                })
            }

            DefaultFunction::EqualsData => arg_stack.iter().all(|arg| {
                if let Term::Constant(c) = arg {
                    matches!(c.as_ref(), Constant::Data(_))
                } else {
                    false
                }
            }),

            DefaultFunction::Bls12_381_G1_Equal | DefaultFunction::Bls12_381_G1_Add => {
                arg_stack.iter().all(|arg| {
                    if let Term::Constant(c) = arg {
                        matches!(c.as_ref(), Constant::Bls12_381G1Element(_))
                    } else {
                        false
                    }
                })
            }

            DefaultFunction::Bls12_381_G2_Equal | DefaultFunction::Bls12_381_G2_Add => {
                arg_stack.iter().all(|arg| {
                    if let Term::Constant(c) = arg {
                        matches!(c.as_ref(), Constant::Bls12_381G2Element(_))
                    } else {
                        false
                    }
                })
            }

            DefaultFunction::ConstrData => {
                if let (Term::Constant(c), Term::Constant(c2)) = (&arg_stack[0], &arg_stack[1]) {
                    if let (Constant::Integer(i), Constant::ProtoList(Type::Data, _)) =
                        (c.as_ref(), c2.as_ref())
                    {
                        i >= &0.into()
                    } else {
                        false
                    }
                } else {
                    false
                }
            }

            _ => false,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum BuiltinArgs {
    TwoArgs {
        fst: (usize, Term<Name>),
        snd: Option<(usize, Term<Name>)>,
    },
    ThreeArgs {
        fst: (usize, Term<Name>),
        snd: Option<(usize, Term<Name>)>,
        thd: Option<(usize, Term<Name>)>,
    },
    TwoArgsAnyOrder {
        fst: (usize, Term<Name>),
        snd: Option<(usize, Term<Name>)>,
    },
}

impl BuiltinArgs {
    fn args_from_arg_stack(stack: Vec<(usize, Term<Name>)>, func: DefaultFunction) -> Self {
        let error_safe = func.is_error_safe(&stack.iter().map(|(_, term)| term).collect_vec());

        let mut ordered_arg_stack = stack.into_iter().sorted_by(|(_, arg1), (_, arg2)| {
            // sort by constant first if the builtin is order agnostic
            if func.is_order_agnostic_builtin() {
                if matches!(arg1, Term::Constant(_)) == matches!(arg2, Term::Constant(_)) {
                    Ordering::Equal
                } else if matches!(arg1, Term::Constant(_)) {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            } else {
                Ordering::Equal
            }
        });

        if ordered_arg_stack.len() == 2 && func.is_order_agnostic_builtin() {
            // This is the special case where the order of args is irrelevant to the builtin
            // An example is addInteger or multiplyInteger
            BuiltinArgs::TwoArgsAnyOrder {
                fst: ordered_arg_stack.next().unwrap(),
                snd: if error_safe {
                    ordered_arg_stack.next()
                } else {
                    None
                },
            }
        } else if ordered_arg_stack.len() == 2 {
            BuiltinArgs::TwoArgs {
                fst: ordered_arg_stack.next().unwrap(),
                snd: if error_safe {
                    ordered_arg_stack.next()
                } else {
                    None
                },
            }
        } else {
            BuiltinArgs::ThreeArgs {
                fst: ordered_arg_stack.next().unwrap(),
                snd: ordered_arg_stack.next(),
                thd: if error_safe {
                    ordered_arg_stack.next()
                } else {
                    None
                },
            }
        }
    }

    fn args_to_curried_args(self, builtin: DefaultFunction) -> CurriedBuiltin {
        let args = match self {
            BuiltinArgs::TwoArgs { fst, snd } | BuiltinArgs::TwoArgsAnyOrder { fst, snd } => {
                CurriedArgs::TwoArgs {
                    fst_args: vec![CurriedNode {
                        id: fst.0,
                        term: fst.1,
                    }],
                    snd_args: snd
                        .into_iter()
                        .map(|item| CurriedNode {
                            id: item.0,
                            term: item.1,
                        })
                        .collect_vec(),
                }
            }
            BuiltinArgs::ThreeArgs { fst, snd, thd } => CurriedArgs::ThreeArgs {
                fst_args: vec![CurriedNode {
                    id: fst.0,
                    term: fst.1,
                }],
                snd_args: snd
                    .into_iter()
                    .map(|item| CurriedNode {
                        id: item.0,
                        term: item.1,
                    })
                    .collect_vec(),
                thd_args: thd
                    .into_iter()
                    .map(|item| CurriedNode {
                        id: item.0,
                        term: item.1,
                    })
                    .collect_vec(),
            },
        };

        CurriedBuiltin {
            func: builtin,
            args,
        }
    }

    pub fn get_id_args(self) -> Vec<UplcNode> {
        match self {
            BuiltinArgs::TwoArgs { fst, snd } | BuiltinArgs::TwoArgsAnyOrder { fst, snd } => {
                iter::once(fst)
                    .chain(snd)
                    .map(|item| UplcNode {
                        applied_id: item.0,
                        curried_id: item.0,
                        term: item.1,
                    })
                    .collect_vec()
            }
            BuiltinArgs::ThreeArgs { fst, snd, thd } => iter::once(fst)
                .chain(snd)
                .chain(thd)
                .map(|item| UplcNode {
                    applied_id: item.0,
                    curried_id: item.0,
                    term: item.1,
                })
                .collect_vec(),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct CurriedNode {
    id: usize,
    term: Term<Name>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct UplcNode {
    applied_id: usize,
    curried_id: usize,
    term: Term<Name>,
}

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub struct CurriedName {
    func_name: String,
    id_vec: Vec<usize>,
}

impl CurriedName {
    pub fn len(&self) -> usize {
        self.id_vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum CurriedArgs {
    TwoArgs {
        fst_args: Vec<CurriedNode>,
        snd_args: Vec<CurriedNode>,
    },
    ThreeArgs {
        fst_args: Vec<CurriedNode>,
        snd_args: Vec<CurriedNode>,
        thd_args: Vec<CurriedNode>,
    },
}

impl CurriedArgs {
    pub fn merge_node_by_path(self, path: BuiltinArgs) -> Self {
        match (self, path) {
            (
                CurriedArgs::TwoArgs {
                    mut fst_args,
                    mut snd_args,
                },
                BuiltinArgs::TwoArgs { fst, snd },
            ) => {
                let fst_args = match fst_args.iter_mut().find(|item| item.term == fst.1) {
                    None => {
                        fst_args.push(CurriedNode {
                            id: fst.0,
                            term: fst.1,
                        });
                        fst_args
                    }
                    _ => fst_args,
                };

                let snd_args = match snd_args.iter_mut().find(|item| match &snd {
                    Some(snd) => item.term == snd.1,
                    None => false,
                }) {
                    None => snd_args
                        .into_iter()
                        .chain(snd.into_iter().map(|item| CurriedNode {
                            id: item.0,
                            term: item.1,
                        }))
                        .collect_vec(),
                    _ => snd_args,
                };

                CurriedArgs::TwoArgs { fst_args, snd_args }
            }
            (
                CurriedArgs::TwoArgs {
                    mut fst_args,
                    mut snd_args,
                },
                BuiltinArgs::TwoArgsAnyOrder { fst, snd },
            ) => {
                let mut switched = false;
                let fst_args = if fst_args.iter_mut().any(|item| item.term == fst.1) {
                    fst_args
                } else if fst_args.iter_mut().any(|item| match &snd {
                    Some(snd) => item.term == snd.1,
                    None => false,
                }) {
                    switched = true;
                    fst_args
                } else {
                    fst_args.push(CurriedNode {
                        id: fst.0,
                        term: fst.1.clone(),
                    });

                    fst_args
                };

                // If switched then put the first arg in the second arg slot
                let snd_args = if switched {
                    if snd_args.iter_mut().any(|item| item.term == fst.1) {
                        snd_args
                    } else {
                        snd_args.push(CurriedNode {
                            id: fst.0,
                            term: fst.1,
                        });
                        snd_args
                    }
                } else if snd_args.iter_mut().any(|item| match &snd {
                    Some(snd) => item.term == snd.1,
                    None => false,
                }) {
                    snd_args
                } else {
                    snd_args
                        .into_iter()
                        .chain(snd.into_iter().map(|item| CurriedNode {
                            id: item.0,
                            term: item.1,
                        }))
                        .collect_vec()
                };

                CurriedArgs::TwoArgs { fst_args, snd_args }
            }
            (
                CurriedArgs::ThreeArgs {
                    mut fst_args,
                    mut snd_args,
                    mut thd_args,
                },
                BuiltinArgs::ThreeArgs { fst, snd, thd },
            ) => {
                let fst_args = match fst_args.iter_mut().find(|item| item.term == fst.1) {
                    None => {
                        fst_args.push(CurriedNode {
                            id: fst.0,
                            term: fst.1,
                        });
                        fst_args
                    }
                    _ => fst_args,
                };

                let snd_args = match snd_args.iter_mut().find(|item| match &snd {
                    Some(snd) => item.term == snd.1,
                    None => false,
                }) {
                    None => snd_args
                        .into_iter()
                        .chain(snd.into_iter().map(|item| CurriedNode {
                            id: item.0,
                            term: item.1,
                        }))
                        .collect_vec(),

                    _ => snd_args,
                };

                let thd_args = match thd_args.iter_mut().find(|item| match &thd {
                    Some(thd) => item.term == thd.1,
                    None => false,
                }) {
                    None => thd_args
                        .into_iter()
                        .chain(thd.into_iter().map(|item| CurriedNode {
                            id: item.0,
                            term: item.1,
                        }))
                        .collect_vec(),

                    _ => thd_args,
                };

                CurriedArgs::ThreeArgs {
                    fst_args,
                    snd_args,
                    thd_args,
                }
            }
            _ => unreachable!(),
        }
    }

    fn get_id_args(&self, path: &BuiltinArgs) -> Option<Vec<UplcNode>> {
        match (self, path) {
            (CurriedArgs::TwoArgs { fst_args, snd_args }, BuiltinArgs::TwoArgs { fst, snd }) => {
                let arg = fst_args.iter().find(|item| fst.1 == item.term)?;

                let Some(arg2) = snd_args.iter().find(|item| match snd {
                    Some(snd) => item.term == snd.1,
                    None => false,
                }) else {
                    return Some(vec![UplcNode {
                        applied_id: fst.0,
                        curried_id: arg.id,
                        term: arg.term.clone(),
                    }]);
                };

                Some(vec![
                    UplcNode {
                        applied_id: fst.0,
                        curried_id: arg.id,
                        term: arg.term.clone(),
                    },
                    UplcNode {
                        applied_id: snd.as_ref().unwrap().0,
                        curried_id: arg2.id,
                        term: arg2.term.clone(),
                    },
                ])
            }
            (
                CurriedArgs::TwoArgs { fst_args, snd_args },
                BuiltinArgs::TwoArgsAnyOrder { fst, snd },
            ) => {
                let mut id_vec = vec![];

                if let Some(arg) = fst_args.iter().find(|item| item.term == fst.1) {
                    id_vec.push(UplcNode {
                        applied_id: fst.0,
                        curried_id: arg.id,
                        term: arg.term.clone(),
                    });

                    let Some(arg2) = snd_args.iter().find(|item| match snd {
                        Some(snd) => snd.1 == item.term,
                        None => false,
                    }) else {
                        return Some(id_vec);
                    };

                    id_vec.push(UplcNode {
                        applied_id: snd.as_ref().unwrap().0,
                        curried_id: arg2.id,
                        term: arg2.term.clone(),
                    });

                    Some(id_vec)
                } else if let Some(arg) = fst_args.iter().find(|item| match &snd {
                    Some(snd) => item.term == snd.1,
                    None => false,
                }) {
                    id_vec.push(UplcNode {
                        applied_id: snd.as_ref().unwrap().0,
                        curried_id: arg.id,
                        term: arg.term.clone(),
                    });

                    let Some(arg2) = snd_args.iter().find(|item| item.term == fst.1) else {
                        return Some(id_vec);
                    };

                    id_vec.push(UplcNode {
                        applied_id: fst.0,
                        curried_id: arg2.id,
                        term: arg2.term.clone(),
                    });

                    Some(id_vec)
                } else {
                    None
                }
            }

            (
                CurriedArgs::ThreeArgs {
                    fst_args,
                    snd_args,
                    thd_args,
                },
                BuiltinArgs::ThreeArgs { fst, snd, thd },
            ) => {
                let arg = fst_args.iter().find(|item| fst.1 == item.term)?;

                let Some(arg2) = snd_args.iter().find(|item| match snd {
                    Some(snd) => item.term == snd.1,
                    None => false,
                }) else {
                    return Some(vec![UplcNode {
                        applied_id: fst.0,
                        curried_id: arg.id,
                        term: arg.term.clone(),
                    }]);
                };

                let Some(arg3) = thd_args.iter().find(|item| match thd {
                    Some(thd) => item.term == thd.1,
                    None => false,
                }) else {
                    return Some(vec![
                        UplcNode {
                            applied_id: fst.0,
                            curried_id: arg.id,
                            term: arg.term.clone(),
                        },
                        UplcNode {
                            applied_id: snd.as_ref().unwrap().0,
                            curried_id: arg2.id,
                            term: arg2.term.clone(),
                        },
                    ]);
                };

                Some(vec![
                    UplcNode {
                        applied_id: fst.0,
                        curried_id: arg.id,
                        term: arg.term.clone(),
                    },
                    UplcNode {
                        applied_id: snd.as_ref().unwrap().0,
                        curried_id: arg2.id,
                        term: arg2.term.clone(),
                    },
                    UplcNode {
                        applied_id: thd.as_ref().unwrap().0,
                        curried_id: arg3.id,
                        term: arg3.term.clone(),
                    },
                ])
            }
            _ => unreachable!(),
        }
    }

    fn is_flipped(&self, path: &BuiltinArgs) -> bool {
        match (self, path) {
            (CurriedArgs::TwoArgs { fst_args, .. }, BuiltinArgs::TwoArgsAnyOrder { fst, snd }) => {
                if fst_args.iter().any(|item| item.term == fst.1) {
                    false
                } else {
                    fst_args.iter().any(|item| match &snd {
                        Some(snd) => item.term == snd.1,
                        None => false,
                    })
                }
            }
            _ => false,
        }
    }
}
#[derive(PartialEq, Clone, Debug)]
pub struct CurriedBuiltin {
    pub func: DefaultFunction,
    /// For use with subtract integer where we can flip the order of the arguments
    /// if the second argument is a constant
    pub args: CurriedArgs,
}

impl CurriedBuiltin {
    pub fn merge_node_by_path(self, path: BuiltinArgs) -> Self {
        Self {
            func: self.func,
            args: self.args.merge_node_by_path(path),
        }
    }

    pub fn get_id_args(&self, path: &BuiltinArgs) -> Option<Vec<UplcNode>> {
        self.args.get_id_args(path)
    }

    pub fn is_flipped(&self, path: &BuiltinArgs) -> bool {
        self.args.is_flipped(path)
    }
}

impl Term<Name> {
    fn traverse_uplc_with_helper(
        &mut self,
        scope: &Scope,
        mut arg_stack: Vec<(usize, Term<Name>)>,
        id_gen: &mut IdGen,
        with: &mut impl FnMut(Option<usize>, &mut Term<Name>, Vec<(usize, Term<Name>)>, &Scope),
        inline_lambda: bool,
    ) {
        match self {
            Term::Apply { function, argument } => {
                let arg = Rc::make_mut(argument);

                Self::traverse_uplc_with_helper(
                    arg,
                    &scope.push(ScopePath::ARG),
                    vec![],
                    id_gen,
                    with,
                    inline_lambda,
                );
                let apply_id = id_gen.next_id();

                arg_stack.push((apply_id, arg.clone()));

                let func = Rc::make_mut(function);

                Self::traverse_uplc_with_helper(
                    func,
                    &scope.push(ScopePath::FUNC),
                    arg_stack,
                    id_gen,
                    with,
                    inline_lambda,
                );

                scope.pop();

                with(Some(apply_id), self, vec![], scope);
            }
            Term::Delay(d) => {
                let d = Rc::make_mut(d);
                // First we recurse further to reduce the inner terms before coming back up to the Delay
                Self::traverse_uplc_with_helper(d, scope, arg_stack, id_gen, with, inline_lambda);
                with(None, self, vec![], scope);
            }
            Term::Lambda {
                parameter_name: p,
                body,
            } => {
                let p = p.as_ref().clone();
                // Lambda pops one item off the arg stack. If there is no item then it is a unsaturated lambda
                // We also skip NO_INLINE lambdas since those are placeholder lambdas created by codegen
                let args = if p.text == NO_INLINE {
                    vec![]
                } else {
                    arg_stack.pop().map(|arg| vec![arg]).unwrap_or_default()
                };

                if inline_lambda {
                    // Pass in either one or zero args.
                    // For lambda we run the function with first then recurse on the body or replaced term
                    with(None, self, args, scope);

                    match self {
                        Term::Lambda {
                            parameter_name,
                            body,
                        } if parameter_name.as_ref() == &p => {
                            let body = Rc::make_mut(body);
                            Self::traverse_uplc_with_helper(
                                body,
                                scope,
                                arg_stack,
                                id_gen,
                                with,
                                inline_lambda,
                            );
                        }

                        Term::Constr { .. } => todo!(),
                        Term::Case { .. } => todo!(),
                        other => Self::traverse_uplc_with_helper(
                            other,
                            scope,
                            arg_stack,
                            id_gen,
                            with,
                            inline_lambda,
                        ),
                    }
                } else {
                    let body = Rc::make_mut(body);

                    Self::traverse_uplc_with_helper(
                        body,
                        scope,
                        arg_stack,
                        id_gen,
                        with,
                        inline_lambda,
                    );

                    with(None, self, args, scope);
                }
            }

            Term::Force(f) => {
                let f = Rc::make_mut(f);
                Self::traverse_uplc_with_helper(f, scope, arg_stack, id_gen, with, inline_lambda);
                with(None, self, vec![], scope);
            }
            Term::Case { .. } => todo!(),
            Term::Constr { .. } => todo!(),

            Term::Builtin(func) => {
                let mut args = vec![];

                for _ in 0..func.arity() {
                    if let Some(arg) = arg_stack.pop() {
                        args.push(arg);
                    }
                }
                // Pass in args up to function arity.
                with(None, self, args, scope);
            }
            term => {
                with(None, term, vec![], scope);
            }
        }
    }
}

impl Program<Name> {
    fn traverse_uplc_with(
        self,
        inline_lambda: bool,
        with: &mut impl FnMut(Option<usize>, &mut Term<Name>, Vec<(usize, Term<Name>)>, &Scope),
    ) -> Self {
        let mut term = self.term;
        let scope = Scope { scope: vec![] };
        let arg_stack = vec![];
        let mut id_gen = IdGen::new();

        term.traverse_uplc_with_helper(&scope, arg_stack, &mut id_gen, with, inline_lambda);
        Program {
            version: self.version,
            term,
        }
    }

    pub fn lambda_reducer(self) -> Self {
        let mut lambda_applied_ids = vec![];

        self.traverse_uplc_with(true, &mut |id, term, mut arg_stack, _scope| {
            match term {
                Term::Apply { function, .. } => {
                    // We are applying some arg so now we unwrap the id of the applied arg
                    let id = id.unwrap();

                    if lambda_applied_ids.contains(&id) {
                        let func = Rc::make_mut(function);
                        // we inlined the arg so now remove the apply and arg from the program
                        *term = func.clone();
                    }
                }
                Term::Lambda {
                    parameter_name,
                    body,
                } => {
                    // pops stack here no matter what
                    if let Some((arg_id, arg_term)) = arg_stack.pop() {
                        match &arg_term {
                            Term::Constant(c) if matches!(c.as_ref(), Constant::String(_)) => {}
                            Term::Delay(e) if matches!(e.as_ref(), Term::Error) => {
                                let body = Rc::make_mut(body);
                                lambda_applied_ids.push(arg_id);
                                // creates new body that replaces all var occurrences with the arg
                                *term = substitute_var(body, parameter_name.clone(), &arg_term);
                            }
                            Term::Constant(_) | Term::Var(_) | Term::Builtin(_) => {
                                let body = Rc::make_mut(body);
                                lambda_applied_ids.push(arg_id);
                                // creates new body that replaces all var occurrences with the arg
                                *term = substitute_var(body, parameter_name.clone(), &arg_term);
                            }
                            l @ Term::Lambda { .. } => {
                                if is_a_builtin_wrapper(l) {
                                    let body = Rc::make_mut(body);
                                    lambda_applied_ids.push(arg_id);
                                    // creates new body that replaces all var occurrences with the arg
                                    *term = substitute_var(body, parameter_name.clone(), &arg_term);
                                }
                            }

                            _ => {}
                        }
                    }
                }

                Term::Case { .. } => todo!(),
                Term::Constr { .. } => todo!(),
                _ => {}
            }
        })
    }

    pub fn builtin_force_reducer(self) -> Self {
        let mut builtin_map = IndexMap::new();

        let program = self.traverse_uplc_with(true, &mut |_id, term, _arg_stack, _scope| {
            if let Term::Force(f) = term {
                let f = Rc::make_mut(f);
                match f {
                    Term::Force(inner_f) => {
                        if let Term::Builtin(func) = inner_f.as_ref() {
                            builtin_map.insert(*func as u8, ());
                            *term = Term::Var(
                                Name {
                                    text: format!("__{}_wrapped", func.aiken_name()),
                                    unique: 0.into(),
                                }
                                .into(),
                            );
                        }
                    }
                    Term::Builtin(func) if func.force_count() == 1 => {
                        builtin_map.insert(*func as u8, ());
                        *term = Term::Var(
                            Name {
                                text: format!("__{}_wrapped", func.aiken_name()),
                                unique: 0.into(),
                            }
                            .into(),
                        );
                    }
                    _ => {}
                }
            }
        });
        let mut term = program.term;

        for default_func_index in builtin_map.keys().sorted().cloned() {
            let default_func: DefaultFunction = default_func_index.try_into().unwrap();

            term = term
                .lambda(format!("__{}_wrapped", default_func.aiken_name()))
                .apply(if default_func.force_count() == 1 {
                    Term::Builtin(default_func).force()
                } else {
                    Term::Builtin(default_func).force().force()
                });
        }

        let mut program = Program {
            version: program.version,
            term,
        };

        let mut interner = CodeGenInterner::new();

        interner.program(&mut program);

        let program = Program::<NamedDeBruijn>::try_from(program).unwrap();

        Program::<Name>::try_from(program).unwrap()
    }

    pub fn identity_reducer(self) -> Self {
        let mut identity_applied_ids = vec![];
        self.traverse_uplc_with(true, &mut |id, term, mut arg_stack, _scope| {
            match term {
                Term::Apply { function, .. } => {
                    // We are applying some arg so now we unwrap the id of the applied arg
                    let id = id.unwrap();

                    if identity_applied_ids.contains(&id) {
                        let func = Rc::make_mut(function);
                        // we inlined the arg so now remove the apply and arg from the program
                        *term = func.clone();
                    }
                }
                Term::Lambda {
                    parameter_name,
                    body,
                } => {
                    // pops stack here no matter what

                    match arg_stack.pop() {
                        Some((
                            arg_id,
                            Term::Lambda {
                                parameter_name: inline_name,
                                body: identity_body,
                            },
                        )) if inline_name.text == NO_INLINE => {
                            if let Term::Lambda {
                                parameter_name: identity_name,
                                body: identity_body,
                            } = identity_body.as_ref()
                            {
                                if let Term::Var(identity_var) = identity_body.as_ref() {
                                    if identity_var.text == identity_name.text
                                        && identity_var.unique == identity_name.unique
                                    {
                                        // Replace all applied usages of identity with the arg
                                        let temp_term = replace_identity_usage(
                                            body.as_ref(),
                                            parameter_name.clone(),
                                        );
                                        // Have to check if the body still has any occurrences of the parameter
                                        // After attempting replacement
                                        if var_occurrences(
                                            &temp_term,
                                            parameter_name.clone(),
                                            vec![],
                                            vec![],
                                        )
                                        .found
                                        {
                                            let body = Rc::make_mut(body);
                                            *body = temp_term;
                                        } else {
                                            identity_applied_ids.push(arg_id);
                                            *term = temp_term;
                                        }
                                    }
                                }
                            }
                        }

                        Some((
                            arg_id,
                            Term::Lambda {
                                parameter_name: identity_name,
                                body: identity_body,
                            },
                        )) => {
                            if let Term::Var(identity_var) = identity_body.as_ref() {
                                if identity_var.text == identity_name.text
                                    && identity_var.unique == identity_name.unique
                                {
                                    // Replace all applied usages of identity with the arg
                                    let temp_term = replace_identity_usage(
                                        body.as_ref(),
                                        parameter_name.clone(),
                                    );
                                    // Have to check if the body still has any occurrences of the parameter
                                    // After attempting replacement
                                    if var_occurrences(
                                        &temp_term,
                                        parameter_name.clone(),
                                        vec![],
                                        vec![],
                                    )
                                    .found
                                    {
                                        let body = Rc::make_mut(body);
                                        *body = temp_term;
                                    } else {
                                        identity_applied_ids.push(arg_id);
                                        *term = temp_term;
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                Term::Constr { .. } => todo!(),
                Term::Case { .. } => todo!(),
                _ => {}
            }
        })
    }

    pub fn inline_reducer(self) -> Self {
        let mut lambda_applied_ids = vec![];

        self.traverse_uplc_with(true, &mut |id, term, mut arg_stack, _scope| match term {
            Term::Apply { function, .. } => {
                // We are applying some arg so now we unwrap the id of the applied arg
                let id = id.unwrap();

                if lambda_applied_ids.contains(&id) {
                    // we inlined the arg so now remove the apply and arg from the program
                    *term = function.as_ref().clone();
                }
            }
            Term::Lambda {
                parameter_name,
                body,
            } => {
                // pops stack here no matter what
                if let Some((arg_id, arg_term)) = arg_stack.pop() {
                    let arg_term = match &arg_term {
                        Term::Lambda {
                            parameter_name,
                            body,
                        } if parameter_name.text == NO_INLINE => body.as_ref().clone(),
                        _ => arg_term,
                    };

                    let body = Rc::make_mut(body);
                    let var_lookup = var_occurrences(body, parameter_name.clone(), vec![], vec![]);

                    let substitute_condition = (var_lookup.delays == 0 && !var_lookup.no_inline)
                        || matches!(
                            &arg_term,
                            Term::Var(_)
                                | Term::Constant(_)
                                | Term::Delay(_)
                                | Term::Lambda { .. }
                                | Term::Builtin(_),
                        );

                    if var_lookup.occurrences == 1 && substitute_condition {
                        *body = substitute_var(body, parameter_name.clone(), &arg_term);

                        lambda_applied_ids.push(arg_id);
                        *term = body.clone();

                    // This will strip out unused terms that can't throw an error by themselves
                    } else if !var_lookup.found
                        && matches!(
                            arg_term,
                            Term::Var(_)
                                | Term::Constant(_)
                                | Term::Delay(_)
                                | Term::Lambda { .. }
                                | Term::Builtin(_)
                        )
                    {
                        lambda_applied_ids.push(arg_id);
                        *term = body.clone();
                    }
                }
            }
            Term::Constr { .. } => todo!(),
            Term::Case { .. } => todo!(),
            _ => {}
        })
    }

    pub fn force_delay_reducer(self) -> Self {
        self.traverse_uplc_with(true, &mut |_id, term, _arg_stack, _scope| {
            if let Term::Force(f) = term {
                let f = f.as_ref();

                if let Term::Delay(body) = f {
                    *term = body.as_ref().clone();
                }
            }
        })
    }

    pub fn remove_no_inlines(self) -> Self {
        self.traverse_uplc_with(true, &mut |_, term, _, _| match term {
            Term::Lambda {
                parameter_name,
                body,
            } if parameter_name.text == NO_INLINE => *term = body.as_ref().clone(),
            _ => {}
        })
    }

    pub fn inline_constr_ops(self) -> Self {
        self.traverse_uplc_with(true, &mut |_, term, _, _| {
            if let Term::Apply { function, argument } = term {
                if let Term::Var(name) = function.as_ref() {
                    if name.text == CONSTR_FIELDS_EXPOSER {
                        *term = Term::snd_pair().apply(Term::Apply {
                            function: Term::unconstr_data().into(),
                            argument: argument.clone(),
                        })
                    } else if name.text == CONSTR_INDEX_EXPOSER {
                        *term = Term::fst_pair().apply(Term::Apply {
                            function: Term::unconstr_data().into(),
                            argument: argument.clone(),
                        })
                    }
                }
            }
        })
    }

    pub fn cast_data_reducer(self) -> Self {
        let mut applied_ids = vec![];

        self.traverse_uplc_with(true, &mut |id, term, mut arg_stack, _scope| {
            match term {
                Term::Apply { function, .. } => {
                    // We are apply some arg so now we unwrap the id of the applied arg
                    let id = id.unwrap();

                    if applied_ids.contains(&id) {
                        let func = Rc::make_mut(function);
                        // we inlined the arg so now remove the apply and arg from the program
                        *term = func.clone();
                    }
                }

                Term::Builtin(first_function) => {
                    let Some((arg_id, arg_term)) = arg_stack.pop() else {
                        return;
                    };

                    match arg_term {
                        Term::Apply { function, argument } => {
                            if let Term::Builtin(second_function) = function.as_ref() {
                                match (first_function, second_function) {
                                    (DefaultFunction::UnIData, DefaultFunction::IData)
                                    | (DefaultFunction::IData, DefaultFunction::UnIData)
                                    | (DefaultFunction::BData, DefaultFunction::UnBData)
                                    | (DefaultFunction::UnBData, DefaultFunction::BData)
                                    | (DefaultFunction::ListData, DefaultFunction::UnListData)
                                    | (DefaultFunction::UnListData, DefaultFunction::ListData)
                                    | (DefaultFunction::MapData, DefaultFunction::UnMapData)
                                    | (DefaultFunction::UnMapData, DefaultFunction::MapData) => {
                                        applied_ids.push(arg_id);
                                        *term = argument.as_ref().clone();
                                    }
                                    _ => {}
                                }
                            }
                        }
                        Term::Constant(c) => match (first_function, c.as_ref()) {
                            (
                                DefaultFunction::UnIData,
                                Constant::Data(PlutusData::BigInt(BigInt::Int(i))),
                            ) => {
                                applied_ids.push(arg_id);
                                *term = Term::integer(i128::from(*i).into());
                            }
                            (DefaultFunction::IData, Constant::Integer(i)) => {
                                applied_ids.push(arg_id);
                                *term = Term::data(Data::integer(i.clone()));
                            }
                            (
                                DefaultFunction::UnBData,
                                Constant::Data(PlutusData::BoundedBytes(b)),
                            ) => {
                                applied_ids.push(arg_id);
                                *term = Term::byte_string(b.clone().into());
                            }
                            (DefaultFunction::BData, Constant::ByteString(b)) => {
                                applied_ids.push(arg_id);
                                *term = Term::data(Data::bytestring(b.clone()));
                            }
                            (DefaultFunction::UnListData, Constant::Data(PlutusData::Array(l))) => {
                                applied_ids.push(arg_id);
                                *term = Term::list_values(
                                    l.iter()
                                        .map(|item| Constant::Data(item.clone()))
                                        .collect_vec(),
                                );
                            }
                            (DefaultFunction::ListData, Constant::ProtoList(_, l)) => {
                                applied_ids.push(arg_id);
                                *term = Term::data(Data::list(
                                    l.iter()
                                        .map(|item| match item {
                                            Constant::Data(d) => d.clone(),
                                            _ => unreachable!(),
                                        })
                                        .collect_vec(),
                                ));
                            }
                            (DefaultFunction::MapData, Constant::ProtoList(_, m)) => {
                                applied_ids.push(arg_id);
                                *term = Term::data(Data::map(
                                    m.iter()
                                        .map(|m| match m {
                                            Constant::ProtoPair(_, _, f, s) => {
                                                match (f.as_ref(), s.as_ref()) {
                                                    (Constant::Data(d), Constant::Data(d2)) => {
                                                        (d.clone(), d2.clone())
                                                    }
                                                    _ => unreachable!(),
                                                }
                                            }
                                            _ => unreachable!(),
                                        })
                                        .collect_vec(),
                                ));
                            }
                            (DefaultFunction::UnMapData, Constant::Data(PlutusData::Map(m))) => {
                                applied_ids.push(arg_id);
                                *term = Term::map_values(
                                    m.iter()
                                        .map(|item| {
                                            Constant::ProtoPair(
                                                Type::Data,
                                                Type::Data,
                                                Constant::Data(item.0.clone()).into(),
                                                Constant::Data(item.1.clone()).into(),
                                            )
                                        })
                                        .collect_vec(),
                                );
                            }
                            _ => {}
                        },
                        _ => {}
                    }
                }
                Term::Constr { .. } => todo!(),
                Term::Case { .. } => todo!(),
                _ => {}
            }
        })
    }

    // Converts subtract integer with a constant to add integer with a negative constant
    pub fn convert_arithmetic_ops(self) -> Self {
        let mut constants_to_flip = vec![];

        self.traverse_uplc_with(true, &mut |id, term, arg_stack, _scope| match term {
            Term::Apply { argument, .. } => {
                let id = id.unwrap();

                if constants_to_flip.contains(&id) {
                    let Term::Constant(c) = Rc::make_mut(argument) else {
                        unreachable!();
                    };

                    let Constant::Integer(i) = c.as_ref() else {
                        unreachable!();
                    };

                    *c = Constant::Integer(i.neg()).into();
                }
            }
            Term::Builtin(d @ DefaultFunction::SubtractInteger) => {
                if arg_stack.len() == d.arity() {
                    let Some((apply_id, Term::Constant(_))) = arg_stack.last() else {
                        return;
                    };
                    constants_to_flip.push(*apply_id);

                    *term = Term::Builtin(DefaultFunction::AddInteger);
                }
            }
            Term::Constr { .. } => todo!(),
            Term::Case { .. } => todo!(),
            _ => {}
        })
    }

    pub fn builtin_curry_reducer(self) -> Self {
        let mut curried_terms = vec![];
        let mut id_mapped_curry_terms: IndexMap<CurriedName, (Scope, Term<Name>, usize)> =
            IndexMap::new();
        let mut curry_applied_ids = vec![];
        let mut scope_mapped_to_term: IndexMap<Scope, Vec<(CurriedName, Term<Name>)>> =
            IndexMap::new();

        let mut flipped_terms: IndexMap<Scope, bool> = IndexMap::new();

        let mut final_ids: IndexMap<Vec<usize>, ()> = IndexMap::new();

        let step_a =
            self.traverse_uplc_with(false, &mut |_id, term, arg_stack, scope| match term {
                Term::Builtin(func) => {
                    if func.can_curry_builtin() && arg_stack.len() == func.arity() {
                        // In the case of order agnostic builtins we want to sort the args by constant first
                        // This gives us the opportunity to curry constants that often pop up in the code

                        let builtin_args = BuiltinArgs::args_from_arg_stack(arg_stack, *func);

                        // First we see if we have already curried this builtin before
                        let mut id_vec = if let Some((index, _)) =
                            curried_terms.iter_mut().find_position(
                                |curried_term: &&mut CurriedBuiltin| curried_term.func == *func,
                            ) {
                            // We found it the builtin was curried before
                            // So now we merge the new args into the existing curried builtin

                            let curried_builtin = curried_terms.swap_remove(index);

                            let curried_builtin =
                                curried_builtin.merge_node_by_path(builtin_args.clone());

                            let Some(id_vec) = curried_builtin.get_id_args(&builtin_args) else {
                                unreachable!();
                            };

                            flipped_terms
                                .insert(scope.clone(), curried_builtin.is_flipped(&builtin_args));

                            curried_terms.push(curried_builtin);

                            id_vec
                        } else {
                            // Brand new buitlin so we add it to the list
                            let curried_builtin = builtin_args.clone().args_to_curried_args(*func);

                            let Some(id_vec) = curried_builtin.get_id_args(&builtin_args) else {
                                unreachable!();
                            };

                            curried_terms.push(curried_builtin);

                            id_vec
                        };

                        while let Some(node) = id_vec.pop() {
                            let mut id_only_vec =
                                id_vec.iter().map(|item| item.curried_id).collect_vec();

                            id_only_vec.push(node.curried_id);

                            let curry_name = CurriedName {
                                func_name: func.aiken_name(),
                                id_vec: id_only_vec,
                            };

                            if let Some((map_scope, _, occurrences)) =
                                id_mapped_curry_terms.get_mut(&curry_name)
                            {
                                *map_scope = map_scope.common_ancestor(scope);
                                *occurrences += 1;
                            } else if id_vec.is_empty() {
                                id_mapped_curry_terms.insert(
                                    curry_name,
                                    (scope.clone(), Term::Builtin(*func).apply(node.term), 1),
                                );
                            } else {
                                let var_name = id_vec_function_to_var(
                                    &func.aiken_name(),
                                    &id_vec.iter().map(|item| item.curried_id).collect_vec(),
                                );

                                id_mapped_curry_terms.insert(
                                    curry_name,
                                    (scope.clone(), Term::var(var_name).apply(node.term), 1),
                                );
                            }
                        }
                    }
                }
                Term::Constr { .. } => todo!(),
                Term::Case { .. } => todo!(),
                _ => {}
            });

        id_mapped_curry_terms
            .into_iter()
            // Only hoist for occurrences greater than 2
            .filter(|(_, (_, _, occurrences))| *occurrences > 2)
            .for_each(|(key, val)| {
                final_ids.insert(key.id_vec.clone(), ());

                match scope_mapped_to_term.get_mut(&val.0) {
                    Some(list) => {
                        let insert_position = list
                            .iter()
                            .position(|(list_key, _)| key.len() <= list_key.len())
                            .unwrap_or(list.len());

                        list.insert(insert_position, (key, val.1));
                    }
                    None => {
                        scope_mapped_to_term.insert(val.0, vec![(key, val.1)]);
                    }
                }
            });

        let mut step_b =
            step_a.traverse_uplc_with(false, &mut |id, term, mut arg_stack, scope| match term {
                Term::Builtin(func) => {
                    if func.can_curry_builtin() && arg_stack.len() == func.arity() {
                        let Some(curried_builtin) =
                            curried_terms.iter().find(|curry| curry.func == *func)
                        else {
                            return;
                        };

                        if let Some(true) = flipped_terms.get(scope) {
                            arg_stack.reverse();
                        }

                        let builtin_args = BuiltinArgs::args_from_arg_stack(arg_stack, *func);

                        let Some(mut id_vec) = curried_builtin.get_id_args(&builtin_args) else {
                            return;
                        };

                        while !id_vec.is_empty() {
                            let id_lookup = id_vec.iter().map(|item| item.curried_id).collect_vec();

                            if final_ids.contains_key(&id_lookup) {
                                break;
                            }
                            id_vec.pop();
                        }

                        if id_vec.is_empty() {
                            return;
                        }

                        let name = id_vec_function_to_var(
                            &func.aiken_name(),
                            &id_vec.iter().map(|item| item.curried_id).collect_vec(),
                        );

                        id_vec.iter().for_each(|item| {
                            curry_applied_ids.push(item.applied_id);
                        });

                        *term = Term::var(name);
                    }
                }
                Term::Apply { function, .. } => {
                    let id = id.unwrap();

                    if curry_applied_ids.contains(&id) {
                        *term = function.as_ref().clone();
                    }

                    if let Some(insert_list) = scope_mapped_to_term.remove(scope) {
                        for (key, val) in insert_list.into_iter().rev() {
                            let name = id_vec_function_to_var(&key.func_name, &key.id_vec);

                            if var_occurrences(term, Name::text(&name).into(), vec![], vec![]).found
                            {
                                *term = term.clone().lambda(name).apply(val);
                            }
                        }
                    }
                }
                Term::Constr { .. } => todo!(),
                Term::Case { .. } => todo!(),
                _ => {
                    if let Some(insert_list) = scope_mapped_to_term.remove(scope) {
                        for (key, val) in insert_list.into_iter().rev() {
                            let name = id_vec_function_to_var(&key.func_name, &key.id_vec);

                            if var_occurrences(term, Name::text(&name).into(), vec![], vec![]).found
                            {
                                *term = term.clone().lambda(name).apply(val);
                            }
                        }
                    }
                }
            });

        let mut interner = CodeGenInterner::new();

        interner.program(&mut step_b);

        step_b
    }
}

fn id_vec_function_to_var(func_name: &str, id_vec: &[usize]) -> String {
    format!(
        "__{}_{}_curried",
        func_name,
        id_vec
            .iter()
            .map(|item| item.to_string())
            .collect::<Vec<String>>()
            .join("_")
    )
}

fn var_occurrences(
    term: &Term<Name>,
    search_for: Rc<Name>,
    mut arg_stack: Vec<()>,
    mut force_stack: Vec<()>,
) -> VarLookup {
    match term {
        Term::Var(name) => {
            if name.text == search_for.text && name.unique == search_for.unique {
                VarLookup::new_found()
            } else {
                VarLookup::new()
            }
        }
        Term::Delay(body) => {
            let not_forced: isize = isize::from(force_stack.pop().is_none());

            var_occurrences(body, search_for, arg_stack, force_stack).delay_if_found(not_forced)
        }
        Term::Lambda {
            parameter_name,
            body,
        } => {
            if parameter_name.text == NO_INLINE {
                var_occurrences(body.as_ref(), search_for, arg_stack, force_stack)
                    .no_inline_if_found()
            } else if parameter_name.text == search_for.text
                && parameter_name.unique == search_for.unique
            {
                VarLookup::new()
            } else {
                let not_applied: isize = isize::from(arg_stack.pop().is_none());
                var_occurrences(body.as_ref(), search_for, arg_stack, force_stack)
                    .delay_if_found(not_applied)
            }
        }
        Term::Apply { function, argument } => {
            arg_stack.push(());

            var_occurrences(
                function.as_ref(),
                search_for.clone(),
                arg_stack,
                force_stack,
            )
            .combine(var_occurrences(
                argument.as_ref(),
                search_for,
                vec![],
                vec![],
            ))
        }
        Term::Force(x) => {
            force_stack.push(());
            var_occurrences(x.as_ref(), search_for, arg_stack, force_stack)
        }
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        _ => VarLookup::new(),
    }
}

fn substitute_var(term: &Term<Name>, original: Rc<Name>, replace_with: &Term<Name>) -> Term<Name> {
    match term {
        Term::Var(name) => {
            if name.text == original.text && name.unique == original.unique {
                replace_with.clone()
            } else {
                Term::Var(name.clone())
            }
        }
        Term::Delay(body) => {
            Term::Delay(substitute_var(body.as_ref(), original, replace_with).into())
        }
        Term::Lambda {
            parameter_name,
            body,
        } => {
            if parameter_name.text == original.text && parameter_name.unique == original.unique {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: body.clone(),
                }
            } else {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: substitute_var(body.as_ref(), original, replace_with).into(),
                }
            }
        }
        Term::Apply { function, argument } => Term::Apply {
            function: substitute_var(function.as_ref(), original.clone(), replace_with).into(),
            argument: substitute_var(argument.as_ref(), original, replace_with).into(),
        },
        Term::Force(f) => Term::Force(substitute_var(f.as_ref(), original, replace_with).into()),
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        x => x.clone(),
    }
}

fn replace_identity_usage(term: &Term<Name>, original: Rc<Name>) -> Term<Name> {
    match term {
        Term::Delay(body) => Term::Delay(replace_identity_usage(body.as_ref(), original).into()),
        Term::Lambda {
            parameter_name,
            body,
        } => {
            if parameter_name.text == original.text && parameter_name.unique == original.unique {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: body.clone(),
                }
            } else {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: Rc::new(replace_identity_usage(body.as_ref(), original)),
                }
            }
        }
        Term::Apply { function, argument } => {
            let func = replace_identity_usage(function.as_ref(), original.clone());
            let arg = replace_identity_usage(argument.as_ref(), original.clone());

            let Term::Var(name) = &func else {
                return Term::Apply {
                    function: func.into(),
                    argument: arg.into(),
                };
            };

            if name.text == original.text && name.unique == original.unique {
                arg
            } else {
                Term::Apply {
                    function: func.into(),
                    argument: arg.into(),
                }
            }
        }
        Term::Force(f) => Term::Force(Rc::new(replace_identity_usage(f.as_ref(), original))),
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        x => x.clone(),
    }
}

fn is_a_builtin_wrapper(term: &Term<Name>) -> bool {
    let (names, term) = pop_lambdas_and_get_names(term);

    let mut arg_names = vec![];

    let mut term = term;

    while let Term::Apply { function, argument } = term {
        match argument.as_ref() {
            Term::Var(name) => arg_names.push(name),

            Term::Constant(_) => {}
            _ => {
                return false;
            }
        }
        term = function.as_ref();
    }

    arg_names.iter().all(|item| names.contains(item)) && matches!(term, Term::Builtin(_))
}

fn pop_lambdas_and_get_names(term: &Term<Name>) -> (Vec<Rc<Name>>, &Term<Name>) {
    let mut names = vec![];

    let mut term = term;

    while let Term::Lambda {
        parameter_name,
        body,
    } = term
    {
        if parameter_name.text != NO_INLINE {
            names.push(parameter_name.clone());
        }
        term = body.as_ref();
    }

    (names, term)
}

#[cfg(test)]
mod tests {

    use pallas_primitives::babbage::{BigInt, PlutusData};
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{Constant, Data, Name, NamedDeBruijn, Program, Term},
        builder::{CONSTR_FIELDS_EXPOSER, CONSTR_INDEX_EXPOSER},
        builtins::DefaultFunction,
        optimize::interner::CodeGenInterner,
    };

    use super::NO_INLINE;

    fn compare_optimization(
        mut expected: Program<Name>,
        mut program: Program<Name>,
        optimization: fn(Program<Name>) -> Program<Name>,
    ) {
        let mut interner = CodeGenInterner::new();

        interner.program(&mut program);

        let mut interner = CodeGenInterner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = optimization(program);

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn lambda_reduce_var() {
        let program = Program {
            version: (1, 0, 0),
            term: Term::var("bar")
                .lambda("bar")
                .apply(Term::var("foo"))
                .lambda("foo")
                .apply(
                    Term::constr_data()
                        .apply(Term::integer(3.into()))
                        .apply(Term::list_values(vec![])),
                ),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::var("foo").lambda("foo").apply(
                Term::constr_data()
                    .apply(Term::integer(3.into()))
                    .apply(Term::list_values(vec![])),
            ),
        };

        compare_optimization(expected, program, |p| p.lambda_reducer());
    }

    #[test]
    fn lambda_reduce_constant() {
        let program = Program {
            version: (1, 0, 0),
            term: Term::var("foo")
                .lambda("foo")
                .apply(Term::integer(6.into())),
        };

        let expected: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::integer(6.into()),
        };

        compare_optimization(expected, program, |p| p.lambda_reducer());
    }

    #[test]
    fn lambda_reduce_builtin() {
        let program = Program {
            version: (1, 0, 0),
            term: Term::var("foo").lambda("foo").apply(Term::add_integer()),
        };

        let expected: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::add_integer(),
        };

        compare_optimization(expected, program, |p| p.lambda_reducer());
    }

    #[test]
    fn lambda_reduce_force_delay_error_lam() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::var("foo")
                .apply(Term::var("bar"))
                .apply(Term::var("baz"))
                .apply(Term::var("bat"))
                .lambda("foo")
                .apply(Term::snd_pair())
                .lambda("bar")
                .apply(Term::integer(1.into()).delay())
                .lambda("baz")
                .apply(Term::Error)
                .lambda("bat")
                .apply(Term::bool(false).lambda("x")),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::var("foo")
                .apply(Term::var("bar"))
                .apply(Term::var("baz"))
                .apply(Term::var("bat"))
                .lambda("foo")
                .apply(Term::snd_pair())
                .lambda("bar")
                .apply(Term::integer(1.into()).delay())
                .lambda("baz")
                .apply(Term::Error)
                .lambda("bat")
                .apply(Term::bool(false).lambda("x")),
        };

        compare_optimization(expected, program, |p| p.lambda_reducer());
    }

    #[test]
    fn builtin_force_reduce_list_builtins() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::mk_cons()
                .apply(Term::var("x"))
                .apply(Term::tail_list().apply(Term::head_list().apply(Term::var("y"))))
                .lambda("x")
                .lambda("y"),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::var("__cons_list_wrapped")
                .apply(Term::var("x"))
                .apply(
                    Term::var("__tail_list_wrapped")
                        .apply(Term::var("__head_list_wrapped").apply(Term::var("y"))),
                )
                .lambda("x")
                .lambda("y")
                // Forces are automatically applied by builder
                .lambda("__cons_list_wrapped")
                .apply(Term::mk_cons())
                .lambda("__head_list_wrapped")
                .apply(Term::head_list())
                .lambda("__tail_list_wrapped")
                .apply(Term::tail_list()),
        };

        compare_optimization(expected, program, |p| p.builtin_force_reducer());
    }

    #[test]
    fn builtin_force_reduce_if_builtin() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::equals_integer()
                .apply(Term::var("x"))
                .apply(
                    Term::add_integer()
                        .apply(Term::integer(2.into()))
                        .apply(Term::var("y")),
                )
                .delayed_if_then_else(
                    Term::length_of_bytearray().apply(Term::byte_string(vec![])),
                    Term::Error,
                )
                .lambda("x")
                .lambda("y"),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::var("__if_then_else_wrapped")
                .apply(
                    Term::equals_integer().apply(Term::var("x")).apply(
                        Term::add_integer()
                            .apply(Term::integer(2.into()))
                            .apply(Term::var("y")),
                    ),
                )
                .apply(
                    Term::length_of_bytearray()
                        .apply(Term::byte_string(vec![]))
                        .delay(),
                )
                .apply(Term::Error.delay())
                .force()
                .lambda("x")
                .lambda("y")
                .lambda("__if_then_else_wrapped")
                .apply(Term::Builtin(DefaultFunction::IfThenElse).force()),
        };

        compare_optimization(expected, program, |p| p.builtin_force_reducer());
    }

    #[test]
    fn builtin_force_reduce_pair_builtins() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::add_integer()
                .apply(Term::un_i_data().apply(Term::fst_pair().apply(Term::var("__pair"))))
                .apply(Term::un_i_data().apply(Term::snd_pair().apply(Term::var("__pair"))))
                .lambda("__pair")
                .apply(
                    Term::mk_pair_data()
                        .apply(Term::data(Data::integer(1.into())))
                        .apply(Term::data(Data::integer(5.into()))),
                ),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::add_integer()
                .apply(
                    Term::un_i_data()
                        .apply(Term::var("__fst_pair_wrapped").apply(Term::var("__pair"))),
                )
                .apply(
                    Term::un_i_data()
                        .apply(Term::var("__snd_pair_wrapped").apply(Term::var("__pair"))),
                )
                .lambda("__pair")
                .apply(
                    Term::mk_pair_data()
                        .apply(Term::data(Data::integer(1.into())))
                        .apply(Term::data(Data::integer(5.into()))),
                )
                .lambda("__fst_pair_wrapped")
                .apply(Term::fst_pair())
                .lambda("__snd_pair_wrapped")
                .apply(Term::snd_pair()),
        };

        compare_optimization(expected, program, |p| p.builtin_force_reducer());
    }

    #[test]
    fn identity_reduce_usage() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("identity").apply(Term::var("x")))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay())
                .lambda("identity")
                .apply(Term::var("y").lambda("y")),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("x"))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay()),
        };

        compare_optimization(expected, program, |p| p.identity_reducer());
    }

    #[test]
    fn identity_reduce_0_occurrence() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("x"))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay())
                .lambda("identity")
                .apply(Term::var("y").lambda("y")),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("x"))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay()),
        };

        compare_optimization(expected, program, |p| p.identity_reducer());
    }

    #[test]
    fn identity_reduce_param() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(
                    Term::var("f")
                        .apply(Term::var("x"))
                        .apply(Term::var("identity")),
                )
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay())
                .lambda("identity")
                .apply(Term::var("y").lambda("y"))
                .lambda("f")
                .apply(
                    Term::var("with")
                        .apply(Term::var("x"))
                        .lambda("with")
                        .lambda("x"),
                ),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(
                    Term::var("f")
                        .apply(Term::var("x"))
                        .apply(Term::var("identity")),
                )
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay())
                .lambda("identity")
                .apply(Term::var("y").lambda("y"))
                .lambda("f")
                .apply(
                    Term::var("with")
                        .apply(Term::var("x"))
                        .lambda("with")
                        .lambda("x"),
                ),
        };

        compare_optimization(expected, program, |p| p.identity_reducer());
    }

    #[test]
    fn identity_reduce_no_inline() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("identity").apply(Term::var("x")))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay())
                .lambda("identity")
                .apply(Term::var("y").lambda("y").lambda(NO_INLINE)),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("x"))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay()),
        };

        compare_optimization(expected, program, |p| p.identity_reducer());
    }

    #[test]
    fn identity_reduce_no_inline_2() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("identity").apply(Term::var("x")))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay())
                .lambda(NO_INLINE)
                .lambda("identity")
                .apply(Term::var("y").lambda("y").lambda(NO_INLINE)),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("x"))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay())
                .lambda(NO_INLINE),
        };

        compare_optimization(expected, program, |p| p.identity_reducer());
    }

    #[test]
    fn inline_reduce_delay_sha() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("x"))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay()),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256().apply(Term::byte_string(vec![]).delay()),
        };

        compare_optimization(expected, program, |p| p.inline_reducer());
    }

    #[test]
    fn inline_reduce_0_occurrence() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay()),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256(),
        };

        compare_optimization(expected, program, |p| p.inline_reducer());
    }

    #[test]
    fn wrap_data_reduce_i_data() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::equals_data()
                .apply(Term::i_data().apply(Term::un_i_data().apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                ))))
                .apply(Term::i_data().apply(Term::integer(1.into())))
                .lambda("x"),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::equals_data()
                .apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                ))
                .apply(Term::data(Data::integer(1.into())))
                .lambda("x"),
        };

        compare_optimization(expected, program, |p| p.cast_data_reducer());
    }

    #[test]
    fn wrap_data_reduce_un_i_data() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::equals_integer()
                .apply(Term::un_i_data().apply(Term::i_data().apply(Term::integer(1.into()))))
                .apply(Term::un_i_data().apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                )))
                .lambda("x"),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::equals_integer()
                .apply(Term::integer(1.into()))
                .apply(Term::integer(5.into()))
                .lambda("x"),
        };

        compare_optimization(expected, program, |p| p.cast_data_reducer());
    }

    #[test]
    fn curry_reducer_test_1() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::add_integer()
                .apply(Term::var("x"))
                .apply(Term::integer(1.into()))
                .lambda("x")
                .apply(
                    Term::add_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("y")),
                )
                .lambda("y")
                .apply(
                    Term::add_integer()
                        .apply(Term::var("g"))
                        .apply(Term::integer(1.into())),
                )
                .lambda("g"),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::var("add_one_curried")
                .apply(Term::var("x"))
                .lambda("x")
                .apply(Term::var("add_one_curried").apply(Term::var("y")))
                .lambda("y")
                .apply(Term::var("add_one_curried").apply(Term::var("g")))
                .lambda("add_one_curried")
                .apply(Term::add_integer().apply(Term::integer(1.into())))
                .lambda("g"),
        };

        compare_optimization(expected, program, |p| p.builtin_curry_reducer());
    }

    #[test]
    fn curry_reducer_test_2() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::add_integer()
                .apply(Term::var("x"))
                .apply(Term::integer(1.into()))
                .lambda("x")
                .apply(
                    Term::add_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("y")),
                )
                .lambda("y")
                .apply(Term::integer(5.into())),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::add_integer()
                .apply(Term::var("x"))
                .apply(Term::integer(1.into()))
                .lambda("x")
                .apply(
                    Term::add_integer()
                        .apply(Term::integer(1.into()))
                        .apply(Term::var("y")),
                )
                .lambda("y")
                .apply(Term::integer(5.into())),
        };

        compare_optimization(expected, program, |p| p.builtin_curry_reducer());
    }

    #[test]
    fn curry_reducer_test_3() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::var("equivalence")
                .lambda("equivalence")
                .apply(
                    Term::equals_integer()
                        .apply(Term::integer(0.into()))
                        .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("tuple_index_0")))
                        .if_then_else(
                            Term::equals_integer()
                                .apply(Term::integer(0.into()))
                                .apply(
                                    Term::var(CONSTR_INDEX_EXPOSER)
                                        .apply(Term::var("tuple_index_1")),
                                )
                                .if_then_else(
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
                                        .apply(Term::un_i_data().apply(
                                            Term::fst_pair().apply(Term::var("field_0_pair")),
                                        ))
                                        .lambda("y2")
                                        .apply(Term::un_i_data().apply(
                                            Term::snd_pair().apply(Term::var("field_0_pair")),
                                        ))
                                        .lambda("field_0_pair")
                                        .apply(
                                            Term::mk_pair_data()
                                                .apply(
                                                    Term::head_list()
                                                        .apply(Term::var("__list_data")),
                                                )
                                                .apply(Term::head_list().apply(Term::var("__tail")))
                                                .lambda("__tail")
                                                .apply(
                                                    Term::tail_list()
                                                        .apply(Term::var("__list_data")),
                                                )
                                                .lambda("__list_data")
                                                .apply(
                                                    Term::unlist_data().apply(
                                                        Term::head_list().apply(Term::var(
                                                            "tuple_index_1_fields",
                                                        )),
                                                    ),
                                                ),
                                        )
                                        .lambda("tuple_index_1_fields")
                                        .apply(
                                            Term::var(CONSTR_FIELDS_EXPOSER)
                                                .apply(Term::var("tuple_index_1")),
                                        )
                                        .delay(),
                                    Term::var("clauses_delayed"),
                                )
                                .force()
                                .lambda("x1")
                                .apply(
                                    Term::un_i_data()
                                        .apply(Term::fst_pair().apply(Term::var("field_0_pair"))),
                                )
                                .lambda("y1")
                                .apply(
                                    Term::un_i_data()
                                        .apply(Term::snd_pair().apply(Term::var("field_0_pair"))),
                                )
                                .lambda("field_0_pair")
                                .apply(
                                    Term::mk_pair_data()
                                        .apply(Term::head_list().apply(Term::var("__list_data")))
                                        .apply(Term::head_list().apply(Term::var("__tail")))
                                        .lambda("__tail")
                                        .apply(Term::tail_list().apply(Term::var("__list_data")))
                                        .lambda("__list_data")
                                        .apply(
                                            Term::unlist_data().apply(
                                                Term::head_list()
                                                    .apply(Term::var("tuple_index_0_fields")),
                                            ),
                                        ),
                                )
                                .lambda("tuple_index_0_fields")
                                .apply(
                                    Term::var(CONSTR_FIELDS_EXPOSER)
                                        .apply(Term::var("tuple_index_0")),
                                )
                                .delay(),
                            Term::var("clauses_delayed"),
                        )
                        .force()
                        .lambda("clauses_delayed")
                        .apply(
                            Term::equals_integer()
                                .apply(Term::integer(1.into()))
                                .apply(
                                    Term::var(CONSTR_INDEX_EXPOSER)
                                        .apply(Term::var("tuple_index_0")),
                                )
                                .if_then_else(
                                    Term::equals_integer()
                                        .apply(Term::integer(1.into()))
                                        .apply(
                                            Term::var(CONSTR_INDEX_EXPOSER)
                                                .apply(Term::var("tuple_index_1")),
                                        )
                                        .if_then_else(
                                            Term::bool(true).delay(),
                                            Term::var("clauses_delayed"),
                                        )
                                        .force()
                                        .delay(),
                                    Term::var("clauses_delayed"),
                                )
                                .force()
                                .lambda("clauses_delayed")
                                .apply(
                                    Term::equals_integer()
                                        .apply(Term::integer(1.into()))
                                        .apply(
                                            Term::var(CONSTR_INDEX_EXPOSER)
                                                .apply(Term::var("tuple_index_0")),
                                        )
                                        .if_then_else(
                                            Term::equals_integer()
                                                .apply(Term::integer(0.into()))
                                                .apply(
                                                    Term::var(CONSTR_INDEX_EXPOSER)
                                                        .apply(Term::var("tuple_index_1")),
                                                )
                                                .if_then_else(
                                                    Term::bool(false).delay(),
                                                    Term::var("clauses_delayed"),
                                                )
                                                .force()
                                                .delay(),
                                            Term::var("clauses_delayed"),
                                        )
                                        .force()
                                        .lambda("clauses_delayed")
                                        .apply(Term::bool(false).delay())
                                        .delay(),
                                )
                                .delay(),
                        )
                        .lambda("tuple_index_0")
                        .apply(Term::fst_pair().apply(Term::var("input")))
                        .lambda("tuple_index_1")
                        .apply(Term::snd_pair().apply(Term::var("input")))
                        .lambda("input")
                        .apply(
                            Term::mk_pair_data()
                                .apply(Term::var("ec1"))
                                .apply(Term::var("ec2")),
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
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::var("equivalence")
                .lambda("equivalence")
                .apply(
                    Term::var("equals_integer_0_curried")
                        .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("tuple_index_0")))
                        .if_then_else(
                            Term::var("equals_integer_0_curried")
                                .apply(
                                    Term::var(CONSTR_INDEX_EXPOSER)
                                        .apply(Term::var("tuple_index_1")),
                                )
                                .if_then_else(
                                    Term::var("equals_integer_0_curried")
                                        .apply(
                                            Term::subtract_integer()
                                                .apply(Term::var("x2"))
                                                .apply(Term::var("x1")),
                                        )
                                        .delayed_if_then_else(
                                            Term::var("equals_integer_0_curried").apply(
                                                Term::subtract_integer()
                                                    .apply(Term::var("y2"))
                                                    .apply(Term::var("y1")),
                                            ),
                                            Term::bool(false),
                                        )
                                        .lambda("x2")
                                        .apply(Term::un_i_data().apply(
                                            Term::fst_pair().apply(Term::var("field_0_pair")),
                                        ))
                                        .lambda("y2")
                                        .apply(Term::un_i_data().apply(
                                            Term::snd_pair().apply(Term::var("field_0_pair")),
                                        ))
                                        .lambda("field_0_pair")
                                        .apply(
                                            Term::mk_pair_data()
                                                .apply(
                                                    Term::head_list()
                                                        .apply(Term::var("__list_data")),
                                                )
                                                .apply(Term::head_list().apply(Term::var("__tail")))
                                                .lambda("__tail")
                                                .apply(
                                                    Term::tail_list()
                                                        .apply(Term::var("__list_data")),
                                                )
                                                .lambda("__list_data")
                                                .apply(
                                                    Term::unlist_data().apply(
                                                        Term::head_list().apply(Term::var(
                                                            "tuple_index_1_fields",
                                                        )),
                                                    ),
                                                ),
                                        )
                                        .lambda("tuple_index_1_fields")
                                        .apply(
                                            Term::var(CONSTR_FIELDS_EXPOSER)
                                                .apply(Term::var("tuple_index_1")),
                                        )
                                        .delay(),
                                    Term::var("clauses_delayed"),
                                )
                                .force()
                                .lambda("x1")
                                .apply(
                                    Term::un_i_data()
                                        .apply(Term::fst_pair().apply(Term::var("field_0_pair"))),
                                )
                                .lambda("y1")
                                .apply(
                                    Term::un_i_data()
                                        .apply(Term::snd_pair().apply(Term::var("field_0_pair"))),
                                )
                                .lambda("field_0_pair")
                                .apply(
                                    Term::mk_pair_data()
                                        .apply(Term::head_list().apply(Term::var("__list_data")))
                                        .apply(Term::head_list().apply(Term::var("__tail")))
                                        .lambda("__tail")
                                        .apply(Term::tail_list().apply(Term::var("__list_data")))
                                        .lambda("__list_data")
                                        .apply(
                                            Term::unlist_data().apply(
                                                Term::head_list()
                                                    .apply(Term::var("tuple_index_0_fields")),
                                            ),
                                        ),
                                )
                                .lambda("tuple_index_0_fields")
                                .apply(
                                    Term::var(CONSTR_FIELDS_EXPOSER)
                                        .apply(Term::var("tuple_index_0")),
                                )
                                .delay(),
                            Term::var("clauses_delayed"),
                        )
                        .force()
                        .lambda("clauses_delayed")
                        .apply(
                            Term::var("equals_integer_1_curried")
                                .apply(
                                    Term::var(CONSTR_INDEX_EXPOSER)
                                        .apply(Term::var("tuple_index_0")),
                                )
                                .if_then_else(
                                    Term::var("equals_integer_1_curried")
                                        .apply(
                                            Term::var(CONSTR_INDEX_EXPOSER)
                                                .apply(Term::var("tuple_index_1")),
                                        )
                                        .if_then_else(
                                            Term::bool(true).delay(),
                                            Term::var("clauses_delayed"),
                                        )
                                        .force()
                                        .delay(),
                                    Term::var("clauses_delayed"),
                                )
                                .force()
                                .lambda("clauses_delayed")
                                .apply(
                                    Term::var("equals_integer_1_curried")
                                        .apply(
                                            Term::var(CONSTR_INDEX_EXPOSER)
                                                .apply(Term::var("tuple_index_0")),
                                        )
                                        .if_then_else(
                                            Term::var("equals_integer_0_curried")
                                                .apply(
                                                    Term::var(CONSTR_INDEX_EXPOSER)
                                                        .apply(Term::var("tuple_index_1")),
                                                )
                                                .if_then_else(
                                                    Term::bool(false).delay(),
                                                    Term::var("clauses_delayed"),
                                                )
                                                .force()
                                                .delay(),
                                            Term::var("clauses_delayed"),
                                        )
                                        .force()
                                        .lambda("clauses_delayed")
                                        .apply(Term::bool(false).delay())
                                        .delay(),
                                )
                                .lambda("equals_integer_1_curried")
                                .apply(Term::equals_integer().apply(Term::integer(1.into())))
                                .delay(),
                        )
                        .lambda("equals_integer_0_curried")
                        .apply(Term::equals_integer().apply(Term::integer(0.into())))
                        .lambda("tuple_index_0")
                        .apply(Term::fst_pair().apply(Term::var("input")))
                        .lambda("tuple_index_1")
                        .apply(Term::snd_pair().apply(Term::var("input")))
                        .lambda("input")
                        .apply(
                            Term::mk_pair_data()
                                .apply(Term::var("ec1"))
                                .apply(Term::var("ec2")),
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
        };

        compare_optimization(expected, program, |p| p.builtin_curry_reducer());
    }

    #[test]
    fn curry_reducer_test_4() {
        let program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::bool(true)
                .delayed_if_then_else(
                    Term::integer(2.into()),
                    Term::add_integer()
                        .apply(
                            Term::add_integer()
                                .apply(Term::var("x"))
                                .apply(Term::var("y")),
                        )
                        .apply(Term::var("z"))
                        .lambda("z")
                        .apply(
                            Term::index_bytearray()
                                .apply(Term::byte_string(vec![
                                    1, 2, 4, 8, 16, 32, 64, 128, 255, 255,
                                ]))
                                .apply(Term::integer(35.into())),
                        )
                        .lambda("y")
                        .apply(
                            Term::index_bytearray()
                                .apply(Term::byte_string(vec![
                                    1, 2, 4, 8, 16, 32, 64, 128, 255, 255,
                                ]))
                                .apply(Term::integer(35.into())),
                        ),
                )
                .lambda("x")
                .apply(
                    Term::bool(true).delayed_if_then_else(
                        Term::integer(1.into()),
                        Term::index_bytearray()
                            .apply(Term::byte_string(vec![
                                1, 2, 4, 8, 16, 32, 64, 128, 255, 255,
                            ]))
                            .apply(Term::integer(35.into())),
                    ),
                ),
        };

        let expected = Program {
            version: (1, 0, 0),
            term: Term::bool(true)
                .delayed_if_then_else(
                    Term::integer(2.into()),
                    Term::add_integer()
                        .apply(
                            Term::add_integer()
                                .apply(Term::var("x"))
                                .apply(Term::var("y")),
                        )
                        .apply(Term::var("z"))
                        .lambda("z")
                        .apply(Term::var("good_curry").apply(Term::integer(35.into())))
                        .lambda("y")
                        .apply(Term::var("good_curry").apply(Term::integer(35.into()))),
                )
                .lambda("x")
                .apply(Term::bool(true).delayed_if_then_else(
                    Term::integer(1.into()),
                    Term::var("good_curry").apply(Term::integer(35.into())),
                ))
                .lambda("good_curry")
                .apply(Term::index_bytearray().apply(Term::byte_string(vec![
                    1, 2, 4, 8, 16, 32, 64, 128, 255, 255,
                ]))),
        };

        compare_optimization(expected, program, |p| p.builtin_curry_reducer());
    }
}
