use std::{rc::Rc, vec};

use indexmap::IndexMap;
use itertools::Itertools;

use pallas_primitives::babbage::{BigInt, PlutusData};

use crate::{
    ast::{Constant, Data, Name, Program, Term, Type},
    builtins::DefaultFunction,
};
#[derive(PartialEq, Clone, Debug)]
pub enum BuiltinArgs {
    TwoArgs(Term<Name>, Term<Name>),
    ThreeArgs(Term<Name>, Term<Name>, Term<Name>),
    TwoArgsAnyOrder(Term<Name>, Term<Name>),
}

impl BuiltinArgs {
    fn args_to_curried_tree(self, scope: &Scope) -> CurriedTree {
        match self {
            BuiltinArgs::TwoArgs(arg1, arg2) | BuiltinArgs::TwoArgsAnyOrder(arg1, arg2) => {
                CurriedTree::Branch {
                    node: arg1,
                    multiple_occurrences: false,
                    children: vec![CurriedTree::Leaf {
                        node: arg2,
                        multiple_occurrences: false,
                        scope: scope.clone(),
                    }],
                    scope: scope.clone(),
                }
            }

            BuiltinArgs::ThreeArgs(arg1, arg2, arg3) => CurriedTree::Branch {
                node: arg1,
                multiple_occurrences: false,
                children: vec![CurriedTree::Branch {
                    node: arg2,
                    multiple_occurrences: false,
                    children: vec![CurriedTree::Leaf {
                        node: arg3,
                        multiple_occurrences: false,
                        scope: scope.clone(),
                    }],
                    scope: scope.clone(),
                }],
                scope: scope.clone(),
            },
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum CurriedTree {
    Branch {
        node: Term<Name>,
        multiple_occurrences: bool,
        children: Vec<CurriedTree>,
        scope: Scope,
    },
    Leaf {
        node: Term<Name>,
        multiple_occurrences: bool,
        scope: Scope,
    },
}

impl CurriedTree {
    pub fn node(&self) -> &Term<Name> {
        match self {
            CurriedTree::Branch { node, .. } => node,
            CurriedTree::Leaf { node, .. } => node,
        }
    }

    pub fn node_mut(&mut self) -> &mut Term<Name> {
        match self {
            CurriedTree::Branch { node, .. } => node,
            CurriedTree::Leaf { node, .. } => node,
        }
    }

    pub fn multiple_occurrences(&self) -> bool {
        match self {
            CurriedTree::Branch {
                multiple_occurrences,
                ..
            } => *multiple_occurrences,
            CurriedTree::Leaf {
                multiple_occurrences,
                ..
            } => *multiple_occurrences,
        }
    }

    pub fn merge_node_by_path(self, path: BuiltinArgs, scope: &Scope) -> CurriedTree {
        match (self, path) {
            (
                CurriedTree::Branch {
                    node,
                    mut children,
                    scope: branch_scope,
                    ..
                },
                BuiltinArgs::TwoArgs(_, arg2) | BuiltinArgs::TwoArgsAnyOrder(_, arg2),
            ) => {
                if let Some(CurriedTree::Leaf {
                    multiple_occurrences,
                    scope: leaf_scope,
                    ..
                }) = children.iter_mut().find(|child| child.node() == &arg2)
                {
                    // So here we mutate the found child of the branch to update the leaf
                    // Note this is a 2 arg builtin so the depth is 2
                    *multiple_occurrences = true;
                    *leaf_scope = leaf_scope.common_ancestor(scope);
                    CurriedTree::Branch {
                        node,
                        multiple_occurrences: true,
                        children,
                        scope: branch_scope.common_ancestor(scope),
                    }
                } else {
                    children.push(CurriedTree::Leaf {
                        node: arg2,
                        multiple_occurrences: false,
                        scope: scope.clone(),
                    });
                    CurriedTree::Branch {
                        node,
                        multiple_occurrences: true,
                        children,
                        scope: branch_scope.common_ancestor(scope),
                    }
                }
            }
            (
                CurriedTree::Branch {
                    node,
                    mut children,
                    scope: branch_scope,
                    ..
                },
                BuiltinArgs::ThreeArgs(_, arg2, arg3),
            ) => {
                if let Some(CurriedTree::Branch {
                    multiple_occurrences: child_multiple_occurrences,
                    children: child_children,
                    scope: child_scope,
                    ..
                }) = children.iter_mut().find(|child| child.node() == &arg2)
                {
                    if let Some(CurriedTree::Leaf {
                        multiple_occurrences: leaf_multiple_occurrences,
                        scope: leaf_scope,
                        ..
                    }) = child_children
                        .iter_mut()
                        .find(|child| child.node() == &arg3)
                    {
                        // So here we mutate the found child of the branch to update the leaf
                        // Note this is a 3 arg builtin so the depth is 3
                        *leaf_multiple_occurrences = true;
                        *leaf_scope = leaf_scope.common_ancestor(scope);
                        *child_multiple_occurrences = true;
                        *child_scope = child_scope.common_ancestor(scope);
                        CurriedTree::Branch {
                            node,
                            multiple_occurrences: true,
                            children,
                            scope: branch_scope.common_ancestor(scope),
                        }
                    } else {
                        child_children.push(CurriedTree::Leaf {
                            node: arg3,
                            multiple_occurrences: false,
                            scope: scope.clone(),
                        });
                        *child_multiple_occurrences = true;
                        *child_scope = child_scope.common_ancestor(scope);
                        CurriedTree::Branch {
                            node,
                            multiple_occurrences: true,
                            children,
                            scope: branch_scope.common_ancestor(scope),
                        }
                    }
                } else {
                    children.push(BuiltinArgs::TwoArgs(arg2, arg3).args_to_curried_tree(scope));
                    CurriedTree::Branch {
                        node,
                        multiple_occurrences: true,
                        children,
                        scope: branch_scope.common_ancestor(scope),
                    }
                }
            }
            // Since all args are always added to the tree. The minimum depth of a tree is 2 and max is 3
            // Therefore we can't have a leaf at the root level of the match
            _ => unreachable!(),
        }
    }

    pub fn prune_single_occurrences(self) -> Self {
        match self {
            CurriedTree::Branch { node, children, .. } => {
                // Implement your logic here for the Branch variant
                // For example, you might want to prune children here
                todo!()
            }
            _ => unreachable!(),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct CurriedBuiltin {
    pub func: DefaultFunction,
    /// For use with subtract integer where we can flip the order of the arguments
    /// if the second argument is a constant
    pub flipped: bool,
    pub children: Vec<CurriedTree>,
}

impl CurriedBuiltin {
    pub fn merge_node_by_path(self, path: BuiltinArgs, scope: &Scope) -> CurriedBuiltin {
        let mut children = self.children.clone();

        match &path {
            // First we just peak at the first arg to see if it was curried before
            BuiltinArgs::TwoArgs(arg1, _) | BuiltinArgs::ThreeArgs(arg1, _, _) => {
                if let Some(child) = children.iter_mut().find(|child| child.node() == arg1) {
                    // mutate child here so we don't have to recreate the whole tree
                    // We pass in the scope so we can get the common ancestor if the arg was curried before
                    *child = child.clone().merge_node_by_path(path, scope);
                    CurriedBuiltin {
                        func: self.func,
                        flipped: self.flipped,
                        children,
                    }
                } else {
                    // We found a new arg so we add it to the list of children
                    children.push(path.args_to_curried_tree(scope));
                    CurriedBuiltin {
                        func: self.func,
                        flipped: self.flipped,
                        children,
                    }
                }
            }
            BuiltinArgs::TwoArgsAnyOrder(arg1, arg2) => {
                // This is the special case where we search by both args before adding a new child
                if let Some(child) = children.iter_mut().find(|child| child.node() == arg1) {
                    *child = child.clone().merge_node_by_path(path, scope);
                    CurriedBuiltin {
                        func: self.func,
                        flipped: self.flipped,
                        children,
                    }
                } else if let Some(child) = children.iter_mut().find(|child| child.node() == arg2) {
                    // If we found a curried argument using arg2 then we flip the order of the args
                    // before merging into the tree
                    *child = child.clone().merge_node_by_path(
                        BuiltinArgs::TwoArgsAnyOrder(arg2.clone(), arg1.clone()),
                        scope,
                    );

                    CurriedBuiltin {
                        func: self.func,
                        flipped: self.flipped,
                        children,
                    }
                } else {
                    // We found a new arg so we add it to the list of children
                    children.push(path.args_to_curried_tree(scope));
                    CurriedBuiltin {
                        func: self.func,
                        flipped: self.flipped,
                        children,
                    }
                }
            }
        }
    }

    pub fn prune_single_occurrences(mut self) -> Self {
        self.children = self
            .children
            .into_iter()
            .filter(|child| child.multiple_occurrences())
            .map(|child| child.prune_single_occurrences())
            .collect_vec();
        self
    }
}

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub enum ScopePath {
    FUNC,
    ARG,
}

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
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
        Scope { scope: new_scope }
    }

    pub fn pop(&self) -> Self {
        let mut new_scope = self.scope.clone();
        new_scope.pop();
        Scope { scope: new_scope }
    }

    pub fn common_ancestor(&self, other: &Scope) -> Self {
        Scope {
            scope: self
                .scope
                .iter()
                .zip(other.scope.iter())
                .map_while(|(a, b)| if a == b { Some(a) } else { None })
                .cloned()
                .collect_vec(),
        }
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
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

pub fn is_order_agnostic_builtin(func: DefaultFunction) -> bool {
    matches!(
        func,
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
pub fn can_curry_builtin(func: DefaultFunction) -> bool {
    matches!(
        func,
        DefaultFunction::AddInteger
            | DefaultFunction::SubtractInteger
            | DefaultFunction::MultiplyInteger
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
            | DefaultFunction::Bls12_381_G1_Add
            | DefaultFunction::Bls12_381_G2_Add
    )
}

impl Program<Name> {
    fn traverse_uplc_with(
        self,
        with: &mut impl FnMut(Option<usize>, &mut Term<Name>, Vec<(usize, Term<Name>)>, &Scope),
    ) -> Self {
        let mut term = self.term;
        let scope = Scope { scope: vec![] };
        let arg_stack = vec![];
        let mut id_gen = IdGen::new();

        Self::traverse_uplc_with_helper(&mut term, &scope, arg_stack, &mut id_gen, with);
        Program {
            version: self.version,
            term,
        }
    }

    fn traverse_uplc_with_helper(
        term: &mut Term<Name>,
        scope: &Scope,
        mut arg_stack: Vec<(usize, Term<Name>)>,
        id_gen: &mut IdGen,
        with: &mut impl FnMut(Option<usize>, &mut Term<Name>, Vec<(usize, Term<Name>)>, &Scope),
    ) {
        match term {
            Term::Apply { function, argument } => {
                let arg = Rc::make_mut(argument);
                let argument_arg_stack = vec![];
                Self::traverse_uplc_with_helper(
                    arg,
                    &scope.push(ScopePath::ARG),
                    argument_arg_stack,
                    id_gen,
                    with,
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
                );

                with(Some(apply_id), term, vec![], scope);
            }
            Term::Delay(d) => {
                let d = Rc::make_mut(d);
                // First we recurse further to reduce the inner terms before coming back up to the Delay
                Self::traverse_uplc_with_helper(d, scope, arg_stack, id_gen, with);
                with(None, term, vec![], scope);
            }
            Term::Lambda { body, .. } => {
                let body = Rc::make_mut(body);
                // Lambda pops one item off the arg stack. If there is no item then it is a unsaturated lambda
                let args = arg_stack.pop().map(|arg| vec![arg]).unwrap_or_default();

                // Pass in either one or zero args.
                Self::traverse_uplc_with_helper(body, scope, arg_stack, id_gen, with);
                with(None, term, args, scope);
            }

            Term::Force(f) => {
                let f = Rc::make_mut(f);
                Self::traverse_uplc_with_helper(f, scope, arg_stack, id_gen, with);
                with(None, term, vec![], scope);
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
                with(None, term, args, scope);
            }
            term => {
                with(None, term, vec![], scope);
            }
        }
    }

    pub fn lambda_reducer(self) -> Self {
        let mut lambda_applied_ids = vec![];

        self.traverse_uplc_with(&mut |id, term, mut arg_stack, _scope| {
            match term {
                Term::Apply { function, .. } => {
                    // We are apply some arg so now we unwrap the id of the applied arg
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
                        match arg_term {
                            Term::Constant(c) if matches!(c.as_ref(), Constant::String(_)) => {}
                            Term::Constant(_) | Term::Var(_) | Term::Builtin(_) => {
                                let body = Rc::make_mut(body);
                                // mutates body to replace all var occurrences with the arg
                                *body = substitute_var(body, parameter_name.clone(), &arg_term);
                                lambda_applied_ids.push(arg_id);
                                *term = body.clone();
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

    pub fn builtin_force_reducer(self) -> Program<Name> {
        let mut builtin_map = IndexMap::new();

        let program = self.traverse_uplc_with(&mut |_id, term, _arg_stack, _scope| {
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

        Program {
            version: program.version,
            term,
        }
    }

    pub fn inline_reducer(self) -> Program<Name> {
        let mut lambda_applied_ids = vec![];
        let mut identity_applied_ids = vec![];
        // TODO: Remove extra traversals
        self.traverse_uplc_with(&mut |_id, term, _arg_stack, _scope| {
            // Since this one just inlines single occurrences. It's probably not needed
            if let Term::Apply { function, argument } = term {
                let func = Rc::make_mut(function);

                if let Term::Lambda {
                    parameter_name,
                    body,
                } = func
                {
                    if let Term::Var(name) = body.as_ref() {
                        if name.as_ref() == parameter_name.as_ref() {
                            *term = argument.as_ref().clone();
                        }
                    }
                }
            }
        })
        .traverse_uplc_with(&mut |id, term, mut arg_stack, _scope| {
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
                    if let Some((
                        arg_id,
                        Term::Lambda {
                            parameter_name: identity_name,
                            body: identity_body,
                        },
                    )) = arg_stack.pop()
                    {
                        if let Term::Var(identity_var) = identity_body.as_ref() {
                            if identity_var.as_ref() == identity_name.as_ref() {
                                // Replace all applied usages of identity with the arg
                                let temp_term =
                                    replace_identity_usage(body.as_ref(), parameter_name.clone());
                                // Have to check if the body still has any occurrences of the parameter
                                // After attempting replacement
                                if var_occurrences(body.as_ref(), parameter_name.clone()) > 0 {
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

                Term::Constr { .. } => todo!(),
                Term::Case { .. } => todo!(),
                _ => {}
            }
        })
        .traverse_uplc_with(&mut |id, term, mut arg_stack, _scope| match term {
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
                    let body = Rc::make_mut(body);
                    let occurrences = var_occurrences(body, parameter_name.clone());
                    let delays = delayed_execution(body);

                    if occurrences == 1
                        && (delays == 0
                            || matches!(
                                &arg_term,
                                Term::Var(_)
                                    | Term::Constant(_)
                                    | Term::Delay(_)
                                    | Term::Lambda { .. }
                                    | Term::Builtin(_),
                            ))
                    {
                        *body = substitute_var(body, parameter_name.clone(), &arg_term);

                        lambda_applied_ids.push(arg_id);
                        *term = body.clone();

                    // This will strip out unused terms that can't throw an error by themselves
                    } else if occurrences == 0
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

    pub fn force_delay_reducer(self) -> Program<Name> {
        self.traverse_uplc_with(&mut |_id, term, _arg_stack, _scope| {
            if let Term::Force(f) = term {
                let f = Rc::make_mut(f);

                if let Term::Delay(body) = f {
                    *term = body.as_ref().clone();
                }
            }
        })
    }

    pub fn cast_data_reducer(self) -> Program<Name> {
        let mut applied_ids = vec![];

        self.traverse_uplc_with(&mut |id, term, mut arg_stack, _scope| {
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
    // WIP
    pub fn builtin_curry_reducer(self) -> Program<Name> {
        let mut curried_terms = vec![];
        let mut curry_applied_ids: Vec<usize> = vec![];

        let a = self.traverse_uplc_with(&mut |_id, term, arg_stack, scope| match term {
            Term::Builtin(func) => {
                if can_curry_builtin(*func) && arg_stack.len() == func.arity() {
                    let mut scope = scope.clone();

                    // Get upper scope of the function plus args
                    // So for example if the scope is [.., ARG, ARG, FUNC]
                    // we want to pop off the last 3 to get the scope right above the function applications
                    for _ in 0..func.arity() {
                        scope = scope.pop();
                    }

                    let is_order_agnostic = is_order_agnostic_builtin(*func);

                    // In the case of order agnostic builtins we want to sort the args by constant first
                    // This gives us the opportunity to curry constants that often pop up in the code
                    let mut ordered_arg_stack = arg_stack
                        .into_iter()
                        .map(|(_, arg)| arg)
                        .sorted_by(|arg1, arg2| {
                            // sort by constant first if the builtin is order agnostic
                            if is_order_agnostic {
                                if matches!(arg1, Term::Constant(_))
                                    && matches!(arg2, Term::Constant(_))
                                {
                                    std::cmp::Ordering::Equal
                                } else if matches!(arg1, Term::Constant(_)) {
                                    std::cmp::Ordering::Less
                                } else if matches!(arg2, Term::Constant(_)) {
                                    std::cmp::Ordering::Greater
                                } else {
                                    std::cmp::Ordering::Equal
                                }
                            } else {
                                std::cmp::Ordering::Equal
                            }
                        });

                    let builtin_args = if ordered_arg_stack.len() == 2 && is_order_agnostic {
                        // This is the special case where the order of args is irrelevant to the builtin
                        // An example is addInteger or multiplyInteger
                        BuiltinArgs::TwoArgsAnyOrder(
                            ordered_arg_stack.next().unwrap(),
                            ordered_arg_stack.next().unwrap(),
                        )
                    } else if ordered_arg_stack.len() == 2 {
                        BuiltinArgs::TwoArgs(
                            ordered_arg_stack.next().unwrap(),
                            ordered_arg_stack.next().unwrap(),
                        )
                    } else {
                        // println!("ARG STACK FOR FUNC {:#?}, {:#?}", ordered_arg_stack, func);
                        BuiltinArgs::ThreeArgs(
                            ordered_arg_stack.next().unwrap(),
                            ordered_arg_stack.next().unwrap(),
                            ordered_arg_stack.next().unwrap(),
                        )
                    };

                    // First we see if we have already curried this builtin before
                    if let Some(curried_builtin) = curried_terms
                        .iter_mut()
                        .find(|curried_term: &&mut CurriedBuiltin| curried_term.func == *func)
                    {
                        // We found it the builtin was curried before
                        // So now we merge the new args into the existing curried builtin
                        *curried_builtin = (*curried_builtin)
                            .clone()
                            .merge_node_by_path(builtin_args, &scope);
                    } else {
                        // Brand new buitlin so we add it to the list
                        curried_terms.push(CurriedBuiltin {
                            func: *func,
                            flipped: false,
                            children: vec![builtin_args.args_to_curried_tree(&scope)],
                        });
                    }
                }
            }

            Term::Constr { .. } => todo!(),
            Term::Case { .. } => todo!(),
            _ => {}
        });

        println!("CURRIED ARGS");
        for (index, curried_term) in curried_terms.into_iter().enumerate() {
            println!("index is {:#?}, term is {:#?}", index, curried_term,);
        }

        a
    }
}

fn var_occurrences(term: &Term<Name>, search_for: Rc<Name>) -> usize {
    match term {
        Term::Var(name) => {
            if name.as_ref() == search_for.as_ref() {
                1
            } else {
                0
            }
        }
        Term::Delay(body) => var_occurrences(body.as_ref(), search_for),
        Term::Lambda {
            parameter_name,
            body,
        } => {
            if parameter_name.clone() != search_for {
                var_occurrences(body.as_ref(), search_for)
            } else {
                0
            }
        }
        Term::Apply { function, argument } => {
            var_occurrences(function.as_ref(), search_for.clone())
                + var_occurrences(argument.as_ref(), search_for)
        }
        Term::Force(x) => var_occurrences(x.as_ref(), search_for),
        Term::Case { .. } => todo!(),
        Term::Constr { .. } => todo!(),
        _ => 0,
    }
}

fn delayed_execution(term: &Term<Name>) -> usize {
    match term {
        Term::Delay(body) => 1 + delayed_execution(body.as_ref()),
        Term::Lambda { body, .. } => 1 + delayed_execution(body.as_ref()),
        Term::Apply { function, argument } => {
            delayed_execution(function.as_ref()) + delayed_execution(argument.as_ref())
        }
        Term::Force(x) => delayed_execution(x.as_ref()),
        Term::Case { constr, branches } => {
            1 + delayed_execution(constr.as_ref())
                + branches
                    .iter()
                    .fold(0, |acc, branch| acc + delayed_execution(branch))
        }
        Term::Constr { fields, .. } => fields
            .iter()
            .fold(0, |acc, field| acc + delayed_execution(field)),
        _ => 0,
    }
}

fn substitute_var(term: &Term<Name>, original: Rc<Name>, replace_with: &Term<Name>) -> Term<Name> {
    match term {
        Term::Var(name) => {
            if name.as_ref() == original.as_ref() {
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
            if parameter_name.as_ref() != original.as_ref() {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: Rc::new(substitute_var(body.as_ref(), original, replace_with)),
                }
            } else {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: body.clone(),
                }
            }
        }
        Term::Apply { function, argument } => Term::Apply {
            function: Rc::new(substitute_var(
                function.as_ref(),
                original.clone(),
                replace_with,
            )),
            argument: Rc::new(substitute_var(argument.as_ref(), original, replace_with)),
        },
        Term::Force(f) => Term::Force(Rc::new(substitute_var(f.as_ref(), original, replace_with))),
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
            if parameter_name.as_ref() != original.as_ref() {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: Rc::new(replace_identity_usage(body.as_ref(), original)),
                }
            } else {
                Term::Lambda {
                    parameter_name: parameter_name.clone(),
                    body: body.clone(),
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

            if name.as_ref() == original.as_ref() {
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

#[cfg(test)]
mod tests {

    use pallas_primitives::babbage::{BigInt, PlutusData};
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{Constant, Data, Name, NamedDeBruijn, Program, Term},
        builtins::DefaultFunction,
        parser::interner::Interner,
    };

    #[test]
    fn lambda_reduce_var() {
        let mut program = Program {
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

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::var("foo").lambda("foo").apply(
                Term::constr_data()
                    .apply(Term::integer(3.into()))
                    .apply(Term::list_values(vec![])),
            ),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = program.lambda_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn lambda_reduce_constant() {
        let mut program = Program {
            version: (1, 0, 0),
            term: Term::var("foo")
                .lambda("foo")
                .apply(Term::integer(6.into())),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::integer(6.into()),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();
        let actual = program.lambda_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn lambda_reduce_builtin() {
        let mut program = Program {
            version: (1, 0, 0),
            term: Term::var("foo").lambda("foo").apply(Term::add_integer()),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::add_integer(),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.lambda_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn lambda_reduce_force_delay_error_lam() {
        let mut program: Program<Name> = Program {
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

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
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

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.lambda_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn wrap_data_reduce_i_data() {
        let mut program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::equals_data()
                .apply(Term::i_data().apply(Term::un_i_data().apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                ))))
                .apply(Term::i_data().apply(Term::integer(1.into())))
                .lambda("x"),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::equals_data()
                .apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                ))
                .apply(Term::data(Data::integer(1.into())))
                .lambda("x"),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.cast_data_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn wrap_data_reduce_un_i_data() {
        let mut program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::equals_integer()
                .apply(Term::un_i_data().apply(Term::i_data().apply(Term::integer(1.into()))))
                .apply(Term::un_i_data().apply(Term::Constant(
                    Constant::Data(PlutusData::BigInt(BigInt::Int(5.into()))).into(),
                )))
                .lambda("x"),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::equals_integer()
                .apply(Term::integer(1.into()))
                .apply(Term::integer(5.into()))
                .lambda("x"),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.cast_data_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
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

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::var("__cons_list_wrapped")
                .apply(Term::var("x"))
                .apply(
                    Term::var("__tail_list_wrapped")
                        .apply(Term::var("__head_list_wrapped").apply(Term::var("y"))),
                )
                .lambda("x")
                .lambda("y")
                .lambda("__cons_list_wrapped")
                .apply(Term::mk_cons())
                .lambda("__head_list_wrapped")
                .apply(Term::head_list())
                .lambda("__tail_list_wrapped")
                .apply(Term::tail_list()),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let mut actual = program.builtin_force_reducer();

        let mut interner = Interner::new();

        interner.program(&mut actual);

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
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

        let mut expected = Program {
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

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let mut actual = program.builtin_force_reducer();

        let mut interner = Interner::new();

        interner.program(&mut actual);

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn inline_reduce_delay_sha() {
        let mut program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("x"))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay()),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256().apply(Term::byte_string(vec![]).delay()),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.inline_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn inline_reduce_identity() {
        let mut program: Program<Name> = Program {
            version: (1, 0, 0),
            term: Term::sha2_256()
                .apply(Term::var("identity").apply(Term::var("x")))
                .lambda("x")
                .apply(Term::byte_string(vec![]).delay())
                .lambda("identity")
                .apply(Term::var("y").lambda("y")),
        };

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256().apply(Term::byte_string(vec![]).delay()),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.inline_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn inline_reduce_identity_param() {
        let mut program: Program<Name> = Program {
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

        let mut interner = Interner::new();

        interner.program(&mut program);

        let mut expected = Program {
            version: (1, 0, 0),
            term: Term::sha2_256().apply(
                Term::var("with")
                    .apply(Term::var("x"))
                    .lambda("with")
                    .lambda("x")
                    .apply(Term::byte_string(vec![]).delay())
                    .apply(Term::var("y").lambda("y")),
            ),
        };

        let mut interner = Interner::new();

        interner.program(&mut expected);

        let expected: Program<NamedDeBruijn> = expected.try_into().unwrap();

        let actual = program.inline_reducer();

        let actual: Program<NamedDeBruijn> = actual.try_into().unwrap();

        assert_eq!(actual, expected);
    }
}
