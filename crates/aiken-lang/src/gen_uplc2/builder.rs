use indexmap::IndexMap;

use crate::{
    ast::TypedDataType,
    builtins::bool,
    gen_uplc::builder::{lookup_data_type_by_tipo, DataTypeKey, FunctionAccessKey},
    tipo::{TypeVar, ValueConstructorVariant},
};
use std::sync::Arc;

use crate::{
    ast::{BinOp, ClauseGuard, Constant, UnOp},
    tipo::Type,
};

use super::tree::{AirExpression, AirStatement, AirTree};

#[derive(Clone, Debug)]
pub struct TreePath {
    path: Vec<(usize, usize)>,
}

impl TreePath {
    pub fn new() -> Self {
        TreePath { path: vec![] }
    }

    pub fn push(&mut self, depth: usize, index: usize) {
        self.path.push((depth, index));
    }

    pub fn pop(&mut self) {
        self.path.pop();
    }
}

pub struct IndexCounter {
    current_index: usize,
}

impl IndexCounter {
    pub fn new() -> Self {
        IndexCounter { current_index: 0 }
    }

    pub fn next(&mut self) -> usize {
        let current_index = self.current_index;
        self.current_index += 1;
        current_index
    }
}

pub fn get_generic_id_and_type(tipo: &Type, param: &Type) -> Vec<(u64, Arc<Type>)> {
    let mut generics_ids = vec![];

    if let Some(id) = tipo.get_generic() {
        generics_ids.push((id, param.clone().into()));
        return generics_ids;
    }

    for (tipo, param_type) in tipo
        .get_inner_types()
        .iter()
        .zip(param.get_inner_types().iter())
    {
        generics_ids.append(&mut get_generic_id_and_type(tipo, param_type));
    }
    generics_ids
}

pub fn convert_opaque_type(
    t: &Arc<Type>,
    data_types: &IndexMap<DataTypeKey, &TypedDataType>,
) -> Arc<Type> {
    if check_replaceable_opaque_type(t, data_types) && matches!(t.as_ref(), Type::App { .. }) {
        let data_type = lookup_data_type_by_tipo(data_types, t).unwrap();
        let new_type_fields = data_type.typed_parameters;

        let mono_types: IndexMap<u64, Arc<Type>>;
        let mut mono_type_vec = vec![];

        for (tipo, param) in new_type_fields.iter().zip(t.arg_types().unwrap()) {
            mono_type_vec.append(&mut get_generic_id_and_type(tipo, &param));
        }
        mono_types = mono_type_vec.into_iter().collect();

        let generic_type = &data_type.constructors[0].arguments[0].tipo;

        let mono_type = find_and_replace_generics(generic_type, &mono_types);

        convert_opaque_type(&mono_type, data_types)
    } else {
        match t.as_ref() {
            Type::App {
                public,
                module,
                name,
                args,
            } => {
                let mut new_args = vec![];
                for arg in args {
                    let arg = convert_opaque_type(arg, data_types);
                    new_args.push(arg);
                }
                Type::App {
                    public: *public,
                    module: module.clone(),
                    name: name.clone(),
                    args: new_args,
                }
                .into()
            }
            Type::Fn { args, ret } => {
                let mut new_args = vec![];
                for arg in args {
                    let arg = convert_opaque_type(arg, data_types);
                    new_args.push(arg);
                }

                let ret = convert_opaque_type(ret, data_types);

                Type::Fn {
                    args: new_args,
                    ret,
                }
                .into()
            }
            Type::Var { tipo: var_tipo } => {
                if let TypeVar::Link { tipo } = &var_tipo.borrow().clone() {
                    convert_opaque_type(tipo, data_types)
                } else {
                    t.clone()
                }
            }
            Type::Tuple { elems } => {
                let mut new_elems = vec![];
                for arg in elems {
                    let arg = convert_opaque_type(arg, data_types);
                    new_elems.push(arg);
                }
                Type::Tuple { elems: new_elems }.into()
            }
        }
    }
}

pub fn check_replaceable_opaque_type(
    t: &Arc<Type>,
    data_types: &IndexMap<DataTypeKey, &TypedDataType>,
) -> bool {
    let data_type = lookup_data_type_by_tipo(data_types, t);

    if let Some(data_type) = data_type {
        let data_type_args = &data_type.constructors[0].arguments;
        data_type_args.len() == 1 && data_type.opaque && data_type.constructors.len() == 1
    } else {
        false
    }
}

pub fn find_and_replace_generics(
    tipo: &Arc<Type>,
    mono_types: &IndexMap<u64, Arc<Type>>,
) -> Arc<Type> {
    if let Some(id) = tipo.get_generic() {
        // If a generic does not have a type we know of
        // like a None in option then just use same type
        mono_types.get(&id).unwrap_or(tipo).clone()
    } else if tipo.is_generic() {
        match &**tipo {
            Type::App {
                args,
                public,
                module,
                name,
            } => {
                let mut new_args = vec![];
                for arg in args {
                    let arg = find_and_replace_generics(arg, mono_types);
                    new_args.push(arg);
                }
                let t = Type::App {
                    args: new_args,
                    public: *public,
                    module: module.clone(),
                    name: name.clone(),
                };
                t.into()
            }
            Type::Fn { args, ret } => {
                let mut new_args = vec![];
                for arg in args {
                    let arg = find_and_replace_generics(arg, mono_types);
                    new_args.push(arg);
                }

                let ret = find_and_replace_generics(ret, mono_types);

                let t = Type::Fn {
                    args: new_args,
                    ret,
                };

                t.into()
            }
            Type::Tuple { elems } => {
                let mut new_elems = vec![];
                for elem in elems {
                    let elem = find_and_replace_generics(elem, mono_types);
                    new_elems.push(elem);
                }
                let t = Type::Tuple { elems: new_elems };
                t.into()
            }
            Type::Var { tipo: var_tipo } => {
                let var_type = var_tipo.as_ref().borrow().clone();

                match var_type {
                    TypeVar::Link { tipo } => {
                        find_and_replace_generics(&tipo, mono_types);
                        tipo
                    }
                    TypeVar::Generic { .. } | TypeVar::Unbound { .. } => unreachable!(),
                }
            }
        }
    } else {
        tipo.clone()
    }
}

pub fn constants_ir(literal: &Constant) -> AirTree {
    match literal {
        Constant::Int { value, .. } => AirTree::int(value),
        Constant::String { value, .. } => AirTree::string(value),
        Constant::ByteArray { bytes, .. } => AirTree::byte_array(bytes.clone()),
    }
}

pub fn handle_clause_guard(clause_guard: &ClauseGuard<Arc<Type>>) -> AirTree {
    match clause_guard {
        ClauseGuard::Not { value, .. } => {
            let val = handle_clause_guard(value);

            AirTree::unop(UnOp::Not, val)
        }
        ClauseGuard::Equals { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::Eq, bool(), left, right)
        }
        ClauseGuard::NotEquals { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::NotEq, bool(), left, right)
        }
        ClauseGuard::GtInt { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::GtInt, bool(), left, right)
        }
        ClauseGuard::GtEqInt { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::GtEqInt, bool(), left, right)
        }
        ClauseGuard::LtInt { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::LtInt, bool(), left, right)
        }
        ClauseGuard::LtEqInt { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::LtEqInt, bool(), left, right)
        }
        ClauseGuard::Or { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::Or, bool(), left, right)
        }
        ClauseGuard::And { left, right, .. } => {
            let left = handle_clause_guard(left);
            let right = handle_clause_guard(right);

            AirTree::binop(BinOp::And, bool(), left, right)
        }
        ClauseGuard::Var { tipo, name, .. } => AirTree::local_var(name, tipo.clone()),
        ClauseGuard::Constant(constant) => constants_ir(constant),
    }
}

pub fn erase_opaque_operations(
    air_tree: &mut AirTree,
    data_types: &IndexMap<DataTypeKey, &TypedDataType>,
) {
    traverse_tree_with(air_tree, &mut TreePath::new(), 0, 0, &|air_tree, _| {
        if let AirTree::Expression(e) = air_tree {
            match e {
                AirExpression::Constr { tipo, args, .. } => {
                    if check_replaceable_opaque_type(tipo, data_types) {
                        *air_tree = args.pop().unwrap();
                    }
                }
                AirExpression::RecordAccess { tipo, record, .. } => {
                    if check_replaceable_opaque_type(tipo, data_types) {
                        *air_tree = (**record).clone();
                    }
                }

                _ => {}
            }
        } else if let AirTree::Statement {
            statement:
                AirStatement::FieldsExpose {
                    record, indices, ..
                },
            hoisted_over: Some(hoisted_over),
        } = air_tree
        {
            let name = indices[0].1.clone();
            if check_replaceable_opaque_type(&record.get_type(), data_types) {
                *air_tree = AirTree::let_assignment(name, (**record).clone())
                    .hoist_over((**hoisted_over).clone())
            }
        }
    });
}

fn traverse_tree_with(
    air_tree: &mut AirTree,
    tree_path: &mut TreePath,
    current_depth: usize,
    depth_index: usize,
    with: &impl Fn(&mut AirTree, &TreePath),
) {
    let mut index_count = IndexCounter::new();
    tree_path.push(current_depth, depth_index);
    match air_tree {
        AirTree::Statement {
            statement,
            hoisted_over: Some(hoisted_over),
        } => {
            match statement {
                AirStatement::Let { value, .. } => {
                    traverse_tree_with(
                        value,
                        tree_path,
                        current_depth + 1,
                        index_count.next(),
                        with,
                    );
                }
                AirStatement::DefineFunc { func_body, .. } => {
                    traverse_tree_with(
                        func_body,
                        tree_path,
                        current_depth + 1,
                        index_count.next(),
                        with,
                    );
                }
                AirStatement::AssertConstr { constr, .. } => {
                    traverse_tree_with(
                        constr,
                        tree_path,
                        current_depth + 1,
                        index_count.next(),
                        with,
                    );
                }
                AirStatement::AssertBool { value, .. } => {
                    traverse_tree_with(
                        value,
                        tree_path,
                        current_depth + 1,
                        index_count.next(),
                        with,
                    );
                }
                AirStatement::ClauseGuard { pattern, .. } => {
                    traverse_tree_with(
                        pattern,
                        tree_path,
                        current_depth + 1,
                        index_count.next(),
                        with,
                    );
                }
                AirStatement::ListClauseGuard { .. } => {}
                AirStatement::TupleGuard { .. } => {}
                AirStatement::FieldsExpose { record, .. } => {
                    traverse_tree_with(
                        record,
                        tree_path,
                        current_depth + 1,
                        index_count.next(),
                        with,
                    );
                }
                AirStatement::ListAccessor { list, .. } => {
                    traverse_tree_with(
                        list,
                        tree_path,
                        current_depth + 1,
                        index_count.next(),
                        with,
                    );
                }
                AirStatement::ListExpose { .. } => {}
                AirStatement::TupleAccessor { tuple, .. } => {
                    traverse_tree_with(
                        tuple,
                        tree_path,
                        current_depth + 1,
                        index_count.next(),
                        with,
                    );
                }
                AirStatement::NoOp => {}
            };

            traverse_tree_with(
                hoisted_over,
                tree_path,
                current_depth + 1,
                index_count.next(),
                with,
            );
        }
        AirTree::Expression(e) => match e {
            AirExpression::List { items, .. } => {
                for item in items {
                    traverse_tree_with(
                        item,
                        tree_path,
                        current_depth + 1,
                        index_count.next(),
                        with,
                    );
                }
            }
            AirExpression::Tuple { items, .. } => {
                for item in items {
                    traverse_tree_with(
                        item,
                        tree_path,
                        current_depth + 1,
                        index_count.next(),
                        with,
                    );
                }
            }
            AirExpression::Var {
                constructor,
                name,
                variant_name,
            } => {
                todo!()
            }
            AirExpression::Call { func, args, .. } => {
                traverse_tree_with(func, tree_path, current_depth + 1, index_count.next(), with);

                for arg in args {
                    traverse_tree_with(arg, tree_path, current_depth + 1, index_count.next(), with);
                }
            }
            AirExpression::Fn { func_body, .. } => {
                traverse_tree_with(
                    func_body,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::Builtin { args, .. } => {
                for arg in args {
                    traverse_tree_with(arg, tree_path, current_depth + 1, index_count.next(), with);
                }
            }
            AirExpression::BinOp { left, right, .. } => {
                traverse_tree_with(left, tree_path, current_depth + 1, index_count.next(), with);

                traverse_tree_with(
                    right,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::UnOp { arg, .. } => {
                traverse_tree_with(arg, tree_path, current_depth + 1, index_count.next(), with);
            }
            AirExpression::UnWrapData { value, .. } => {
                traverse_tree_with(
                    value,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::WrapData { value, .. } => {
                traverse_tree_with(
                    value,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::When {
                subject, clauses, ..
            } => {
                traverse_tree_with(
                    subject,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );

                traverse_tree_with(
                    clauses,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::Clause {
                pattern,
                then,
                otherwise,
                ..
            } => {
                traverse_tree_with(
                    pattern,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );

                traverse_tree_with(then, tree_path, current_depth + 1, index_count.next(), with);

                traverse_tree_with(
                    otherwise,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::ListClause {
                then, otherwise, ..
            } => {
                traverse_tree_with(then, tree_path, current_depth + 1, index_count.next(), with);

                traverse_tree_with(
                    otherwise,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::WrapClause { then, otherwise } => {
                traverse_tree_with(then, tree_path, current_depth + 1, index_count.next(), with);

                traverse_tree_with(
                    otherwise,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::TupleClause {
                then, otherwise, ..
            } => {
                traverse_tree_with(then, tree_path, current_depth + 1, index_count.next(), with);

                traverse_tree_with(
                    otherwise,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::Finally { pattern, then } => {
                traverse_tree_with(
                    pattern,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );

                traverse_tree_with(then, tree_path, current_depth + 1, index_count.next(), with);
            }
            AirExpression::If {
                pattern,
                then,
                otherwise,
                ..
            } => {
                traverse_tree_with(
                    pattern,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );

                traverse_tree_with(then, tree_path, current_depth + 1, index_count.next(), with);

                traverse_tree_with(
                    otherwise,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::Constr { args, .. } => {
                for arg in args {
                    traverse_tree_with(arg, tree_path, current_depth + 1, index_count.next(), with);
                }
            }
            AirExpression::RecordUpdate { record, args, .. } => {
                traverse_tree_with(
                    record,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
                for arg in args {
                    traverse_tree_with(arg, tree_path, current_depth + 1, index_count.next(), with);
                }
            }
            AirExpression::RecordAccess { record, .. } => {
                traverse_tree_with(
                    record,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::TupleIndex { tuple, .. } => {
                traverse_tree_with(
                    tuple,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::Trace { msg, then, .. } => {
                traverse_tree_with(msg, tree_path, current_depth + 1, index_count.next(), with);

                traverse_tree_with(then, tree_path, current_depth + 1, index_count.next(), with);
            }
            AirExpression::FieldsEmpty { constr } => {
                traverse_tree_with(
                    constr,
                    tree_path,
                    current_depth + 1,
                    index_count.next(),
                    with,
                );
            }
            AirExpression::ListEmpty { list } => {
                traverse_tree_with(list, tree_path, current_depth + 1, index_count.next(), with);
            }
            _ => {}
        },
        _ => unreachable!(),
    }

    with(air_tree, tree_path);

    tree_path.pop();
}
