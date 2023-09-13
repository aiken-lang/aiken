use std::{collections::HashMap, rc::Rc};

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use uplc::{
    ast::{Constant as UplcConstant, Name, Term, Type as UplcType},
    builder::{CONSTR_FIELDS_EXPOSER, CONSTR_INDEX_EXPOSER},
    builtins::DefaultFunction,
    machine::{
        runtime::{convert_constr_to_tag, ANY_TAG},
        value::to_pallas_bigint,
    },
    Constr, KeyValuePairs, PlutusData,
};

use crate::{
    ast::{
        AssignmentKind, DataType, Pattern, Span, TypedArg, TypedClause, TypedClauseGuard,
        TypedDataType, TypedPattern,
    },
    builtins::{bool, void},
    expr::TypedExpr,
    tipo::{PatternConstructor, TypeVar, ValueConstructor, ValueConstructorVariant},
};

use crate::{
    ast::{BinOp, ClauseGuard, Constant, UnOp},
    tipo::Type,
};

use super::{
    air::Air,
    tree::{AirExpression, AirStatement, AirTree, TreePath},
};

#[derive(Clone, Debug)]
pub enum CodeGenFunction {
    Function { body: AirTree, params: Vec<String> },
    Link(String),
}

#[derive(Clone, Debug)]
pub enum UserFunction {
    Function {
        body: AirTree,
        deps: Vec<(FunctionAccessKey, String)>,
        params: Vec<String>,
    },
    Link(String),
}

#[derive(Clone, Debug)]
pub struct FuncComponents {
    pub ir: Vec<Air>,
    pub dependencies: Vec<FunctionAccessKey>,
    pub args: Vec<String>,
    pub recursive: bool,
    pub defined_by_zero_arg: bool,
    pub is_code_gen_func: bool,
}

#[derive(Clone, Eq, Debug, PartialEq, Hash)]
pub struct ConstrFieldKey {
    pub local_var: String,
    pub field_name: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DataTypeKey {
    pub module_name: String,
    pub defined_type: String,
}

pub type ConstrUsageKey = String;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FunctionAccessKey {
    pub module_name: String,
    pub function_name: String,
}

#[derive(Clone, Debug)]
pub struct AssignmentProperties {
    pub value_type: Rc<Type>,
    pub kind: AssignmentKind,
    pub remove_unused: bool,
    pub full_check: bool,
}

#[derive(Clone, Debug)]

pub struct ClauseProperties {
    pub clause_var_name: String,
    pub complex_clause: bool,
    pub needs_constr_var: bool,
    pub original_subject_name: String,
    pub final_clause: bool,
    pub specific_clause: SpecificClause,
}
#[derive(Clone, Debug)]
pub enum SpecificClause {
    ConstrClause,
    ListClause {
        defined_tails_index: i64,
        defined_tails: Vec<String>,
        checked_index: i64,
    },
    TupleClause {
        defined_tuple_indices: IndexSet<(usize, String)>,
    },
}

impl ClauseProperties {
    pub fn init(t: &Rc<Type>, constr_var: String, subject_name: String) -> Self {
        if t.is_list() {
            ClauseProperties {
                clause_var_name: constr_var,
                complex_clause: false,
                original_subject_name: subject_name.clone(),
                final_clause: false,
                needs_constr_var: false,
                specific_clause: SpecificClause::ListClause {
                    defined_tails_index: 0,
                    defined_tails: vec![subject_name],
                    checked_index: -1,
                },
            }
        } else if t.is_tuple() {
            ClauseProperties {
                clause_var_name: constr_var,
                complex_clause: false,
                original_subject_name: subject_name,
                needs_constr_var: false,
                final_clause: false,
                specific_clause: SpecificClause::TupleClause {
                    defined_tuple_indices: IndexSet::new(),
                },
            }
        } else {
            ClauseProperties {
                clause_var_name: constr_var,
                complex_clause: false,
                original_subject_name: subject_name,
                needs_constr_var: false,
                final_clause: false,
                specific_clause: SpecificClause::ConstrClause,
            }
        }
    }

    pub fn init_inner(
        t: &Rc<Type>,
        constr_var: String,
        subject_name: String,
        final_clause: bool,
    ) -> Self {
        if t.is_list() {
            ClauseProperties {
                clause_var_name: constr_var,
                complex_clause: false,
                original_subject_name: subject_name,
                final_clause,
                needs_constr_var: false,
                specific_clause: SpecificClause::ListClause {
                    defined_tails_index: 0,
                    defined_tails: vec![],
                    checked_index: -1,
                },
            }
        } else if t.is_tuple() {
            ClauseProperties {
                clause_var_name: constr_var,
                complex_clause: false,
                original_subject_name: subject_name,
                needs_constr_var: false,
                final_clause,
                specific_clause: SpecificClause::TupleClause {
                    defined_tuple_indices: IndexSet::new(),
                },
            }
        } else {
            ClauseProperties {
                clause_var_name: constr_var,
                complex_clause: false,
                original_subject_name: subject_name,
                needs_constr_var: false,
                final_clause,
                specific_clause: SpecificClause::ConstrClause,
            }
        }
    }
}

pub fn get_generic_id_and_type(tipo: &Type, param: &Type) -> Vec<(u64, Rc<Type>)> {
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

pub fn lookup_data_type_by_tipo(
    data_types: &IndexMap<DataTypeKey, &TypedDataType>,
    tipo: &Type,
) -> Option<DataType<Rc<Type>>> {
    match tipo {
        Type::Fn { ret, .. } => match ret.as_ref() {
            Type::App { module, name, .. } => {
                let data_type_key = DataTypeKey {
                    module_name: module.clone(),
                    defined_type: name.clone(),
                };
                data_types.get(&data_type_key).map(|item| (*item).clone())
            }
            _ => None,
        },
        Type::App { module, name, .. } => {
            let data_type_key = DataTypeKey {
                module_name: module.clone(),
                defined_type: name.clone(),
            };

            data_types.get(&data_type_key).map(|item| (*item).clone())
        }
        Type::Var { tipo } => {
            if let TypeVar::Link { tipo } = &*tipo.borrow() {
                lookup_data_type_by_tipo(data_types, tipo)
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn get_arg_type_name(tipo: &Type) -> String {
    match tipo {
        Type::App { name, args, .. } => {
            let inner_args = args.iter().map(|arg| get_arg_type_name(arg)).collect_vec();
            format!("{}_{}", name, inner_args.join("_"))
        }
        Type::Var { tipo } => match tipo.borrow().clone() {
            TypeVar::Link { tipo } => get_arg_type_name(tipo.as_ref()),
            _ => unreachable!(),
        },
        Type::Tuple { elems } => {
            let inner_args = elems.iter().map(|arg| get_arg_type_name(arg)).collect_vec();
            inner_args.join("_")
        }
        _ => unreachable!(),
    }
}

pub fn convert_opaque_type(
    t: &Rc<Type>,
    data_types: &IndexMap<DataTypeKey, &TypedDataType>,
) -> Rc<Type> {
    if check_replaceable_opaque_type(t, data_types) && matches!(t.as_ref(), Type::App { .. }) {
        let data_type = lookup_data_type_by_tipo(data_types, t).unwrap();
        let new_type_fields = data_type.typed_parameters;

        let mut mono_type_vec = vec![];

        for (tipo, param) in new_type_fields.iter().zip(t.arg_types().unwrap()) {
            mono_type_vec.append(&mut get_generic_id_and_type(tipo, &param));
        }
        let mono_types = mono_type_vec.into_iter().collect();

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
    t: &Rc<Type>,
    data_types: &IndexMap<DataTypeKey, &TypedDataType>,
) -> bool {
    let data_type = lookup_data_type_by_tipo(data_types, t);

    if let Some(data_type) = data_type {
        assert!(!data_type.constructors.is_empty());
        let data_type_args = &data_type.constructors[0].arguments;
        data_type_args.len() == 1 && data_type.opaque && data_type.constructors.len() == 1
    } else {
        false
    }
}

pub fn find_and_replace_generics(
    tipo: &Rc<Type>,
    mono_types: &IndexMap<u64, Rc<Type>>,
) -> Rc<Type> {
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
                    TypeVar::Link { tipo } => find_and_replace_generics(&tipo, mono_types),
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

pub fn handle_clause_guard(clause_guard: &TypedClauseGuard) -> AirTree {
    match clause_guard {
        ClauseGuard::Not { value, .. } => {
            let val = handle_clause_guard(value);

            AirTree::unop(UnOp::Not, val)
        }
        ClauseGuard::Equals { left, right, .. } => {
            let left_child = handle_clause_guard(left);
            let right_child = handle_clause_guard(right);

            AirTree::binop(BinOp::Eq, bool(), left_child, right_child, left.tipo())
        }
        ClauseGuard::NotEquals { left, right, .. } => {
            let left_child = handle_clause_guard(left);
            let right_child = handle_clause_guard(right);

            AirTree::binop(BinOp::NotEq, bool(), left_child, right_child, left.tipo())
        }
        ClauseGuard::GtInt { left, right, .. } => {
            let left_child = handle_clause_guard(left);
            let right_child = handle_clause_guard(right);

            AirTree::binop(BinOp::GtInt, bool(), left_child, right_child, left.tipo())
        }
        ClauseGuard::GtEqInt { left, right, .. } => {
            let left_child = handle_clause_guard(left);
            let right_child = handle_clause_guard(right);

            AirTree::binop(BinOp::GtEqInt, bool(), left_child, right_child, left.tipo())
        }
        ClauseGuard::LtInt { left, right, .. } => {
            let left_child = handle_clause_guard(left);
            let right_child = handle_clause_guard(right);

            AirTree::binop(BinOp::LtInt, bool(), left_child, right_child, left.tipo())
        }
        ClauseGuard::LtEqInt { left, right, .. } => {
            let left_child = handle_clause_guard(left);
            let right_child = handle_clause_guard(right);

            AirTree::binop(BinOp::LtEqInt, bool(), left_child, right_child, left.tipo())
        }
        ClauseGuard::Or { left, right, .. } => {
            let left_child = handle_clause_guard(left);
            let right_child = handle_clause_guard(right);

            AirTree::binop(BinOp::Or, bool(), left_child, right_child, left.tipo())
        }
        ClauseGuard::And { left, right, .. } => {
            let left_child = handle_clause_guard(left);
            let right_child = handle_clause_guard(right);

            AirTree::binop(BinOp::And, bool(), left_child, right_child, left.tipo())
        }
        ClauseGuard::Var { tipo, name, .. } => AirTree::local_var(name, tipo.clone()),
        ClauseGuard::Constant(constant) => constants_ir(constant),
    }
}

pub fn get_variant_name(t: &Rc<Type>) -> String {
    if t.is_string() {
        "_string".to_string()
    } else if t.is_int() {
        "_int".to_string()
    } else if t.is_bool() {
        "_bool".to_string()
    } else if t.is_bytearray() {
        "_bytearray".to_string()
    } else if t.is_map() {
        let mut full_type = vec!["_map".to_string()];
        let pair_type = &t.get_inner_types()[0];
        let fst_type = &pair_type.get_inner_types()[0];
        let snd_type = &pair_type.get_inner_types()[1];
        full_type.push(get_variant_name(fst_type));
        full_type.push(get_variant_name(snd_type));
        full_type.join("")
    } else if t.is_list() {
        let full_type = "_list".to_string();
        let list_type = &t.get_inner_types()[0];

        format!("{}{}", full_type, get_variant_name(list_type))
    } else if t.is_tuple() {
        let mut full_type = vec!["_tuple".to_string()];

        let inner_types = t.get_inner_types();

        for arg_type in inner_types {
            full_type.push(get_variant_name(&arg_type));
        }
        full_type.join("")
    } else if t.is_unbound() {
        "_unbound".to_string()
    } else {
        let full_type = "_data".to_string();

        if t.is_generic() {
            panic!("FOUND A POLYMORPHIC TYPE. EXPECTED MONOMORPHIC TYPE");
        }

        full_type
    }
}

pub fn monomorphize(air_tree: &mut AirTree, mono_types: &IndexMap<u64, Rc<Type>>) {
    let mut held_types = air_tree.mut_held_types();

    while let Some(tipo) = held_types.pop() {
        *tipo = find_and_replace_generics(tipo, mono_types);
    }
}

pub fn erase_opaque_type_operations(
    air_tree: &mut AirTree,
    data_types: &IndexMap<DataTypeKey, &TypedDataType>,
) {
    if let AirTree::Expression(AirExpression::Constr { tipo, args, .. }) = air_tree {
        if check_replaceable_opaque_type(tipo, data_types) {
            let arg = args.pop().unwrap();
            if let AirTree::Expression(AirExpression::CastToData { value, .. }) = arg {
                *air_tree = *value;
            } else {
                *air_tree = arg;
            }
        }
    }

    let mut held_types = air_tree.mut_held_types();

    while let Some(tipo) = held_types.pop() {
        *tipo = convert_opaque_type(tipo, data_types);
    }
}

/// Determine whether this air_tree node introduces any shadowing over `potential_matches`
pub fn find_introduced_variables(air_tree: &AirTree) -> Vec<String> {
    match air_tree {
        AirTree::Statement {
            statement: AirStatement::Let { name, .. },
            ..
        } => vec![name.clone()],
        AirTree::Statement {
            statement: AirStatement::TupleGuard { indices, .. },
            ..
        }
        | AirTree::Expression(AirExpression::TupleClause { indices, .. }) => {
            indices.iter().map(|(_, name)| name).cloned().collect()
        }
        AirTree::Expression(AirExpression::Fn { params, .. }) => params.to_vec(),
        AirTree::Statement {
            statement: AirStatement::ListAccessor { names, .. },
            ..
        } => names.clone(),
        AirTree::Statement {
            statement:
                AirStatement::ListExpose {
                    tail,
                    tail_head_names,
                    ..
                },
            ..
        } => {
            let mut ret = vec![];
            if let Some((_, head)) = tail {
                ret.push(head.clone())
            }

            for name in tail_head_names.iter().map(|(_, head)| head) {
                ret.push(name.clone());
            }
            ret
        }
        AirTree::Statement {
            statement: AirStatement::TupleAccessor { names, .. },
            ..
        } => names.clone(),
        AirTree::Statement {
            statement: AirStatement::FieldsExpose { indices, .. },
            ..
        } => indices.iter().map(|(_, name, _)| name).cloned().collect(),
        _ => vec![],
    }
}

/// Determine whether a function is recursive, and if so, get the arguments
pub fn is_recursive_function_call<'a>(
    air_tree: &'a AirTree,
    func_key: &FunctionAccessKey,
    variant: &String,
) -> (bool, Option<&'a Vec<AirTree>>) {
    if let AirTree::Expression(AirExpression::Call { func, args, .. }) = air_tree {
        if let AirTree::Expression(AirExpression::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { name, module, .. },
                    ..
                },
            variant_name,
            ..
        }) = func.as_ref()
        {
            if name == &func_key.function_name
                && module == &func_key.module_name
                && variant == variant_name
            {
                return (true, Some(args));
            }
        }
    }
    (false, None)
}

pub fn identify_recursive_static_params(
    air_tree: &mut AirTree,
    tree_path: &TreePath,
    func_params: &[String],
    func_key: &FunctionAccessKey,
    variant: &String,
    shadowed_parameters: &mut HashMap<String, TreePath>,
    potential_recursive_statics: &mut Vec<String>,
) {
    // Find whether any of the potential recursive statics get shadowed (because even if we pass in the same referenced name, it might not be static)
    for introduced_variable in find_introduced_variables(air_tree) {
        if potential_recursive_statics.contains(&introduced_variable) {
            shadowed_parameters.insert(introduced_variable, tree_path.clone());
        }
    }
    // Otherwise, if this is a recursive call site, disqualify anything that is different (or the same, but shadowed)
    if let (true, Some(args)) = is_recursive_function_call(air_tree, func_key, variant) {
        for (param, arg) in func_params.iter().zip(args) {
            if let Some((idx, _)) = potential_recursive_statics
                .iter()
                .find_position(|&p| p == param)
            {
                // Check if we pass something different in this recursive call site
                // by different, we mean
                // - a variable that is bound to a different name
                // - a variable with the same name, but that was shadowed in an ancestor scope
                // - any other type of expression
                let param_is_different = match arg {
                    AirTree::Expression(AirExpression::Var { name, .. }) => {
                        // "shadowed in an ancestor scope" means "the definition scope is a prefix of our scope"
                        name != param
                            || if let Some(p) = shadowed_parameters.get(param) {
                                p.common_ancestor(tree_path) == *p
                            } else {
                                false
                            }
                    }
                    _ => true,
                };
                // If so, then we disqualify this parameter from being a recursive static parameter
                if param_is_different {
                    potential_recursive_statics.remove(idx);
                }
            }
        }
    }
}

pub fn modify_self_calls(
    body: &mut AirTree,
    func_key: &FunctionAccessKey,
    variant: &String,
    func_params: &[String],
) -> Vec<String> {
    let mut potential_recursive_statics = func_params.to_vec();
    // identify which parameters are recursively nonstatic (i.e. get modified before the self-call)
    // TODO: this would be a lot simpler if each `Var`, `Let`, function argument, etc. had a unique identifier
    // rather than just a name; this would let us track if the Var passed to itself was the same value as the method argument
    let mut shadowed_parameters: HashMap<String, TreePath> = HashMap::new();
    body.traverse_tree_with(
        &mut |air_tree: &mut AirTree, tree_path| {
            identify_recursive_static_params(
                air_tree,
                tree_path,
                func_params,
                func_key,
                variant,
                &mut shadowed_parameters,
                &mut potential_recursive_statics,
            );
        },
        false,
    );

    // Find the index of any recursively static parameters,
    // so we can remove them from the call-site of each recursive call
    let recursive_static_indexes: Vec<_> = func_params
        .iter()
        .enumerate()
        .filter(|&(_, p)| potential_recursive_statics.contains(p))
        .map(|(idx, _)| idx)
        .collect();

    // Modify any self calls to remove recursive static parameters and append `self` as a parameter for the recursion
    body.traverse_tree_with(
        &mut |air_tree: &mut AirTree, _| {
            if let AirTree::Expression(AirExpression::Call { func, args, .. }) = air_tree {
                if let AirTree::Expression(AirExpression::Var {
                    constructor:
                        ValueConstructor {
                            variant: ValueConstructorVariant::ModuleFn { name, module, .. },
                            ..
                        },
                    variant_name,
                    ..
                }) = func.as_ref()
                {
                    if name == &func_key.function_name
                        && module == &func_key.module_name
                        && variant == variant_name
                    {
                        // Remove any static-recursive-parameters, because they'll be bound statically
                        // above the recursive part of the function
                        // note: assumes that static_recursive_params is sorted
                        for arg in recursive_static_indexes.iter().rev() {
                            args.remove(*arg);
                        }
                        let mut new_args = vec![func.as_ref().clone()];
                        new_args.append(args);
                        *args = new_args;
                    }
                }
            }
        },
        true,
    );
    let recursive_nonstatics = func_params
        .iter()
        .filter(|p| !potential_recursive_statics.contains(p))
        .cloned()
        .collect();
    recursive_nonstatics
}

pub fn pattern_has_conditions(
    pattern: &TypedPattern,
    data_types: &IndexMap<DataTypeKey, &TypedDataType>,
) -> bool {
    match pattern {
        Pattern::List { .. } | Pattern::Int { .. } => true,
        Pattern::Tuple { elems, .. } => elems
            .iter()
            .any(|elem| pattern_has_conditions(elem, data_types)),
        Pattern::Constructor {
            arguments, tipo, ..
        } => {
            let data_type =
                lookup_data_type_by_tipo(data_types, tipo).expect("Data type not found");

            data_type.constructors.len() > 1
                || arguments
                    .iter()
                    .any(|arg| pattern_has_conditions(&arg.value, data_types))
        }
        Pattern::Assign { pattern, .. } => pattern_has_conditions(pattern, data_types),
        Pattern::Var { .. } | Pattern::Discard { .. } => false,
    }
}

// TODO: write some tests
pub fn rearrange_list_clauses(
    clauses: Vec<TypedClause>,
    data_types: &IndexMap<DataTypeKey, &TypedDataType>,
) -> Vec<TypedClause> {
    let mut sorted_clauses = clauses;

    // if we have a list sort clauses so we can plug holes for cases not covered by clauses
    // Now we sort by elements + tail if possible and otherwise leave an index in place if var or discard
    // This is a stable sort. i.e. matching elements amounts will remain in user given order.
    sorted_clauses = sorted_clauses
        .into_iter()
        .enumerate()
        .sorted_by(|(index1, clause1), (index2, clause2)| {
            let mut clause_pattern1 = &clause1.pattern;
            let mut clause_pattern2 = &clause2.pattern;

            if let Pattern::Assign { pattern, .. } = clause_pattern1 {
                clause_pattern1 = pattern;
            }

            if let Pattern::Assign { pattern, .. } = clause_pattern2 {
                clause_pattern2 = pattern;
            }

            let clause1_len = match clause_pattern1 {
                Pattern::List { elements, tail, .. } => {
                    Some(elements.len() + usize::from(tail.is_some() && clause1.guard.is_none()))
                }
                _ if clause1.guard.is_none() => Some(100000),
                _ => None,
            };

            let clause2_len = match clause_pattern2 {
                Pattern::List { elements, tail, .. } => {
                    Some(elements.len() + usize::from(tail.is_some() && clause2.guard.is_none()))
                }
                _ if clause2.guard.is_none() => Some(100001),
                _ => None,
            };

            if let Some(clause1_len) = clause1_len {
                if let Some(clause2_len) = clause2_len {
                    return clause1_len.cmp(&clause2_len);
                }
            }

            index1.cmp(index2)
        })
        .map(|(_, item)| item)
        .collect_vec();

    let mut final_clauses = sorted_clauses.clone();
    let mut holes_to_fill = vec![];
    let mut last_clause_index = 0;
    let mut last_clause_set = false;
    let mut wild_card_clause_elems = 0;

    // If we have a catch all, use that. Otherwise use todo which will result in error
    // TODO: fill in todo label with description
    let plug_in_then = &|index: usize, last_clause: &TypedClause| {
        if last_clause.guard.is_none() {
            match &last_clause.pattern {
                Pattern::Var { .. } | Pattern::Discard { .. } => last_clause.clone().then,
                _ => {
                    let tipo = last_clause.then.tipo();

                    TypedExpr::Trace {
                        location: Span::empty(),
                        tipo: tipo.clone(),
                        text: Box::new(TypedExpr::String {
                            location: Span::empty(),
                            tipo: crate::builtins::string(),
                            value: format!("Clause hole found for {index} elements."),
                        }),
                        then: Box::new(TypedExpr::ErrorTerm {
                            location: Span::empty(),
                            tipo,
                        }),
                    }
                }
            }
        } else {
            let tipo = last_clause.then.tipo();

            TypedExpr::Trace {
                location: Span::empty(),
                tipo: tipo.clone(),
                text: Box::new(TypedExpr::String {
                    location: Span::empty(),
                    tipo: crate::builtins::string(),
                    value: format!("Clause hole found for {index} elements."),
                }),
                then: Box::new(TypedExpr::ErrorTerm {
                    location: Span::empty(),
                    tipo,
                }),
            }
        }
    };

    let last_clause = &sorted_clauses[sorted_clauses.len() - 1];
    let assign_plug_in_name = if let Pattern::Var { name, .. } = &last_clause.pattern {
        Some(name)
    } else {
        None
    };

    for (index, clause) in sorted_clauses.iter().enumerate() {
        if last_clause_set {
            continue;
        }

        let mut clause_pattern = &clause.pattern;

        if let Pattern::Assign { pattern, .. } = clause_pattern {
            clause_pattern = pattern;
        }

        assert!(matches!(
            clause_pattern,
            Pattern::List { .. } | Pattern::Var { .. } | Pattern::Discard { .. }
        ));

        if let Pattern::List { elements, tail, .. } = clause_pattern {
            // found a hole and now we plug it
            while wild_card_clause_elems < elements.len() {
                let mut discard_elems = vec![];

                for _ in 0..wild_card_clause_elems {
                    discard_elems.push(Pattern::Discard {
                        name: "__fill".to_string(),
                        location: Span::empty(),
                    });
                }

                // If we have a named catch all then in scope the name and create list of discards, otherwise list of discards
                let clause_to_fill = if let Some(name) = assign_plug_in_name {
                    TypedClause {
                        location: Span::empty(),
                        pattern: Pattern::Assign {
                            name: name.clone(),
                            location: Span::empty(),
                            pattern: Pattern::List {
                                location: Span::empty(),
                                elements: discard_elems,
                                tail: None,
                            }
                            .into(),
                        },
                        guard: None,
                        then: plug_in_then(wild_card_clause_elems, last_clause),
                    }
                } else {
                    TypedClause {
                        location: Span::empty(),
                        pattern: Pattern::List {
                            location: Span::empty(),
                            elements: discard_elems,
                            tail: None,
                        },
                        guard: None,
                        then: plug_in_then(wild_card_clause_elems, last_clause),
                    }
                };

                holes_to_fill.push((index, clause_to_fill));
                wild_card_clause_elems += 1;
            }

            let mut is_wild_card_elems_clause = clause.guard.is_none();

            for element in elements.iter() {
                is_wild_card_elems_clause =
                    is_wild_card_elems_clause && !pattern_has_conditions(element, data_types);
            }

            if is_wild_card_elems_clause {
                if wild_card_clause_elems < elements.len() + usize::from(tail.is_none()) {
                    wild_card_clause_elems += 1;
                }

                if clause.guard.is_none() && tail.is_some() && !elements.is_empty() {
                    last_clause_index = index;
                    last_clause_set = true;
                }
            }
        } else if let Pattern::Var { .. } | Pattern::Discard { .. } = &clause.pattern {
            if clause.guard.is_none() {
                last_clause_set = true;
                last_clause_index = index;
            }
        } else {
            unreachable!("Found a clause that is not a list or var or discard");
        }

        // If the last condition doesn't have a catch all or tail then add a catch all with a todo
        if index == sorted_clauses.len() - 1 {
            if let Pattern::List { tail: None, .. } = &clause.pattern {
                final_clauses.push(TypedClause {
                    location: Span::empty(),
                    pattern: Pattern::Discard {
                        name: "_".to_string(),
                        location: Span::empty(),
                    },
                    guard: None,
                    then: plug_in_then(index + 1, last_clause),
                });
            }
        }
    }

    // Encountered a tail so stop there with that as last clause
    if last_clause_set {
        for _ in 0..(sorted_clauses.len() - 1 - last_clause_index) {
            final_clauses.pop();
        }
    }

    // insert hole fillers into clauses
    for (index, clause) in holes_to_fill.into_iter().rev() {
        final_clauses.insert(index, clause);
    }
    assert!(final_clauses.len() > 1);

    final_clauses
}

pub fn find_list_clause_or_default_first(clauses: &[TypedClause]) -> &TypedClause {
    assert!(!clauses.is_empty());

    clauses
        .iter()
        .find(|clause| match &clause.pattern {
            Pattern::List { .. } => true,
            Pattern::Assign { pattern, .. } if matches!(&**pattern, Pattern::List { .. }) => true,
            _ => false,
        })
        .unwrap_or(&clauses[0])
}

pub fn convert_data_to_type(term: Term<Name>, field_type: &Rc<Type>) -> Term<Name> {
    if field_type.is_int() {
        Term::un_i_data().apply(term)
    } else if field_type.is_bytearray() {
        Term::un_b_data().apply(term)
    } else if field_type.is_void() {
        Term::equals_integer()
            .apply(Term::integer(0.into()))
            .apply(Term::fst_pair().apply(Term::unconstr_data().apply(term)))
            .delayed_if_else(Term::unit(), Term::Error)
    } else if field_type.is_map() {
        Term::unmap_data().apply(term)
    } else if field_type.is_string() {
        Term::Builtin(DefaultFunction::DecodeUtf8).apply(Term::un_b_data().apply(term))
    } else if field_type.is_tuple() && matches!(field_type.get_uplc_type(), UplcType::Pair(_, _)) {
        Term::mk_pair_data()
            .apply(Term::head_list().apply(Term::var("__list_data")))
            .apply(Term::head_list().apply(Term::var("__tail")))
            .lambda("__tail")
            .apply(Term::tail_list().apply(Term::var("__list_data")))
            .lambda("__list_data")
            .apply(Term::unlist_data().apply(term))
    } else if field_type.is_list() || field_type.is_tuple() {
        Term::unlist_data().apply(term)
    } else if field_type.is_bool() {
        Term::equals_integer()
            .apply(Term::integer(1.into()))
            .apply(Term::fst_pair().apply(Term::unconstr_data().apply(term)))
    } else {
        term
    }
}

pub fn convert_constants_to_data(constants: Vec<Rc<UplcConstant>>) -> Vec<UplcConstant> {
    let mut new_constants = vec![];
    for constant in constants {
        let constant = match constant.as_ref() {
            UplcConstant::Integer(i) => UplcConstant::Data(PlutusData::BigInt(to_pallas_bigint(i))),
            UplcConstant::ByteString(b) => {
                UplcConstant::Data(PlutusData::BoundedBytes(b.clone().try_into().unwrap()))
            }
            UplcConstant::String(s) => UplcConstant::Data(PlutusData::BoundedBytes(
                s.as_bytes().to_vec().try_into().unwrap(),
            )),

            UplcConstant::Bool(b) => UplcConstant::Data(PlutusData::Constr(Constr {
                tag: convert_constr_to_tag((*b).into()).unwrap_or(ANY_TAG),
                any_constructor: convert_constr_to_tag((*b).into())
                    .map_or(Some((*b).into()), |_| None),
                fields: vec![],
            })),
            UplcConstant::ProtoList(list_type, constants) => {
                if matches!(list_type, UplcType::Pair(_, _)) {
                    let inner_constants = constants
                        .iter()
                        .cloned()
                        .map(|pair| match pair {
                            UplcConstant::ProtoPair(_, _, left, right) => {
                                let inner_constants = vec![left, right];
                                let inner_constants = convert_constants_to_data(inner_constants)
                                    .into_iter()
                                    .map(|constant| match constant {
                                        UplcConstant::Data(d) => d,
                                        _ => todo!(),
                                    })
                                    .collect_vec();
                                (inner_constants[0].clone(), inner_constants[1].clone())
                            }
                            _ => unreachable!(),
                        })
                        .collect_vec();

                    UplcConstant::Data(PlutusData::Map(KeyValuePairs::Def(inner_constants)))
                } else {
                    let inner_constants =
                        convert_constants_to_data(constants.iter().cloned().map(Rc::new).collect())
                            .into_iter()
                            .map(|constant| match constant {
                                UplcConstant::Data(d) => d,
                                _ => todo!(),
                            })
                            .collect_vec();

                    UplcConstant::Data(PlutusData::Array(inner_constants))
                }
            }
            UplcConstant::ProtoPair(_, _, left, right) => {
                let inner_constants = vec![left.clone(), right.clone()];
                let inner_constants = convert_constants_to_data(inner_constants)
                    .into_iter()
                    .map(|constant| match constant {
                        UplcConstant::Data(d) => d,
                        _ => todo!(),
                    })
                    .collect_vec();

                UplcConstant::Data(PlutusData::Array(vec![
                    inner_constants[0].clone(),
                    inner_constants[1].clone(),
                ]))
            }
            d @ UplcConstant::Data(_) => d.clone(),
            UplcConstant::Unit => UplcConstant::Data(PlutusData::Constr(Constr {
                tag: convert_constr_to_tag(0).unwrap(),
                any_constructor: None,
                fields: vec![],
            })),
        };
        new_constants.push(constant);
    }
    new_constants
}

pub fn convert_type_to_data(term: Term<Name>, field_type: &Rc<Type>) -> Term<Name> {
    if field_type.is_bytearray() {
        Term::b_data().apply(term)
    } else if field_type.is_int() {
        Term::i_data().apply(term)
    } else if field_type.is_void() {
        term.choose_unit(Term::Constant(
            UplcConstant::Data(PlutusData::Constr(Constr {
                tag: convert_constr_to_tag(0).unwrap(),
                any_constructor: None,
                fields: vec![],
            }))
            .into(),
        ))
    } else if field_type.is_map() {
        Term::map_data().apply(term)
    } else if field_type.is_string() {
        Term::b_data().apply(Term::Builtin(DefaultFunction::EncodeUtf8).apply(term))
    } else if field_type.is_tuple() && matches!(field_type.get_uplc_type(), UplcType::Pair(_, _)) {
        Term::list_data()
            .apply(
                Term::mk_cons()
                    .apply(Term::fst_pair().apply(Term::var("__pair")))
                    .apply(
                        Term::mk_cons()
                            .apply(Term::snd_pair().apply(Term::var("__pair")))
                            .apply(Term::empty_list()),
                    ),
            )
            .lambda("__pair")
            .apply(term)
    } else if field_type.is_list() || field_type.is_tuple() {
        Term::list_data().apply(term)
    } else if field_type.is_bool() {
        term.if_else(
            Term::Constant(
                UplcConstant::Data(PlutusData::Constr(Constr {
                    tag: convert_constr_to_tag(1).unwrap(),
                    any_constructor: None,
                    fields: vec![],
                }))
                .into(),
            ),
            Term::Constant(
                UplcConstant::Data(PlutusData::Constr(Constr {
                    tag: convert_constr_to_tag(0).unwrap(),
                    any_constructor: None,
                    fields: vec![],
                }))
                .into(),
            ),
        )
    } else {
        term
    }
}

#[allow(clippy::too_many_arguments)]
pub fn list_access_to_uplc(
    names: &[String],
    id_list: &[u64],
    tail: bool,
    current_index: usize,
    term: Term<Name>,
    tipos: Vec<Rc<Type>>,
    check_last_item: bool,
    is_list_accessor: bool,
    tracing: bool,
) -> Term<Name> {
    let trace_term = if tracing {
        Term::Error.trace(Term::string(
            "List/Tuple/Constr contains more items than expected",
        ))
    } else {
        Term::Error
    };

    if let Some((first, names)) = names.split_first() {
        let (current_tipo, tipos) = tipos.split_first().unwrap();

        let head_list =
            if matches!(current_tipo.get_uplc_type(), UplcType::Pair(_, _)) && is_list_accessor {
                Term::head_list().apply(Term::var(format!(
                    "tail_index_{}_{}",
                    current_index, id_list[current_index]
                )))
            } else {
                convert_data_to_type(
                    Term::head_list().apply(Term::var(format!(
                        "tail_index_{}_{}",
                        current_index, id_list[current_index]
                    ))),
                    &current_tipo.to_owned(),
                )
            };

        if names.len() == 1 && tail {
            if first == "_" && names[0] == "_" {
                term.lambda("_")
            } else if first == "_" {
                term.lambda(names[0].clone())
                    .apply(Term::tail_list().apply(Term::var(format!(
                        "tail_index_{}_{}",
                        current_index, id_list[current_index]
                    ))))
                    .lambda(format!(
                        "tail_index_{}_{}",
                        current_index, id_list[current_index]
                    ))
            } else if names[0] == "_" {
                term.lambda(first.clone()).apply(head_list).lambda(format!(
                    "tail_index_{}_{}",
                    current_index, id_list[current_index]
                ))
            } else {
                term.lambda(names[0].clone())
                    .apply(Term::tail_list().apply(Term::var(format!(
                        "tail_index_{}_{}",
                        current_index, id_list[current_index]
                    ))))
                    .lambda(first.clone())
                    .apply(head_list)
                    .lambda(format!(
                        "tail_index_{}_{}",
                        current_index, id_list[current_index]
                    ))
            }
        } else if names.is_empty() {
            if first == "_" {
                if check_last_item {
                    Term::tail_list()
                        .apply(Term::var(format!(
                            "tail_index_{}_{}",
                            current_index, id_list[current_index]
                        )))
                        .delayed_choose_list(term, trace_term)
                } else {
                    term
                }
                .lambda(if check_last_item {
                    format!("tail_index_{}_{}", current_index, id_list[current_index])
                } else {
                    "_".to_string()
                })
            } else {
                if check_last_item {
                    Term::tail_list()
                        .apply(Term::var(format!(
                            "tail_index_{}_{}",
                            current_index, id_list[current_index]
                        )))
                        .delayed_choose_list(term, trace_term)
                } else {
                    term
                }
                .lambda(first.clone())
                .apply(head_list)
                .lambda(format!(
                    "tail_index_{}_{}",
                    current_index, id_list[current_index]
                ))
            }
        } else if first == "_" {
            let mut list_access_inner = list_access_to_uplc(
                names,
                id_list,
                tail,
                current_index + 1,
                term,
                tipos.to_owned(),
                check_last_item,
                is_list_accessor,
                tracing,
            );

            list_access_inner = match &list_access_inner {
                Term::Lambda {
                    parameter_name,
                    body,
                } => {
                    if &parameter_name.text == "_" {
                        body.as_ref().clone()
                    } else {
                        list_access_inner
                            .apply(Term::tail_list().apply(Term::var(format!(
                                "tail_index_{}_{}",
                                current_index, id_list[current_index]
                            ))))
                            .lambda(format!(
                                "tail_index_{}_{}",
                                current_index, id_list[current_index]
                            ))
                    }
                }
                _ => list_access_inner,
            };

            match &list_access_inner {
                Term::Lambda { .. } => list_access_inner,
                _ => list_access_inner.lambda("_"),
            }
        } else {
            let mut list_access_inner = list_access_to_uplc(
                names,
                id_list,
                tail,
                current_index + 1,
                term,
                tipos.to_owned(),
                check_last_item,
                is_list_accessor,
                tracing,
            );

            list_access_inner = match &list_access_inner {
                Term::Lambda {
                    parameter_name,
                    body,
                } => {
                    if &parameter_name.text == "_" {
                        body.as_ref()
                            .clone()
                            .lambda(first.clone())
                            .apply(head_list)
                            .lambda(format!(
                                "tail_index_{}_{}",
                                current_index, id_list[current_index]
                            ))
                    } else {
                        list_access_inner
                            .apply(Term::tail_list().apply(Term::var(format!(
                                "tail_index_{}_{}",
                                current_index, id_list[current_index]
                            ))))
                            .lambda(first.clone())
                            .apply(head_list)
                            .lambda(format!(
                                "tail_index_{}_{}",
                                current_index, id_list[current_index]
                            ))
                    }
                }
                _ => list_access_inner
                    .lambda(first.clone())
                    .apply(head_list)
                    .lambda(format!(
                        "tail_index_{}_{}",
                        current_index, id_list[current_index]
                    )),
            };
            list_access_inner
        }
    } else {
        term
    }
}

pub fn apply_builtin_forces(mut term: Term<Name>, force_count: u32) -> Term<Name> {
    for _ in 0..force_count {
        term = term.force();
    }
    term
}

pub fn undata_builtin(
    func: &DefaultFunction,
    count: usize,
    tipo: &Rc<Type>,
    args: Vec<Term<Name>>,
) -> Term<Name> {
    let mut term: Term<Name> = (*func).into();

    term = apply_builtin_forces(term, func.force_count());

    for arg in args {
        term = term.apply(arg);
    }

    let temp_var = "__item_x";

    if count == 0 {
        term = term.apply(Term::var(temp_var));
    }

    term = convert_data_to_type(term, tipo);

    if count == 0 {
        term = term.lambda(temp_var);
    }
    term
}

pub fn to_data_builtin(
    func: &DefaultFunction,
    count: usize,
    tipo: &Rc<Type>,
    mut args: Vec<Term<Name>>,
) -> Term<Name> {
    let mut term: Term<Name> = (*func).into();

    term = apply_builtin_forces(term, func.force_count());

    if count == 0 {
        assert!(args.is_empty());

        for arg_index in 0..func.arity() {
            let temp_var = format!("__item_index_{}", arg_index);

            args.push(Term::var(temp_var))
        }
    }

    for (index, arg) in args.into_iter().enumerate() {
        if index == 0 || matches!(func, DefaultFunction::MkPairData) {
            term = term.apply(convert_type_to_data(arg, tipo));
        } else {
            term = term.apply(arg);
        }
    }

    if count == 0 {
        for arg_index in (0..func.arity()).rev() {
            let temp_var = format!("__item_index_{}", arg_index);
            term = term.lambda(temp_var);
        }
    }

    term
}

pub fn special_case_builtin(
    func: &DefaultFunction,
    count: usize,
    mut args: Vec<Term<Name>>,
) -> Term<Name> {
    match func {
        DefaultFunction::IfThenElse
        | DefaultFunction::ChooseList
        | DefaultFunction::ChooseData
        | DefaultFunction::Trace => {
            let mut term: Term<Name> = (*func).into();

            term = apply_builtin_forces(term, func.force_count());

            if count == 0 {
                assert!(args.is_empty());

                for arg_index in 0..func.arity() {
                    let temp_var = format!("__item_index_{}", arg_index);

                    args.push(Term::var(temp_var))
                }
            }

            for (index, arg) in args.into_iter().enumerate() {
                if index == 0 {
                    term = term.apply(arg);
                } else {
                    term = term.apply(arg.delay());
                }
            }

            term = term.force();

            if count == 0 {
                for arg_index in (0..func.arity()).rev() {
                    let temp_var = format!("__item_index_{}", arg_index);
                    term = term.lambda(temp_var);
                }
            }

            term
        }
        DefaultFunction::ChooseUnit => {
            if count == 0 {
                unimplemented!("Honestly, why are you doing this?")
            } else {
                let term = args.pop().unwrap();
                let unit = args.pop().unwrap();

                term.lambda("_").apply(unit)
            }
        }
        DefaultFunction::UnConstrData => {
            let mut term: Term<Name> = (*func).into();

            let temp_tuple = "__unconstr_tuple";

            for arg in args {
                term = term.apply(arg);
            }

            let temp_var = "__item_x";

            if count == 0 {
                term = term.apply(Term::var(temp_var));
            }

            term = Term::mk_pair_data()
                .apply(Term::i_data().apply(Term::fst_pair().apply(Term::var(temp_tuple))))
                .apply(Term::list_data().apply(Term::snd_pair().apply(Term::var(temp_tuple))))
                .lambda(temp_tuple)
                .apply(term);

            if count == 0 {
                term = term.lambda(temp_var);
            }

            term
        }
        _ => unreachable!(),
    }
}

pub fn wrap_as_multi_validator(spend: Term<Name>, mint: Term<Name>) -> Term<Name> {
    Term::equals_integer()
        .apply(Term::integer(0.into()))
        .apply(Term::var(CONSTR_INDEX_EXPOSER).apply(Term::var("__second_arg")))
        .delayed_if_else(
            mint.apply(Term::var("__first_arg"))
                .apply(Term::var("__second_arg")),
            spend.apply(Term::var("__first_arg")).apply(
                Term::head_list()
                    .apply(Term::var(CONSTR_FIELDS_EXPOSER).apply(Term::var("__second_arg"))),
            ),
        )
        .lambda("__second_arg")
        .lambda("__first_arg")
}

/// If the pattern is a list the return the number of elements and if it has a tail
/// Otherwise return None
pub fn get_list_elements_len_and_tail(
    pattern: &Pattern<PatternConstructor, Rc<Type>>,
) -> Option<(usize, bool)> {
    if let Pattern::List { elements, tail, .. } = &pattern {
        Some((elements.len(), tail.is_some()))
    } else if let Pattern::Assign { pattern, .. } = &pattern {
        if let Pattern::List { elements, tail, .. } = pattern.as_ref() {
            Some((elements.len(), tail.is_some()))
        } else {
            None
        }
    } else {
        None
    }
}

pub fn cast_validator_args(term: Term<Name>, arguments: &[TypedArg]) -> Term<Name> {
    let mut term = term;
    for arg in arguments.iter().rev() {
        if !matches!(arg.tipo.get_uplc_type(), UplcType::Data) {
            term = term
                .lambda(arg.arg_name.get_variable_name().unwrap_or("_"))
                .apply(convert_data_to_type(
                    Term::var(arg.arg_name.get_variable_name().unwrap_or("_")),
                    &arg.tipo,
                ));
        }

        term = term.lambda(arg.arg_name.get_variable_name().unwrap_or("_"))
    }
    term
}

pub fn wrap_validator_condition(air_tree: AirTree) -> AirTree {
    let success_branch = vec![(air_tree, AirTree::void())];
    let otherwise = AirTree::error(void());

    AirTree::if_branches(success_branch, void(), otherwise)
}

pub fn extract_constant(term: &Term<Name>) -> Option<Rc<UplcConstant>> {
    let mut constant = None;

    if let Term::Constant(c) = term {
        constant = Some(c.clone());
    } else if let Term::Apply { function, argument } = term {
        if let Term::Constant(c) = argument.as_ref() {
            if let Term::Builtin(b) = function.as_ref() {
                if matches!(
                    b,
                    DefaultFunction::BData
                        | DefaultFunction::IData
                        | DefaultFunction::MapData
                        | DefaultFunction::ListData
                ) {
                    constant = Some(c.clone());
                }
            }
        }
    }
    constant
}
