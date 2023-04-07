use std::{rc::Rc, sync::Arc};

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use uplc::{
    ast::{Constant as UplcConstant, Name, Term, Type as UplcType},
    builder::{CONSTR_FIELDS_EXPOSER, CONSTR_INDEX_EXPOSER},
    builtins::DefaultFunction,
    machine::{
        runtime::{convert_constr_to_tag, ANY_TAG},
        to_pallas_bigint,
    },
    Constr, KeyValuePairs, PlutusData,
};

use crate::{
    ast::{
        AssignmentKind, BinOp, ClauseGuard, Constant, DataType, Pattern, Span, TypedArg,
        TypedClause, TypedDataType, UnOp,
    },
    expr::TypedExpr,
    tipo::{PatternConstructor, Type, TypeVar, ValueConstructorVariant},
    IdGenerator,
};

use super::{air::Air, scope::Scope, stack::AirStack};

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
    pub variant_name: String,
}

#[derive(Clone, Debug)]
pub struct AssignmentProperties {
    pub value_type: Arc<Type>,
    pub kind: AssignmentKind,
}

#[derive(Clone, Debug)]
pub enum ClauseProperties {
    ConstrClause {
        clause_var_name: String,
        needs_constr_var: bool,
        is_complex_clause: bool,
        original_subject_name: String,
        final_clause: bool,
    },
    ListClause {
        clause_var_name: String,
        needs_constr_var: bool,
        is_complex_clause: bool,
        original_subject_name: String,
        current_index: i64,
        final_clause: bool,
    },
    TupleClause {
        clause_var_name: String,
        needs_constr_var: bool,
        is_complex_clause: bool,
        original_subject_name: String,
        defined_tuple_indices: IndexSet<(usize, String)>,
        final_clause: bool,
    },
}

impl ClauseProperties {
    pub fn init(t: &Arc<Type>, constr_var: String, subject_name: String) -> Self {
        if t.is_list() {
            ClauseProperties::ListClause {
                clause_var_name: constr_var,
                needs_constr_var: false,
                is_complex_clause: false,
                original_subject_name: subject_name,
                current_index: -1,
                final_clause: false,
            }
        } else if t.is_tuple() {
            ClauseProperties::TupleClause {
                clause_var_name: constr_var,
                needs_constr_var: false,
                is_complex_clause: false,
                original_subject_name: subject_name,
                defined_tuple_indices: IndexSet::new(),
                final_clause: false,
            }
        } else {
            ClauseProperties::ConstrClause {
                clause_var_name: constr_var,
                needs_constr_var: false,
                is_complex_clause: false,
                original_subject_name: subject_name,
                final_clause: false,
            }
        }
    }

    pub fn is_complex_clause(&mut self) -> &mut bool {
        match self {
            ClauseProperties::ConstrClause {
                is_complex_clause, ..
            }
            | ClauseProperties::ListClause {
                is_complex_clause, ..
            }
            | ClauseProperties::TupleClause {
                is_complex_clause, ..
            } => is_complex_clause,
        }
    }
    pub fn needs_constr_var(&mut self) -> &mut bool {
        match self {
            ClauseProperties::ConstrClause {
                needs_constr_var, ..
            }
            | ClauseProperties::ListClause {
                needs_constr_var, ..
            }
            | ClauseProperties::TupleClause {
                needs_constr_var, ..
            } => needs_constr_var,
        }
    }

    pub fn is_final_clause(&mut self) -> &mut bool {
        match self {
            ClauseProperties::ConstrClause { final_clause, .. }
            | ClauseProperties::ListClause { final_clause, .. }
            | ClauseProperties::TupleClause { final_clause, .. } => final_clause,
        }
    }

    pub fn clause_var_name(&mut self) -> &mut String {
        match self {
            ClauseProperties::ConstrClause {
                clause_var_name, ..
            }
            | ClauseProperties::ListClause {
                clause_var_name, ..
            }
            | ClauseProperties::TupleClause {
                clause_var_name, ..
            } => clause_var_name,
        }
    }

    pub fn original_subject_name(&mut self) -> &mut String {
        match self {
            ClauseProperties::ConstrClause {
                original_subject_name,
                ..
            }
            | ClauseProperties::ListClause {
                original_subject_name,
                ..
            }
            | ClauseProperties::TupleClause {
                original_subject_name,
                ..
            } => original_subject_name,
        }
    }
}

pub fn convert_type_to_data(term: Term<Name>, field_type: &Arc<Type>) -> Term<Name> {
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

pub fn convert_data_to_type(term: Term<Name>, field_type: &Arc<Type>) -> Term<Name> {
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

pub fn rearrange_clauses(clauses: Vec<TypedClause>) -> Vec<TypedClause> {
    let mut sorted_clauses = clauses;

    // if we have a list sort clauses so we can plug holes for cases not covered by clauses
    // TODO: while having 10000000 element list is impossible to destructure in plutus budget,
    // let's sort clauses by a safer manner
    // TODO: how shall tails be weighted? Since any clause after will not run
    sorted_clauses.sort_by(|clause1, clause2| {
        let clause1_len = match &clause1.pattern {
            Pattern::List { elements, tail, .. } => elements.len() + usize::from(tail.is_some()),
            _ => 10000000,
        };
        let clause2_len = match &clause2.pattern {
            Pattern::List { elements, tail, .. } => elements.len() + usize::from(tail.is_some()),
            _ => 10000001,
        };

        clause1_len.cmp(&clause2_len)
    });

    let mut elems_len = 0;
    let mut final_clauses = sorted_clauses.clone();
    let mut holes_to_fill = vec![];
    let mut assign_plug_in_name = None;
    let mut last_clause_index = 0;
    let mut last_clause_set = false;

    // If we have a catch all, use that. Otherwise use todo which will result in error
    // TODO: fill in todo label with description
    let plug_in_then = match &sorted_clauses[sorted_clauses.len() - 1].pattern {
        Pattern::Var { name, .. } => {
            assign_plug_in_name = Some(name);
            sorted_clauses[sorted_clauses.len() - 1].clone().then
        }
        Pattern::Discard { .. } => sorted_clauses[sorted_clauses.len() - 1].clone().then,
        _ => {
            let tipo = sorted_clauses[sorted_clauses.len() - 1].then.tipo();
            TypedExpr::Trace {
                location: Span::empty(),
                tipo: tipo.clone(),
                text: Box::new(TypedExpr::String {
                    location: Span::empty(),
                    tipo: crate::builtins::string(),
                    value: "Clause not filled".to_string(),
                }),
                then: Box::new(TypedExpr::ErrorTerm {
                    location: Span::empty(),
                    tipo,
                }),
            }
        }
    };

    for (index, clause) in sorted_clauses.iter().enumerate() {
        if let Pattern::List { elements, .. } = &clause.pattern {
            // found a hole and now we plug it
            while elems_len < elements.len() {
                let mut discard_elems = vec![];

                for _ in 0..elems_len {
                    discard_elems.push(Pattern::Discard {
                        name: "_".to_string(),
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
                        then: plug_in_then.clone(),
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
                        then: plug_in_then.clone(),
                    }
                };

                holes_to_fill.push((index, clause_to_fill));
                elems_len += 1;
            }
        }

        // if we have a pattern with no clause guards and a tail then no lists will get past here to other clauses
        match &clause.pattern {
            Pattern::Var { .. } => {
                last_clause_index = index + 1;
                last_clause_set = true;
            }
            Pattern::Discard { .. } => {
                last_clause_index = index + 1;
                last_clause_set = true;
            }
            Pattern::List {
                elements,
                tail: Some(tail),
                ..
            } => {
                let mut elements = elements.clone();
                elements.push(*tail.clone());
                if elements
                    .iter()
                    .all(|element| matches!(element, Pattern::Var { .. } | Pattern::Discard { .. }))
                    && !last_clause_set
                    && !elements.is_empty()
                {
                    last_clause_index = index + 1;
                    last_clause_set = true;
                }
            }
            _ => {}
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
                    then: plug_in_then.clone(),
                });
            }
        }

        elems_len += 1;
    }

    // Encountered a tail so stop there with that as last clause
    if last_clause_set {
        final_clauses = final_clauses[0..last_clause_index].to_vec();
    }

    // insert hole fillers into clauses
    for (index, clause) in holes_to_fill.into_iter().rev() {
        final_clauses.insert(index, clause);
    }

    final_clauses
}

#[allow(clippy::too_many_arguments)]
pub fn list_access_to_uplc(
    names: &[String],
    id_list: &[u64],
    tail: bool,
    current_index: usize,
    term: Term<Name>,
    tipos: Vec<Arc<Type>>,
    check_last_item: bool,
    is_list_accessor: bool,
) -> Term<Name> {
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
                        .delayed_choose_list(
                            term,
                            Term::Error.trace(Term::string(
                                "List/Tuple/Constr contains more items than expected",
                            )),
                        )
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
                        .delayed_choose_list(
                            term,
                            Term::Error.trace(Term::string(
                                "List/Tuple/Constr contains more items than expected",
                            )),
                        )
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

pub fn check_when_pattern_needs(
    pattern: &Pattern<PatternConstructor, Arc<Type>>,
    clause_properties: &mut ClauseProperties,
) {
    match pattern {
        Pattern::Var { .. } => {
            *clause_properties.needs_constr_var() = true;
        }
        Pattern::List { elements, tail, .. } => {
            *clause_properties.needs_constr_var() = true;

            *clause_properties.is_complex_clause() = true;

            for element in elements {
                check_when_pattern_needs(element, clause_properties);
            }
            if let Some(tail) = tail {
                check_when_pattern_needs(tail, clause_properties);
            }
        }
        Pattern::Tuple { elems, .. } => {
            *clause_properties.needs_constr_var() = true;

            *clause_properties.is_complex_clause() = true;

            for element in elems {
                check_when_pattern_needs(element, clause_properties);
            }
        }
        Pattern::Int { .. } => {
            *clause_properties.needs_constr_var() = true;

            *clause_properties.is_complex_clause() = true;
        }
        Pattern::Constructor { arguments, .. } => {
            *clause_properties.needs_constr_var() = true;

            *clause_properties.is_complex_clause() = true;

            for argument in arguments {
                check_when_pattern_needs(&argument.value, clause_properties);
            }
        }
        Pattern::Discard { .. } => {
            *clause_properties.needs_constr_var() = true;
        }
        Pattern::Assign { pattern, .. } => {
            *clause_properties.needs_constr_var() = true;

            check_when_pattern_needs(pattern, clause_properties)
        }
    }
}

pub fn constants_ir(literal: &Constant, ir_stack: &mut AirStack) {
    match literal {
        Constant::Int { value, .. } => {
            ir_stack.integer(value.clone());
        }
        Constant::String { value, .. } => {
            ir_stack.string(value.clone());
        }
        Constant::ByteArray { bytes, .. } => {
            ir_stack.byte_array(bytes.clone());
        }
    };
}

pub fn match_ir_for_recursion(
    ir: Air,
    insert_var_vec: &mut Vec<(usize, Air)>,
    function_access_key: &FunctionAccessKey,
    index: usize,
) {
    if let Air::Var {
        scope,
        constructor,
        variant_name,
        ..
    } = ir
    {
        if let ValueConstructorVariant::ModuleFn {
            name: func_name,
            module,
            ..
        } = constructor.clone().variant
        {
            let var_func_access = FunctionAccessKey {
                module_name: module,
                function_name: func_name.clone(),
                variant_name: variant_name.clone(),
            };

            if function_access_key.clone() == var_func_access {
                insert_var_vec.push((
                    index,
                    Air::Var {
                        scope,
                        constructor,
                        name: func_name,
                        variant_name,
                    },
                ));
            }
        }
    }
}

pub fn find_and_replace_generics(tipo: &mut Arc<Type>, mono_types: &IndexMap<u64, Arc<Type>>) {
    if let Some(id) = tipo.get_generic() {
        // If a generic does not have a type we know of
        // like a None in option then just use same type
        *tipo = mono_types.get(&id).unwrap_or(tipo).clone();
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
                    let mut arg = arg.clone();
                    find_and_replace_generics(&mut arg, mono_types);
                    new_args.push(arg);
                }
                let t = Type::App {
                    args: new_args,
                    public: *public,
                    module: module.clone(),
                    name: name.clone(),
                };
                *tipo = t.into();
            }
            Type::Fn { args, ret } => {
                let mut new_args = vec![];
                for arg in args {
                    let mut arg = arg.clone();
                    find_and_replace_generics(&mut arg, mono_types);
                    new_args.push(arg);
                }

                let mut ret = ret.clone();
                find_and_replace_generics(&mut ret, mono_types);

                let t = Type::Fn {
                    args: new_args,
                    ret,
                };
                *tipo = t.into();
            }
            Type::Tuple { elems } => {
                let mut new_elems = vec![];
                for elem in elems {
                    let mut elem = elem.clone();
                    find_and_replace_generics(&mut elem, mono_types);
                    new_elems.push(elem);
                }
                let t = Type::Tuple { elems: new_elems };
                *tipo = t.into();
            }
            Type::Var { tipo: var_tipo } => {
                let var_type = var_tipo.as_ref().borrow().clone();
                let var_tipo = match var_type {
                    TypeVar::Link { tipo } => {
                        let mut tipo = tipo;
                        find_and_replace_generics(&mut tipo, mono_types);
                        tipo
                    }
                    TypeVar::Generic { .. } | TypeVar::Unbound { .. } => unreachable!(),
                };

                *tipo = var_tipo;
            }
        };
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

pub fn get_variant_name(new_name: &mut String, t: &Arc<Type>) {
    new_name.push_str(&if t.is_string() {
        "_string".to_string()
    } else if t.is_int() {
        "_int".to_string()
    } else if t.is_bool() {
        "_bool".to_string()
    } else if t.is_bytearray() {
        "_bytearray".to_string()
    } else if t.is_map() {
        let mut full_type = "_map".to_string();
        let pair_type = &t.get_inner_types()[0];
        let fst_type = &pair_type.get_inner_types()[0];
        let snd_type = &pair_type.get_inner_types()[1];

        get_variant_name(&mut full_type, fst_type);
        get_variant_name(&mut full_type, snd_type);
        full_type
    } else if t.is_list() {
        let mut full_type = "_list".to_string();
        let list_type = &t.get_inner_types()[0];
        get_variant_name(&mut full_type, list_type);
        full_type
    } else if t.is_tuple() {
        let mut full_type = "_tuple".to_string();

        let inner_types = t.get_inner_types();

        for arg_type in inner_types {
            get_variant_name(&mut full_type, &arg_type);
        }
        full_type
    } else if t.is_unbound() {
        "_unbound".to_string()
    } else {
        let mut full_type = "_data".to_string();

        if t.is_generic() {
            panic!("FOUND A POLYMORPHIC TYPE. EXPECTED MONOMORPHIC TYPE");
        }

        let inner_types = t.get_inner_types();

        for arg_type in inner_types {
            get_variant_name(&mut full_type, &arg_type);
        }
        full_type
    });
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
            UplcConstant::ProtoList(_, constants) => {
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
            UplcConstant::ProtoPair(_, _, left, right) => {
                let inner_constants = vec![left.clone(), right.clone()];
                let inner_constants = convert_constants_to_data(inner_constants)
                    .into_iter()
                    .map(|constant| match constant {
                        UplcConstant::Data(d) => d,
                        _ => todo!(),
                    })
                    .collect_vec();

                UplcConstant::Data(PlutusData::Map(KeyValuePairs::Def(vec![(
                    inner_constants[0].clone(),
                    inner_constants[1].clone(),
                )])))
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

pub fn wrap_validator_args(term: Term<Name>, arguments: &[TypedArg]) -> Term<Name> {
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

pub fn monomorphize(
    ir: Vec<Air>,
    mono_types: IndexMap<u64, Arc<Type>>,
    full_type: &Arc<Type>,
) -> (String, Vec<Air>) {
    let mut new_air = ir.clone();
    let mut new_name = String::new();
    let mut needs_variant = false;

    for (index, ir) in ir.into_iter().enumerate() {
        match ir {
            Air::Var {
                constructor,
                scope,
                name,
                ..
            } => {
                if constructor.tipo.is_generic() {
                    let mut tipo = constructor.tipo.clone();

                    find_and_replace_generics(&mut tipo, &mono_types);

                    let mut variant = String::new();

                    let mut constructor = constructor.clone();
                    constructor.tipo = tipo;

                    if let Type::Fn { args, .. } = &*constructor.tipo {
                        if matches!(
                            constructor.variant,
                            ValueConstructorVariant::ModuleFn { .. }
                        ) {
                            for arg in args {
                                get_variant_name(&mut variant, arg);
                            }
                        }
                    }
                    new_air[index] = Air::Var {
                        scope,
                        constructor,
                        name,
                        variant_name: variant,
                    };
                    needs_variant = true;
                }
            }
            Air::List {
                tipo,
                scope,
                count,
                tail,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::List {
                        scope,
                        count,
                        tipo,
                        tail,
                    };
                    needs_variant = true;
                }
            }
            Air::ListAccessor {
                scope,
                tipo,
                names,
                tail,
                check_last_item,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::ListAccessor {
                        scope,
                        names,
                        tipo,
                        tail,
                        check_last_item,
                    };
                    needs_variant = true;
                }
            }
            Air::ListExpose {
                scope,
                tipo,
                tail_head_names,
                tail,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::ListExpose {
                        scope,
                        tail_head_names,
                        tipo,
                        tail,
                    };
                    needs_variant = true;
                }
            }
            Air::BinOp { scope, name, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::BinOp { scope, name, tipo };
                    needs_variant = true;
                }
            }
            Air::Builtin {
                scope,
                func,
                tipo,
                count,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::Builtin {
                        scope,
                        func,
                        tipo,
                        count,
                    };
                    needs_variant = true;
                }
            }
            Air::UnWrapData { scope, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::UnWrapData { scope, tipo };
                    needs_variant = true;
                }
            }
            Air::WrapData { scope, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::WrapData { scope, tipo };
                    needs_variant = true;
                }
            }
            Air::When {
                scope,
                tipo,
                subject_name,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::When {
                        scope,
                        subject_name,
                        tipo,
                    };
                    needs_variant = true;
                }
            }
            Air::Clause {
                scope,
                tipo,
                subject_name,
                complex_clause,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::Clause {
                        scope,
                        tipo,
                        subject_name,
                        complex_clause,
                    };
                    needs_variant = true;
                }
            }
            Air::ListClause {
                scope,
                tipo,
                tail_name,
                complex_clause,
                next_tail_name,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::ListClause {
                        scope,
                        tipo,
                        tail_name,
                        complex_clause,
                        next_tail_name,
                    };
                    needs_variant = true;
                }
            }
            Air::TupleClause {
                scope,
                tipo,
                indices,
                predefined_indices,
                subject_name,
                count,
                complex_clause,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::TupleClause {
                        scope,
                        tipo,
                        indices,
                        predefined_indices,
                        subject_name,
                        count,
                        complex_clause,
                    };
                    needs_variant = true;
                }
            }
            Air::ClauseGuard {
                tipo,
                scope,
                subject_name,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::ClauseGuard {
                        scope,
                        subject_name,
                        tipo,
                    };
                    needs_variant = true;
                }
            }
            Air::ListClauseGuard {
                scope,
                tipo,
                tail_name,
                next_tail_name,
                inverse,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::ListClauseGuard {
                        scope,
                        tipo,
                        tail_name,
                        next_tail_name,
                        inverse,
                    };
                    needs_variant = true;
                }
            }
            Air::Tuple { scope, tipo, count } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::Tuple { scope, tipo, count };
                    needs_variant = true;
                }
            }
            Air::TupleIndex {
                scope,
                tipo,
                tuple_index,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::TupleIndex {
                        scope,
                        tipo,
                        tuple_index,
                    };
                    needs_variant = true;
                }
            }
            Air::ErrorTerm { scope, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::ErrorTerm { scope, tipo };
                    needs_variant = true;
                }
            }
            Air::Trace { scope, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::Trace { scope, tipo };
                    needs_variant = true;
                }
            }
            Air::Record {
                scope,
                tag: constr_index,
                tipo,
                count,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::Record {
                        scope,
                        tipo,
                        tag: constr_index,
                        count,
                    };
                    needs_variant = true;
                }
            }
            Air::RecordAccess {
                scope,
                record_index,
                tipo,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::RecordAccess {
                        scope,
                        record_index,
                        tipo,
                    };
                    needs_variant = true;
                }
            }
            Air::FieldsExpose {
                scope,
                indices,
                check_last_item,
            } => {
                let mut new_indices = vec![];
                for (ind, name, tipo) in indices {
                    if tipo.is_generic() {
                        let mut tipo = tipo.clone();
                        find_and_replace_generics(&mut tipo, &mono_types);
                        needs_variant = true;
                        new_indices.push((ind, name, tipo));
                    } else {
                        new_indices.push((ind, name, tipo));
                    }
                }
                new_air[index] = Air::FieldsExpose {
                    scope,
                    indices: new_indices,
                    check_last_item,
                };
            }
            Air::RecordUpdate {
                scope,
                highest_index,
                indices,
                tipo,
            } => {
                let mut new_indices = vec![];
                let mut tipo = tipo.clone();
                for (ind, tipo) in indices {
                    if tipo.is_generic() {
                        let mut tipo = tipo.clone();
                        find_and_replace_generics(&mut tipo, &mono_types);
                        needs_variant = true;
                        new_indices.push((ind, tipo));
                    } else {
                        new_indices.push((ind, tipo));
                    }
                }
                if tipo.is_generic() {
                    find_and_replace_generics(&mut tipo, &mono_types);
                }
                new_air[index] = Air::RecordUpdate {
                    scope,
                    highest_index,
                    indices: new_indices,
                    tipo,
                };
            }
            Air::TupleAccessor {
                scope,
                names,
                tipo,
                check_last_item,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::TupleAccessor {
                        scope,
                        names,
                        tipo,
                        check_last_item,
                    };
                    needs_variant = true;
                }
            }

            Air::Call { scope, count, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::Call { scope, count, tipo };
                    needs_variant = true;
                }
            }
            Air::If { scope, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_and_replace_generics(&mut tipo, &mono_types);

                    new_air[index] = Air::If { scope, tipo };
                    needs_variant = true;
                }
            }
            _ => {}
        }
    }

    if let Type::Fn { args, .. } = &**full_type {
        if needs_variant {
            for arg in args {
                get_variant_name(&mut new_name, arg);
            }
        }
    }

    (new_name, new_air)
}

#[allow(clippy::too_many_arguments)]
pub fn handle_func_dependencies(
    dependencies_ir: &mut Vec<Air>,
    function_component: &FuncComponents,
    func_components: &IndexMap<FunctionAccessKey, FuncComponents>,
    defined_functions: &mut IndexMap<FunctionAccessKey, ()>,
    func_index_map: &IndexMap<FunctionAccessKey, Scope>,
    func_scope: &Scope,
    to_be_defined: &mut IndexMap<FunctionAccessKey, ()>,
    id_gen: Rc<IdGenerator>,
) {
    let mut function_component = function_component.clone();

    let mut dependency_map = IndexMap::new();
    let mut dependency_vec = vec![];

    // deal with function dependencies by sorting order in which we pop them.
    while let Some(dependency) = function_component.dependencies.pop() {
        let depend_comp = func_components.get(&dependency).unwrap();

        if dependency_map.contains_key(&dependency) {
            dependency_map.shift_remove(&dependency);
        }

        dependency_map.insert(dependency, ());

        function_component
            .dependencies
            .extend(depend_comp.dependencies.clone().into_iter());
    }

    dependency_vec.extend(dependency_map.keys().cloned());
    dependency_vec.reverse();

    while let Some(dependency) = dependency_vec.pop() {
        let func_component_dep = func_components.get(&dependency);

        if defined_functions.contains_key(&dependency) {
            continue;
        }

        let Some(depend_comp) = func_component_dep else {continue};

        let dep_scope = func_index_map.get(&dependency).unwrap();

        if (dep_scope.common_ancestor(func_scope) == *func_scope && !depend_comp.args.is_empty())
            || function_component.args.is_empty()
        {
            let mut recursion_ir = vec![];
            handle_recursion_ir(&dependency, depend_comp, &mut recursion_ir);

            let mut temp_stack = AirStack {
                id_gen: id_gen.clone(),
                scope: func_scope.clone(),
                air: vec![],
            };

            let recursion_stack = AirStack {
                id_gen: id_gen.clone(),
                scope: func_scope.clone(),
                air: recursion_ir,
            };

            if depend_comp.is_code_gen_func {
                temp_stack = recursion_stack;
            } else {
                temp_stack.define_func(
                    dependency.function_name.clone(),
                    dependency.module_name.clone(),
                    dependency.variant_name.clone(),
                    depend_comp.args.clone(),
                    depend_comp.recursive,
                    recursion_stack,
                );
            }

            let mut temp_ir = temp_stack.complete();

            temp_ir.append(dependencies_ir);

            *dependencies_ir = temp_ir;

            if dep_scope.common_ancestor(func_scope) == *func_scope {
                defined_functions.insert(dependency, ());
            }
        } else if depend_comp.args.is_empty() {
            to_be_defined.insert(dependency, ());
        }
    }
}

pub fn handle_recursion_ir(
    func_key: &FunctionAccessKey,
    func_comp: &FuncComponents,
    recursion_ir: &mut Vec<Air>,
) {
    let mut insert_var_vec = vec![];

    for (index, ir) in func_comp.ir.iter().enumerate().rev() {
        match_ir_for_recursion(
            ir.clone(),
            &mut insert_var_vec,
            &FunctionAccessKey {
                function_name: func_key.function_name.clone(),
                module_name: func_key.module_name.clone(),
                variant_name: func_key.variant_name.clone(),
            },
            index,
        );
    }
    *recursion_ir = func_comp.ir.clone();
    // Deals with self recursive function
    for (index, ir) in insert_var_vec.clone() {
        recursion_ir.insert(index, ir);

        let current_call = recursion_ir[index - 1].clone();

        match current_call {
            Air::Call { scope, count, tipo } => {
                recursion_ir[index - 1] = Air::Call {
                    scope,
                    count: count + 1,
                    tipo,
                }
            }
            _ => unreachable!("Will support not using call right away later."),
        }
    }
}

pub fn lookup_data_type_by_tipo(
    data_types: IndexMap<DataTypeKey, &TypedDataType>,
    tipo: &Type,
) -> Option<DataType<Arc<Type>>> {
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

pub fn check_replaceable_opaque_type(
    t: &Arc<Type>,
    data_types: &IndexMap<DataTypeKey, &TypedDataType>,
) -> bool {
    let data_type = lookup_data_type_by_tipo(data_types.clone(), t);

    if let Some(data_type) = data_type {
        let data_type_args = data_type.constructors[0].arguments.clone();
        data_type_args.len() == 1 && data_type.opaque && data_type.constructors.len() == 1
    } else {
        false
    }
}

pub fn replace_opaque_type(t: &mut Arc<Type>, data_types: IndexMap<DataTypeKey, &TypedDataType>) {
    if check_replaceable_opaque_type(t, &data_types) && matches!(&**t, Type::App { .. }) {
        let data_type = lookup_data_type_by_tipo(data_types.clone(), t).unwrap();
        let new_type_fields = data_type.typed_parameters.clone();

        let mut mono_types: IndexMap<u64, Arc<Type>> = IndexMap::new();

        for (tipo, param) in new_type_fields.iter().zip(t.arg_types().unwrap()) {
            let mut map = mono_types.into_iter().collect_vec();
            map.append(&mut get_generic_id_and_type(tipo, &param));
            mono_types = map.into_iter().collect();
        }

        let mut generic_type = data_type.constructors[0].arguments[0].tipo.clone();

        find_and_replace_generics(&mut generic_type, &mono_types);

        replace_opaque_type(&mut generic_type, data_types.clone());
        *t = generic_type;
    } else {
        match (**t).clone() {
            Type::App {
                public,
                module,
                name,
                args,
            } => {
                let mut new_args = vec![];
                for arg in args {
                    let mut new_arg_type = arg.clone();
                    replace_opaque_type(&mut new_arg_type, data_types.clone());
                    new_args.push(new_arg_type);
                }
                *t = Type::App {
                    public,
                    module,
                    name,
                    args: new_args,
                }
                .into();
            }
            Type::Fn { args, ret } => {
                let mut new_args = vec![];
                for arg in args {
                    let mut new_arg_type = arg.clone();
                    replace_opaque_type(&mut new_arg_type, data_types.clone());
                    new_args.push(new_arg_type);
                }

                let mut new_ret = ret;
                replace_opaque_type(&mut new_ret, data_types.clone());

                *t = Type::Fn {
                    args: new_args,
                    ret: new_ret,
                }
                .into();
            }
            Type::Var { tipo } => {
                if let TypeVar::Link { tipo } = &*tipo.borrow() {
                    let mut new_type = tipo.clone();
                    replace_opaque_type(&mut new_type, data_types.clone());
                    *t = new_type;
                }
            }
            Type::Tuple { elems } => {
                let mut new_elems = vec![];
                for arg in elems {
                    let mut new_arg_type = arg.clone();
                    replace_opaque_type(&mut new_arg_type, data_types.clone());
                    new_elems.push(new_arg_type);
                }
                *t = Type::Tuple { elems: new_elems }.into();
            }
        }
    }
}

pub fn handle_clause_guard(
    clause_guard: &ClauseGuard<Arc<Type>>,
    clause_guard_stack: &mut AirStack,
) {
    match clause_guard {
        ClauseGuard::Not { value, .. } => {
            let mut value_stack = clause_guard_stack.empty_with_scope();

            handle_clause_guard(value, &mut value_stack);

            clause_guard_stack.unop(UnOp::Not, value_stack);
        }
        ClauseGuard::Equals { left, right, .. } => {
            let mut left_stack = clause_guard_stack.empty_with_scope();
            let mut right_stack = clause_guard_stack.empty_with_scope();

            handle_clause_guard(left, &mut left_stack);
            handle_clause_guard(right, &mut right_stack);

            clause_guard_stack.binop(BinOp::Eq, left.tipo(), left_stack, right_stack);
        }
        ClauseGuard::NotEquals { left, right, .. } => {
            let mut left_stack = clause_guard_stack.empty_with_scope();
            let mut right_stack = clause_guard_stack.empty_with_scope();

            handle_clause_guard(left, &mut left_stack);
            handle_clause_guard(right, &mut right_stack);

            clause_guard_stack.binop(BinOp::NotEq, left.tipo(), left_stack, right_stack);
        }
        ClauseGuard::GtInt { left, right, .. } => {
            let mut left_stack = clause_guard_stack.empty_with_scope();
            let mut right_stack = clause_guard_stack.empty_with_scope();

            handle_clause_guard(left, &mut left_stack);
            handle_clause_guard(right, &mut right_stack);

            clause_guard_stack.binop(BinOp::GtInt, left.tipo(), left_stack, right_stack);
        }
        ClauseGuard::GtEqInt { left, right, .. } => {
            let mut left_stack = clause_guard_stack.empty_with_scope();
            let mut right_stack = clause_guard_stack.empty_with_scope();

            handle_clause_guard(left, &mut left_stack);
            handle_clause_guard(right, &mut right_stack);

            clause_guard_stack.binop(BinOp::GtEqInt, left.tipo(), left_stack, right_stack);
        }
        ClauseGuard::LtInt { left, right, .. } => {
            let mut left_stack = clause_guard_stack.empty_with_scope();
            let mut right_stack = clause_guard_stack.empty_with_scope();

            handle_clause_guard(left, &mut left_stack);
            handle_clause_guard(right, &mut right_stack);

            clause_guard_stack.binop(BinOp::LtInt, left.tipo(), left_stack, right_stack);
        }
        ClauseGuard::LtEqInt { left, right, .. } => {
            let mut left_stack = clause_guard_stack.empty_with_scope();
            let mut right_stack = clause_guard_stack.empty_with_scope();

            handle_clause_guard(left, &mut left_stack);
            handle_clause_guard(right, &mut right_stack);

            clause_guard_stack.binop(BinOp::LtEqInt, left.tipo(), left_stack, right_stack);
        }
        ClauseGuard::Or { left, right, .. } => {
            let mut left_stack = clause_guard_stack.empty_with_scope();
            let mut right_stack = clause_guard_stack.empty_with_scope();

            handle_clause_guard(left, &mut left_stack);
            handle_clause_guard(right, &mut right_stack);

            clause_guard_stack.binop(BinOp::Or, left.tipo(), left_stack, right_stack);
        }
        ClauseGuard::And { left, right, .. } => {
            let mut left_stack = clause_guard_stack.empty_with_scope();
            let mut right_stack = clause_guard_stack.empty_with_scope();

            handle_clause_guard(left, &mut left_stack);
            handle_clause_guard(right, &mut right_stack);

            clause_guard_stack.binop(BinOp::And, left.tipo(), left_stack, right_stack);
        }
        ClauseGuard::Var { tipo, name, .. } => {
            clause_guard_stack.local_var(tipo.clone(), name);
        }
        ClauseGuard::Constant(constant) => {
            constants_ir(constant, clause_guard_stack);
        }
    }
}
