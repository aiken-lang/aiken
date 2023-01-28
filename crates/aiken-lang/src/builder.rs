use std::{cell::RefCell, sync::Arc};

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use uplc::{
    ast::{
        builder::{apply_wrap, choose_list, if_else},
        Constant as UplcConstant, Name, Term, Type as UplcType,
    },
    builtins::DefaultFunction,
    machine::runtime::convert_constr_to_tag,
    BigInt, Constr, KeyValuePairs, PlutusData,
};

use crate::{
    air::Air,
    ast::{AssignmentKind, Clause, Constant, DataType, Pattern, Span, TypedArg, TypedDataType},
    expr::TypedExpr,
    tipo::{PatternConstructor, Type, TypeVar, ValueConstructorVariant},
};

#[derive(Clone, Debug)]
pub struct FuncComponents {
    pub ir: Vec<Air>,
    pub dependencies: Vec<FunctionAccessKey>,
    pub args: Vec<String>,
    pub recursive: bool,
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
    pub value_is_data: bool,
    pub kind: AssignmentKind,
}

#[derive(Clone, Debug)]
pub enum ClauseProperties {
    ConstrClause {
        clause_var_name: String,
        needs_constr_var: bool,
        is_complex_clause: bool,
        original_subject_name: String,
    },
    ListClause {
        clause_var_name: String,
        needs_constr_var: bool,
        is_complex_clause: bool,
        original_subject_name: String,
        current_index: i64,
    },
    TupleClause {
        clause_var_name: String,
        needs_constr_var: bool,
        is_complex_clause: bool,
        original_subject_name: String,
        defined_tuple_indices: IndexSet<(usize, String)>,
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
            }
        } else if t.is_tuple() {
            ClauseProperties::TupleClause {
                clause_var_name: constr_var,
                needs_constr_var: false,
                is_complex_clause: false,
                original_subject_name: subject_name,
                defined_tuple_indices: IndexSet::new(),
            }
        } else {
            ClauseProperties::ConstrClause {
                clause_var_name: constr_var,
                needs_constr_var: false,
                is_complex_clause: false,
                original_subject_name: subject_name,
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
        apply_wrap(DefaultFunction::BData.into(), term)
    } else if field_type.is_int() {
        apply_wrap(DefaultFunction::IData.into(), term)
    } else if field_type.is_map() {
        apply_wrap(DefaultFunction::MapData.into(), term)
    } else if field_type.is_string() {
        apply_wrap(
            DefaultFunction::BData.into(),
            apply_wrap(DefaultFunction::EncodeUtf8.into(), term),
        )
    } else if field_type.is_tuple() && matches!(field_type.get_uplc_type(), UplcType::Pair(_, _)) {
        apply_wrap(
            Term::Lambda {
                parameter_name: Name {
                    text: "__pair".to_string(),
                    unique: 0.into(),
                },
                body: apply_wrap(
                    DefaultFunction::ListData.into(),
                    apply_wrap(
                        apply_wrap(
                            Term::Builtin(DefaultFunction::MkCons).force_wrap(),
                            apply_wrap(
                                Term::Builtin(DefaultFunction::FstPair)
                                    .force_wrap()
                                    .force_wrap(),
                                Term::Var(Name {
                                    text: "__pair".to_string(),
                                    unique: 0.into(),
                                }),
                            ),
                        ),
                        apply_wrap(
                            apply_wrap(
                                Term::Builtin(DefaultFunction::MkCons).force_wrap(),
                                apply_wrap(
                                    Term::Builtin(DefaultFunction::SndPair)
                                        .force_wrap()
                                        .force_wrap(),
                                    Term::Var(Name {
                                        text: "__pair".to_string(),
                                        unique: 0.into(),
                                    }),
                                ),
                            ),
                            Term::Constant(UplcConstant::ProtoList(UplcType::Data, vec![])),
                        ),
                    ),
                )
                .into(),
            },
            term,
        )
    } else if field_type.is_list() || field_type.is_tuple() {
        apply_wrap(DefaultFunction::ListData.into(), term)
    } else if field_type.is_bool() {
        if_else(
            term,
            Term::Constant(UplcConstant::Data(PlutusData::Constr(Constr {
                tag: convert_constr_to_tag(1),
                any_constructor: None,
                fields: vec![],
            }))),
            Term::Constant(UplcConstant::Data(PlutusData::Constr(Constr {
                tag: convert_constr_to_tag(0),
                any_constructor: None,
                fields: vec![],
            }))),
        )
    } else {
        term
    }
}

pub fn convert_data_to_type(term: Term<Name>, field_type: &Arc<Type>) -> Term<Name> {
    if field_type.is_int() {
        apply_wrap(DefaultFunction::UnIData.into(), term)
    } else if field_type.is_bytearray() {
        apply_wrap(DefaultFunction::UnBData.into(), term)
    } else if field_type.is_map() {
        apply_wrap(DefaultFunction::UnMapData.into(), term)
    } else if field_type.is_string() {
        apply_wrap(
            DefaultFunction::DecodeUtf8.into(),
            apply_wrap(DefaultFunction::UnBData.into(), term),
        )
    } else if field_type.is_tuple() && matches!(field_type.get_uplc_type(), UplcType::Pair(_, _)) {
        apply_wrap(
            Term::Lambda {
                parameter_name: Name {
                    text: "__list_data".to_string(),
                    unique: 0.into(),
                },
                body: apply_wrap(
                    Term::Lambda {
                        parameter_name: Name {
                            text: "__tail".to_string(),
                            unique: 0.into(),
                        },
                        body: apply_wrap(
                            apply_wrap(
                                Term::Builtin(DefaultFunction::MkPairData),
                                apply_wrap(
                                    Term::Builtin(DefaultFunction::HeadList).force_wrap(),
                                    Term::Var(Name {
                                        text: "__list_data".to_string(),
                                        unique: 0.into(),
                                    }),
                                ),
                            ),
                            apply_wrap(
                                Term::Builtin(DefaultFunction::HeadList).force_wrap(),
                                Term::Var(Name {
                                    text: "__tail".to_string(),
                                    unique: 0.into(),
                                }),
                            ),
                        )
                        .into(),
                    },
                    apply_wrap(
                        Term::Builtin(DefaultFunction::TailList).force_wrap(),
                        Term::Var(Name {
                            text: "__list_data".to_string(),
                            unique: 0.into(),
                        }),
                    ),
                )
                .into(),
            },
            apply_wrap(Term::Builtin(DefaultFunction::UnListData), term),
        )
    } else if field_type.is_list() || field_type.is_tuple() {
        apply_wrap(DefaultFunction::UnListData.into(), term)
    } else if field_type.is_bool() {
        apply_wrap(
            apply_wrap(
                DefaultFunction::EqualsInteger.into(),
                Term::Constant(UplcConstant::Integer(1)),
            ),
            apply_wrap(
                Term::Builtin(DefaultFunction::FstPair)
                    .force_wrap()
                    .force_wrap(),
                apply_wrap(DefaultFunction::UnConstrData.into(), term),
            ),
        )
    } else {
        term
    }
}

pub fn rearrange_clauses(
    clauses: Vec<Clause<TypedExpr, PatternConstructor, Arc<Type>, String>>,
) -> Vec<Clause<TypedExpr, PatternConstructor, Arc<Type>, String>> {
    let mut sorted_clauses = clauses;

    // if we have a list sort clauses so we can plug holes for cases not covered by clauses
    // TODO: while having 10000000 element list is impossible to destructure in plutus budget,
    // let's sort clauses by a safer manner
    // TODO: how shall tails be weighted? Since any clause after will not run
    sorted_clauses.sort_by(|clause1, clause2| {
        let clause1_len = match &clause1.pattern[0] {
            Pattern::List { elements, tail, .. } => elements.len() + usize::from(tail.is_some()),
            _ => 10000000,
        };
        let clause2_len = match &clause2.pattern[0] {
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
    let plug_in_then = match &sorted_clauses[sorted_clauses.len() - 1].pattern[0] {
        Pattern::Var { name, .. } => {
            assign_plug_in_name = Some(name);
            sorted_clauses[sorted_clauses.len() - 1].clone().then
        }
        Pattern::Discard { .. } => sorted_clauses[sorted_clauses.len() - 1].clone().then,
        _ => TypedExpr::ErrorTerm {
            location: Span::empty(),
            tipo: sorted_clauses[sorted_clauses.len() - 1].then.tipo(),
            label: Some("Clause not filled".to_string()),
        },
    };

    for (index, clause) in sorted_clauses.iter().enumerate() {
        if let Pattern::List { elements, .. } = &clause.pattern[0] {
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
                    Clause {
                        location: Span::empty(),
                        pattern: vec![Pattern::Assign {
                            name: name.clone(),
                            location: Span::empty(),
                            pattern: Pattern::List {
                                location: Span::empty(),
                                elements: discard_elems,
                                tail: None,
                            }
                            .into(),
                        }],
                        alternative_patterns: vec![],
                        guard: None,
                        then: plug_in_then.clone(),
                    }
                } else {
                    Clause {
                        location: Span::empty(),
                        pattern: vec![Pattern::List {
                            location: Span::empty(),
                            elements: discard_elems,
                            tail: None,
                        }],
                        alternative_patterns: vec![],
                        guard: None,
                        then: plug_in_then.clone(),
                    }
                };

                holes_to_fill.push((index, clause_to_fill));
                elems_len += 1;
            }
        }

        // if we have a pattern with no clause guards and a tail then no lists will get past here to other clauses
        match &clause.pattern[0] {
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
            if let Pattern::List { tail: None, .. } = &clause.pattern[0] {
                final_clauses.push(Clause {
                    location: Span::empty(),
                    pattern: vec![Pattern::Discard {
                        name: "_".to_string(),
                        location: Span::empty(),
                    }],
                    alternative_patterns: vec![],
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

pub fn list_access_to_uplc(
    names: &[String],
    id_list: &[u64],
    tail: bool,
    current_index: usize,
    term: Term<Name>,
    tipo: &Type,
    check_last_item: bool,
) -> Term<Name> {
    let (first, names) = names.split_first().unwrap();

    let head_list = if tipo.is_map() {
        apply_wrap(
            Term::Builtin(DefaultFunction::HeadList).force_wrap(),
            Term::Var(Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            }),
        )
    } else {
        convert_data_to_type(
            apply_wrap(
                Term::Builtin(DefaultFunction::HeadList).force_wrap(),
                Term::Var(Name {
                    text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                    unique: 0.into(),
                }),
            ),
            &tipo.clone().get_inner_types()[0],
        )
    };

    if names.len() == 1 && tail {
        Term::Lambda {
            parameter_name: Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            },
            body: apply_wrap(
                Term::Lambda {
                    parameter_name: Name {
                        text: first.clone(),
                        unique: 0.into(),
                    },
                    body: apply_wrap(
                        Term::Lambda {
                            parameter_name: Name {
                                text: names[0].clone(),
                                unique: 0.into(),
                            },
                            body: term.into(),
                        },
                        apply_wrap(
                            Term::Builtin(DefaultFunction::TailList).force_wrap(),
                            Term::Var(Name {
                                text: format!(
                                    "tail_index_{}_{}",
                                    current_index, id_list[current_index]
                                ),
                                unique: 0.into(),
                            }),
                        ),
                    )
                    .into(),
                },
                head_list,
            )
            .into(),
        }
    } else if names.is_empty() {
        // Maybe check list is actually empty or should we leave that to when .. is only
        // this would replace term.into() if we decide to
        Term::Lambda {
            parameter_name: Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            },
            body: apply_wrap(
                Term::Lambda {
                    parameter_name: Name {
                        text: first.clone(),
                        unique: 0.into(),
                    },
                    body: if check_last_item {
                        choose_list(
                            apply_wrap(
                                Term::Builtin(DefaultFunction::TailList).force_wrap(),
                                Term::Var(Name {
                                    text: format!(
                                        "tail_index_{}_{}",
                                        current_index, id_list[current_index]
                                    ),
                                    unique: 0.into(),
                                }),
                            ),
                            term,
                            apply_wrap(
                                apply_wrap(
                                    Term::Builtin(DefaultFunction::Trace).force_wrap(),
                                    Term::Constant(UplcConstant::String(
                                        "List/Tuple contains more items than it should".to_string(),
                                    )),
                                ),
                                Term::Delay(Term::Error.into()),
                            )
                            .force_wrap(),
                        )
                        .into()
                    } else {
                        term.into()
                    },
                },
                apply_wrap(
                    Term::Builtin(DefaultFunction::HeadList).force_wrap(),
                    Term::Var(Name {
                        text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                        unique: 0.into(),
                    }),
                ),
            )
            .into(),
        }
    } else {
        Term::Lambda {
            parameter_name: Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            },
            body: apply_wrap(
                Term::Lambda {
                    parameter_name: Name {
                        text: first.clone(),
                        unique: 0.into(),
                    },
                    body: apply_wrap(
                        list_access_to_uplc(
                            names,
                            id_list,
                            tail,
                            current_index + 1,
                            term,
                            tipo,
                            check_last_item,
                        ),
                        apply_wrap(
                            Term::Builtin(DefaultFunction::TailList).force_wrap(),
                            Term::Var(Name {
                                text: format!(
                                    "tail_index_{}_{}",
                                    current_index, id_list[current_index]
                                ),
                                unique: 0.into(),
                            }),
                        ),
                    )
                    .into(),
                },
                head_list,
            )
            .into(),
        }
    }
}

pub fn get_common_ancestor(scope: &[u64], scope_prev: &[u64]) -> Vec<u64> {
    let longest_length = if scope.len() >= scope_prev.len() {
        scope.len()
    } else {
        scope_prev.len()
    };

    if *scope == *scope_prev {
        return scope.to_vec();
    }

    for index in 0..longest_length {
        if scope.get(index).is_none() {
            return scope.to_vec();
        } else if scope_prev.get(index).is_none() {
            return scope_prev.to_vec();
        } else if scope[index] != scope_prev[index] {
            return scope[0..index].to_vec();
        }
    }
    vec![]
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
        Pattern::Discard { .. } => {}
        _ => todo!("{pattern:#?}"),
    }
}

pub fn constants_ir(
    literal: &Constant<Arc<Type>, String>,
    ir_stack: &mut Vec<Air>,
    scope: Vec<u64>,
) {
    match literal {
        Constant::Int { value, .. } => {
            ir_stack.push(Air::Int {
                scope,
                value: value.clone(),
            });
        }
        Constant::String { value, .. } => {
            ir_stack.push(Air::String {
                scope,
                value: value.clone(),
            });
        }
        Constant::Tuple { .. } => {
            todo!()
        }
        Constant::List { elements, tipo, .. } => {
            ir_stack.push(Air::List {
                scope: scope.clone(),
                count: elements.len(),
                tipo: tipo.clone(),
                tail: false,
            });

            for element in elements {
                constants_ir(element, ir_stack, scope.clone());
            }
        }
        Constant::Record { .. } => {
            // ir_stack.push(Air::Record { scope,  });
            todo!()
        }
        Constant::ByteArray { bytes, .. } => {
            ir_stack.push(Air::ByteArray {
                scope,
                bytes: bytes.clone(),
            });
        }
        Constant::Var { .. } => todo!(),
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

pub fn find_generics_to_replace(tipo: &mut Arc<Type>, generic_types: &IndexMap<u64, Arc<Type>>) {
    if let Some(id) = tipo.get_generic() {
        //If generic does not have a type we know of like a None in option then just use same type
        *tipo = generic_types.get(&id).unwrap_or(tipo).clone();
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
                    find_generics_to_replace(&mut arg, generic_types);
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
                    find_generics_to_replace(&mut arg, generic_types);
                    new_args.push(arg);
                }

                let mut ret = ret.clone();
                find_generics_to_replace(&mut ret, generic_types);

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
                    find_generics_to_replace(&mut elem, generic_types);
                    new_elems.push(elem);
                }
                let t = Type::Tuple { elems: new_elems };
                *tipo = t.into();
            }
            Type::Var { tipo: var_tipo } => {
                let var_type = var_tipo.as_ref().borrow().clone();
                let var_tipo = match var_type {
                    TypeVar::Unbound { .. } => todo!(),
                    TypeVar::Link { tipo } => {
                        let mut tipo = tipo;
                        find_generics_to_replace(&mut tipo, generic_types);
                        tipo
                    }
                    TypeVar::Generic { .. } => unreachable!(),
                };

                let t = Type::Var {
                    tipo: RefCell::from(TypeVar::Link { tipo: var_tipo }).into(),
                };
                *tipo = t.into()
            }
        };
    }
}

pub fn get_generics_and_type(tipo: &Type, param: &Type) -> Vec<(u64, Arc<Type>)> {
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
        generics_ids.append(&mut get_generics_and_type(tipo, param_type));
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

        let inner_types = t.get_inner_types();

        for arg_type in inner_types {
            get_variant_name(&mut full_type, &arg_type);
        }
        full_type
    });
}

pub fn convert_constants_to_data(constants: Vec<UplcConstant>) -> Vec<UplcConstant> {
    let mut new_constants = vec![];
    for constant in constants {
        let constant = match constant {
            UplcConstant::Integer(i) => {
                UplcConstant::Data(PlutusData::BigInt(BigInt::Int((i).try_into().unwrap())))
            }
            UplcConstant::ByteString(b) => {
                UplcConstant::Data(PlutusData::BoundedBytes(b.try_into().unwrap()))
            }
            UplcConstant::String(s) => UplcConstant::Data(PlutusData::BoundedBytes(
                s.as_bytes().to_vec().try_into().unwrap(),
            )),

            UplcConstant::Bool(b) => UplcConstant::Data(PlutusData::Constr(Constr {
                tag: convert_constr_to_tag(b.into()),
                any_constructor: None,
                fields: vec![],
            })),
            UplcConstant::ProtoList(_, constants) => {
                let inner_constants = convert_constants_to_data(constants)
                    .into_iter()
                    .map(|constant| match constant {
                        UplcConstant::Data(d) => d,
                        _ => todo!(),
                    })
                    .collect_vec();

                UplcConstant::Data(PlutusData::Array(inner_constants))
            }
            UplcConstant::ProtoPair(_, _, left, right) => {
                let inner_constants = vec![*left, *right];
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
            d @ UplcConstant::Data(_) => d,
            _ => unreachable!(),
        };
        new_constants.push(constant);
    }
    new_constants
}

pub fn wrap_validator_args(term: Term<Name>, arguments: Vec<TypedArg>) -> Term<Name> {
    let mut term = term;
    for arg in arguments.iter().rev() {
        if !matches!(arg.tipo.get_uplc_type(), UplcType::Data) {
            term = apply_wrap(
                Term::Lambda {
                    parameter_name: Name {
                        text: arg.arg_name.get_variable_name().unwrap_or("_").to_string(),
                        unique: 0.into(),
                    },
                    body: term.into(),
                },
                convert_data_to_type(
                    Term::Var(Name {
                        text: arg.arg_name.get_variable_name().unwrap_or("_").to_string(),
                        unique: 0.into(),
                    }),
                    &arg.tipo,
                ),
            );
        }

        term = Term::Lambda {
            parameter_name: Name {
                text: arg.arg_name.get_variable_name().unwrap_or("_").to_string(),
                unique: 0.into(),
            },
            body: term.into(),
        }
    }
    term
}

pub fn monomorphize(
    ir: Vec<Air>,
    generic_types: IndexMap<u64, Arc<Type>>,
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

                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::ListExpose {
                        scope,
                        tail_head_names,
                        tipo,
                        tail,
                    };
                    needs_variant = true;
                }
            }
            Air::BinOp {
                scope,
                name,
                count,
                tipo,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::BinOp {
                        scope,
                        name,
                        tipo,
                        count,
                    };
                    needs_variant = true;
                }
            }
            Air::Builtin { scope, func, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::Builtin { scope, func, tipo };
                    needs_variant = true;
                }
            }
            Air::UnWrapData { scope, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::UnWrapData { scope, tipo };
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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::TupleIndex {
                        scope,
                        tipo,
                        tuple_index,
                    };
                    needs_variant = true;
                }
            }
            Air::Todo { scope, label, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::Todo { scope, tipo, label };
                    needs_variant = true;
                }
            }
            Air::ErrorTerm { scope, label, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::ErrorTerm { scope, tipo, label };
                    needs_variant = true;
                }
            }
            Air::Trace { scope, text, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::Trace { scope, tipo, text };
                    needs_variant = true;
                }
            }
            Air::Record {
                scope,
                constr_index,
                tipo,
                count,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::Record {
                        scope,
                        tipo,
                        constr_index,
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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                        find_generics_to_replace(&mut tipo, &generic_types);
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
                        find_generics_to_replace(&mut tipo, &generic_types);
                        needs_variant = true;
                        new_indices.push((ind, tipo));
                    } else {
                        new_indices.push((ind, tipo));
                    }
                }
                if tipo.is_generic() {
                    find_generics_to_replace(&mut tipo, &generic_types);
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
                    find_generics_to_replace(&mut tipo, &generic_types);

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
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::Call { scope, count, tipo };
                    needs_variant = true;
                }
            }
            Air::If { scope, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

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

pub fn handle_func_dependencies_ir(
    dependencies_ir: &mut Vec<Air>,
    funt_comp: &FuncComponents,
    func_components: &IndexMap<FunctionAccessKey, FuncComponents>,
    defined_functions: &mut IndexMap<FunctionAccessKey, ()>,
    func_index_map: &IndexMap<FunctionAccessKey, Vec<u64>>,
    func_scope: &[u64],
    to_be_defined: &mut IndexMap<FunctionAccessKey, ()>,
) {
    let mut funt_comp = funt_comp.clone();

    let mut dependency_map = IndexMap::new();
    let mut dependency_vec = vec![];

    // deal with function dependencies by sorting order in which we pop them.
    while let Some(dependency) = funt_comp.dependencies.pop() {
        let depend_comp = func_components.get(&dependency).unwrap();
        if dependency_map.contains_key(&dependency) {
            dependency_map.shift_remove(&dependency);
        }
        dependency_map.insert(dependency, ());
        funt_comp
            .dependencies
            .extend(depend_comp.dependencies.clone().into_iter());
    }

    dependency_vec.extend(dependency_map.keys().cloned());
    dependency_vec.reverse();

    while let Some(dependency) = dependency_vec.pop() {
        if (defined_functions.contains_key(&dependency) && !funt_comp.args.is_empty())
            || func_components.get(&dependency).is_none()
        {
            continue;
        }

        let depend_comp = func_components.get(&dependency).unwrap();
        let dep_scope = func_index_map.get(&dependency).unwrap();

        if get_common_ancestor(dep_scope, func_scope) == func_scope.to_vec()
            || funt_comp.args.is_empty()
        {
            // we handle zero arg functions and their dependencies in a unique way
            if !depend_comp.args.is_empty() {
                let mut recursion_ir = vec![];
                handle_recursion_ir(&dependency, depend_comp, &mut recursion_ir);

                let mut temp_ir = vec![Air::DefineFunc {
                    scope: func_scope.to_vec(),
                    func_name: dependency.function_name.clone(),
                    module_name: dependency.module_name.clone(),
                    params: depend_comp.args.clone(),
                    recursive: depend_comp.recursive,
                    variant_name: dependency.variant_name.clone(),
                }];

                temp_ir.append(&mut recursion_ir);

                temp_ir.append(dependencies_ir);

                *dependencies_ir = temp_ir;
                if get_common_ancestor(dep_scope, func_scope) == func_scope.to_vec() {
                    defined_functions.insert(dependency, ());
                }
            }
        } else {
            // Dependency will need to be defined somewhere in the main body
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
            _ => unreachable!(),
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

        let mut generics_type_map: IndexMap<u64, Arc<Type>> = IndexMap::new();

        for (tipo, param) in new_type_fields.iter().zip(t.arg_types().unwrap()) {
            let mut map = generics_type_map.into_iter().collect_vec();
            map.append(&mut get_generics_and_type(tipo, &param));
            generics_type_map = map.into_iter().collect();
        }

        let mut generic_type = data_type.constructors[0].arguments[0].tipo.clone();

        find_generics_to_replace(&mut generic_type, &generics_type_map);

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
