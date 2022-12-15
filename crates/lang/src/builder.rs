use std::{cell::RefCell, collections::HashMap, sync::Arc};

use itertools::Itertools;
use uplc::{
    ast::{Constant as UplcConstant, Name, Term, Type as UplcType},
    builtins::DefaultFunction,
    machine::runtime::convert_constr_to_tag,
    BigInt, Constr, KeyValuePairs, PlutusData,
};

use crate::{
    air::Air,
    ast::{Clause, Constant, Pattern, Span},
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
pub struct ClauseProperties {
    pub clause_var_name: String,
    pub needs_constr_var: bool,
    pub is_complex_clause: bool,
    pub current_index: usize,
    pub original_subject_name: String,
}

pub fn convert_type_to_data(term: Term<Name>, field_type: &Arc<Type>) -> Term<Name> {
    if field_type.is_bytearray() {
        Term::Apply {
            function: DefaultFunction::BData.into(),
            argument: term.into(),
        }
    } else if field_type.is_int() {
        Term::Apply {
            function: DefaultFunction::IData.into(),
            argument: term.into(),
        }
    } else if field_type.is_map() {
        Term::Apply {
            function: DefaultFunction::MapData.into(),
            argument: term.into(),
        }
    } else if field_type.is_list() {
        Term::Apply {
            function: DefaultFunction::ListData.into(),
            argument: term.into(),
        }
    } else if field_type.is_string() {
        Term::Apply {
            function: DefaultFunction::BData.into(),
            argument: Term::Apply {
                function: DefaultFunction::EncodeUtf8.into(),
                argument: term.into(),
            }
            .into(),
        }
    } else if field_type.is_tuple() {
        match field_type.get_uplc_type() {
            UplcType::List(_) => Term::Apply {
                function: DefaultFunction::ListData.into(),
                argument: term.into(),
            },
            UplcType::Pair(_, _) => Term::Apply {
                function: Term::Lambda {
                    parameter_name: Name {
                        text: "__pair".to_string(),
                        unique: 0.into(),
                    },
                    body: Term::Apply {
                        function: DefaultFunction::ListData.into(),
                        argument: Term::Apply {
                            function: Term::Apply {
                                function: Term::Builtin(DefaultFunction::MkCons)
                                    .force_wrap()
                                    .into(),
                                argument: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::FstPair)
                                        .force_wrap()
                                        .force_wrap()
                                        .into(),
                                    argument: Term::Var(Name {
                                        text: "__pair".to_string(),
                                        unique: 0.into(),
                                    })
                                    .into(),
                                }
                                .into(),
                            }
                            .into(),

                            argument: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::MkCons)
                                        .force_wrap()
                                        .into(),
                                    argument: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::SndPair)
                                            .force_wrap()
                                            .force_wrap()
                                            .into(),
                                        argument: Term::Var(Name {
                                            text: "__pair".to_string(),
                                            unique: 0.into(),
                                        })
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                argument: Term::Constant(UplcConstant::ProtoList(
                                    UplcType::Data,
                                    vec![],
                                ))
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                }
                .into(),
                argument: term.into(),
            },
            _ => unreachable!(),
        }
    } else if field_type.is_bool() {
        Term::Apply {
            function: Term::Apply {
                function: Term::Apply {
                    function: Term::Builtin(DefaultFunction::IfThenElse)
                        .force_wrap()
                        .into(),
                    argument: term.into(),
                }
                .into(),
                argument: Term::Constant(UplcConstant::Data(PlutusData::Constr(Constr {
                    tag: convert_constr_to_tag(1),
                    any_constructor: None,
                    fields: vec![],
                })))
                .into(),
            }
            .into(),
            argument: Term::Constant(UplcConstant::Data(PlutusData::Constr(Constr {
                tag: convert_constr_to_tag(0),
                any_constructor: None,
                fields: vec![],
            })))
            .into(),
        }
    } else {
        term
    }
}

pub fn convert_data_to_type(term: Term<Name>, field_type: &Arc<Type>) -> Term<Name> {
    if field_type.is_int() {
        Term::Apply {
            function: DefaultFunction::UnIData.into(),
            argument: term.into(),
        }
    } else if field_type.is_bytearray() {
        Term::Apply {
            function: DefaultFunction::UnBData.into(),
            argument: term.into(),
        }
    } else if field_type.is_map() {
        Term::Apply {
            function: DefaultFunction::UnMapData.into(),
            argument: term.into(),
        }
    } else if field_type.is_list() {
        Term::Apply {
            function: DefaultFunction::UnListData.into(),
            argument: term.into(),
        }
    } else if field_type.is_string() {
        Term::Apply {
            function: DefaultFunction::DecodeUtf8.into(),
            argument: Term::Apply {
                function: DefaultFunction::UnBData.into(),
                argument: term.into(),
            }
            .into(),
        }
    } else if field_type.is_tuple() {
        match field_type.get_uplc_type() {
            UplcType::List(_) => Term::Apply {
                function: DefaultFunction::UnListData.into(),
                argument: term.into(),
            },
            UplcType::Pair(_, _) => Term::Apply {
                function: Term::Lambda {
                    parameter_name: Name {
                        text: "__list_data".to_string(),
                        unique: 0.into(),
                    },
                    body: Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: "__tail".to_string(),
                                unique: 0.into(),
                            },
                            body: Term::Apply {
                                function: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::MkPairData).into(),
                                    argument: Term::Apply {
                                        function: Term::Builtin(DefaultFunction::HeadList)
                                            .force_wrap()
                                            .into(),
                                        argument: Term::Var(Name {
                                            text: "__list_data".to_string(),
                                            unique: 0.into(),
                                        })
                                        .into(),
                                    }
                                    .into(),
                                }
                                .into(),
                                argument: Term::Apply {
                                    function: Term::Builtin(DefaultFunction::HeadList)
                                        .force_wrap()
                                        .into(),
                                    argument: Term::Var(Name {
                                        text: "__tail".to_string(),
                                        unique: 0.into(),
                                    })
                                    .into(),
                                }
                                .into(),
                            }
                            .into(),
                        }
                        .into(),
                        argument: Term::Apply {
                            function: Term::Builtin(DefaultFunction::TailList).force_wrap().into(),
                            argument: Term::Var(Name {
                                text: "__list_data".to_string(),
                                unique: 0.into(),
                            })
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                }
                .into(),
                argument: Term::Apply {
                    function: Term::Builtin(DefaultFunction::UnListData)
                        .force_wrap()
                        .into(),
                    argument: term.into(),
                }
                .into(),
            },
            _ => unreachable!(),
        }
    } else if field_type.is_bool() {
        Term::Apply {
            function: Term::Apply {
                function: Term::Builtin(DefaultFunction::EqualsInteger).into(),
                argument: Term::Constant(UplcConstant::Integer(1)).into(),
            }
            .into(),
            argument: Term::Apply {
                function: Term::Builtin(DefaultFunction::FstPair)
                    .force_wrap()
                    .force_wrap()
                    .into(),
                argument: Term::Apply {
                    function: Term::Builtin(DefaultFunction::UnConstrData).into(),
                    argument: term.into(),
                }
                .into(),
            }
            .into(),
        }
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
    let mut last_clause_index = sorted_clauses.len() - 1;
    let mut last_clause_set = false;

    // If we have a catch all, use that. Otherwise use todo which will result in error
    // TODO: fill in todo label with description
    let plug_in_then = match &sorted_clauses[sorted_clauses.len() - 1].pattern[0] {
        Pattern::Var { name, .. } => {
            assign_plug_in_name = Some(name);
            sorted_clauses[sorted_clauses.len() - 1].clone().then
        }
        Pattern::Discard { .. } => sorted_clauses[sorted_clauses.len() - 1].clone().then,
        _ => TypedExpr::Todo {
            location: Span::empty(),
            label: None,
            tipo: sorted_clauses[sorted_clauses.len() - 1].then.tipo(),
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
        if let Pattern::List {
            elements,
            tail: Some(tail),
            ..
        } = &clause.pattern[0]
        {
            let mut elements = elements.clone();
            elements.push(*tail.clone());
            if elements
                .iter()
                .all(|element| matches!(element, Pattern::Var { .. } | Pattern::Discard { .. }))
                && !last_clause_set
            {
                last_clause_index = index;
                last_clause_set = true;
            }
        }

        // If the last condition doesn't have a catch all or tail then add a catch all with a todo
        if index == sorted_clauses.len() - 1 {
            if let Pattern::List {
                elements,
                tail: Some(tail),
                ..
            } = &clause.pattern[0]
            {
                let mut elements = elements.clone();
                elements.push(*tail.clone());
                if !elements
                    .iter()
                    .all(|element| matches!(element, Pattern::Var { .. } | Pattern::Discard { .. }))
                {
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
        }

        elems_len += 1;
    }

    // Encountered a tail so stop there with that as last clause
    final_clauses = final_clauses[0..(last_clause_index + 1)].to_vec();

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
) -> Term<Name> {
    let (first, names) = names.split_first().unwrap();

    let head_list = if tipo.is_map() {
        Term::Apply {
            function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into()).into(),
            argument: Term::Var(Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            })
            .into(),
        }
    } else {
        convert_data_to_type(
            Term::Apply {
                function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into()).into(),
                argument: Term::Var(Name {
                    text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                    unique: 0.into(),
                })
                .into(),
            },
            &tipo.clone().get_inner_types()[0],
        )
    };

    if names.len() == 1 && tail {
        Term::Lambda {
            parameter_name: Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            },
            body: Term::Apply {
                function: Term::Lambda {
                    parameter_name: Name {
                        text: first.clone(),
                        unique: 0.into(),
                    },
                    body: Term::Apply {
                        function: Term::Lambda {
                            parameter_name: Name {
                                text: names[0].clone(),
                                unique: 0.into(),
                            },
                            body: term.into(),
                        }
                        .into(),
                        argument: Term::Apply {
                            function: Term::Force(Term::Builtin(DefaultFunction::TailList).into())
                                .into(),
                            argument: Term::Var(Name {
                                text: format!(
                                    "tail_index_{}_{}",
                                    current_index, id_list[current_index]
                                ),
                                unique: 0.into(),
                            })
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                }
                .into(),
                argument: head_list.into(),
            }
            .into(),
        }
    } else if names.is_empty() {
        Term::Lambda {
            parameter_name: Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            },
            body: Term::Apply {
                function: Term::Lambda {
                    parameter_name: Name {
                        text: first.clone(),
                        unique: 0.into(),
                    },
                    body: term.into(),
                }
                .into(),
                argument: Term::Apply {
                    function: Term::Force(Term::Builtin(DefaultFunction::HeadList).into()).into(),
                    argument: Term::Var(Name {
                        text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                        unique: 0.into(),
                    })
                    .into(),
                }
                .into(),
            }
            .into(),
        }
    } else {
        Term::Lambda {
            parameter_name: Name {
                text: format!("tail_index_{}_{}", current_index, id_list[current_index]),
                unique: 0.into(),
            },
            body: Term::Apply {
                function: Term::Lambda {
                    parameter_name: Name {
                        text: first.clone(),
                        unique: 0.into(),
                    },
                    body: Term::Apply {
                        function: list_access_to_uplc(
                            names,
                            id_list,
                            tail,
                            current_index + 1,
                            term,
                            tipo,
                        )
                        .into(),
                        argument: Term::Apply {
                            function: Term::Force(Term::Builtin(DefaultFunction::TailList).into())
                                .into(),
                            argument: Term::Var(Name {
                                text: format!(
                                    "tail_index_{}_{}",
                                    current_index, id_list[current_index]
                                ),
                                unique: 0.into(),
                            })
                            .into(),
                        }
                        .into(),
                    }
                    .into(),
                }
                .into(),
                argument: head_list.into(),
            }
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
    needs_access_to_constr_var: &mut bool,
    needs_clause_guard: &mut bool,
) {
    match pattern {
        Pattern::Var { .. } => {
            *needs_access_to_constr_var = true;
        }
        Pattern::List { .. }
        | Pattern::Constructor { .. }
        | Pattern::Tuple { .. }
        | Pattern::Int { .. } => {
            *needs_access_to_constr_var = true;
            *needs_clause_guard = true;
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

pub fn find_generics_to_replace(tipo: &mut Arc<Type>, generic_types: &HashMap<u64, Arc<Type>>) {
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
    new_name.push_str(&format!(
        "_{}",
        if t.is_string() {
            "string".to_string()
        } else if t.is_int() {
            "int".to_string()
        } else if t.is_bool() {
            "bool".to_string()
        } else if t.is_map() {
            let mut full_type = "map".to_string();
            let pair_type = &t.get_inner_types()[0];
            let fst_type = &pair_type.get_inner_types()[0];
            let snd_type = &pair_type.get_inner_types()[1];

            get_variant_name(&mut full_type, fst_type);
            get_variant_name(&mut full_type, snd_type);
            full_type
        } else if t.is_list() {
            let mut full_type = "list".to_string();
            let list_type = &t.get_inner_types()[0];
            get_variant_name(&mut full_type, list_type);
            full_type
        } else {
            "data".to_string()
        }
    ));
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
                tag: u64::from(b),
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

pub fn monomorphize(
    ir: Vec<Air>,
    generic_types: HashMap<u64, Arc<Type>>,
    full_type: &Arc<Type>,
) -> (String, Vec<Air>) {
    let mut new_air = ir.clone();
    let mut new_name = String::new();

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
                }
            }
            Air::ListAccessor {
                scope,
                tipo,
                names,
                tail,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::ListAccessor {
                        scope,
                        names,
                        tipo,
                        tail,
                    };
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
                }
            }
            Air::Builtin { scope, func, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::Builtin { scope, func, tipo };
                }
            }
            // TODO check on assignment if type is needed
            Air::Assignment { .. } => {}
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
                }
            }
            Air::RecordAccess {
                scope,
                index: record_index,
                tipo,
            } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::RecordAccess {
                        scope,
                        index: record_index,
                        tipo,
                    };
                }
            }
            Air::FieldsExpose {
                scope,
                count,
                indices,
            } => {
                let mut new_indices = vec![];
                for (ind, name, tipo) in indices {
                    if tipo.is_generic() {
                        let mut tipo = tipo.clone();
                        find_generics_to_replace(&mut tipo, &generic_types);
                    }
                    new_indices.push((ind, name, tipo));
                }
                new_air[index] = Air::FieldsExpose {
                    scope,
                    count,
                    indices: new_indices,
                };
            }
            Air::Tuple { scope, tipo, count } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::Tuple { scope, count, tipo };
                }
            }
            Air::Todo { scope, label, tipo } => {
                if tipo.is_generic() {
                    let mut tipo = tipo.clone();
                    find_generics_to_replace(&mut tipo, &generic_types);

                    new_air[index] = Air::Todo { scope, label, tipo };
                }
            }
            Air::RecordUpdate { .. } => todo!(),
            Air::TupleAccessor { .. } => todo!(),
            _ => {}
        }
    }

    if let Type::Fn { args, .. } = &**full_type {
        for arg in args {
            get_variant_name(&mut new_name, arg);
        }
    }

    (new_name, new_air)
}
