use super::analysis_support::*;
use super::fuzzer_analysis::*;
use super::shallow_ir::*;
use super::transition_prop::*;
use super::*;

pub(super) type StateMachineTraceFields =
    (u64, u64, Rc<Type>, Rc<Type>, SemanticType, SemanticType);

pub(super) fn extract_state_machine_trace_fields(
    transition_type: &Rc<Type>,
    state_type: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Result<StateMachineTraceFields, String> {
    let data_type =
        lookup_data_type_by_tipo(data_types, transition_type.as_ref()).ok_or_else(|| {
            format!(
                "state-machine step output '{}' is not a known transition data type",
                pretty_print_type(transition_type.as_ref())
            )
        })?;

    let zero_arity_ctors: Vec<_> = data_type
        .constructors
        .iter()
        .enumerate()
        .filter(|(_, ctor)| ctor.arguments.is_empty())
        .collect();
    let step_constructors: Vec<_> = data_type
        .constructors
        .iter()
        .enumerate()
        .filter(|(_, ctor)| ctor.arguments.len() == 3)
        .collect();

    if zero_arity_ctors.len() != 1
        || step_constructors.len() != 1
        || data_type.constructors.len() != 2
    {
        return Err(format!(
            "state-machine transition type '{}' must have one terminal constructor and one 3-field step constructor",
            pretty_print_type(transition_type.as_ref())
        ));
    }

    // Use `get_constr_index_variant` so `@tag(N)` decorators on the
    // constructors are honoured. Falling back to the enumeration index
    // preserves prior behaviour when the decorator lookup is unavailable.
    let terminal_tag = get_constr_index_variant(&data_type, &zero_arity_ctors[0].1.name)
        .map(|(i, _)| i as u64)
        .unwrap_or(zero_arity_ctors[0].0 as u64);
    let step_tag = get_constr_index_variant(&data_type, &step_constructors[0].1.name)
        .map(|(i, _)| i as u64)
        .unwrap_or(step_constructors[0].0 as u64);

    let mono_types: IndexMap<u64, Rc<Type>> = match transition_type.as_ref() {
        Type::App { args, .. } => data_type
            .typed_parameters
            .iter()
            .zip(args.iter())
            .flat_map(|(generic, arg)| get_generic_id_and_type(generic.as_ref(), arg.as_ref()))
            .collect(),
        _ => IndexMap::new(),
    };

    let step_fields: Vec<Rc<Type>> = step_constructors[0]
        .1
        .arguments
        .iter()
        .map(|field| find_and_replace_generics(&field.tipo, &mono_types))
        .collect();

    let next_state_type = convert_opaque_type(&step_fields[1], data_types, true);
    if next_state_type.as_ref() != state_type.as_ref() {
        return Err(format!(
            "state-machine transition state field '{}' does not match initial state type '{}'",
            pretty_print_type(next_state_type.as_ref()),
            pretty_print_type(state_type.as_ref())
        ));
    }

    let label_type = convert_opaque_type(&step_fields[0], data_types, true);
    let event_type = convert_opaque_type(&step_fields[2], data_types, true);
    let label_semantic_type = semantic_type_from_type(label_type.as_ref());
    let event_semantic_type = semantic_type_from_type(event_type.as_ref());

    Ok((
        terminal_tag,
        step_tag,
        label_type,
        event_type,
        label_semantic_type,
        event_semantic_type,
    ))
}

pub(super) fn infer_state_machine_acceptance_from_output_type(
    output_type: &Type,
) -> Option<StateMachineAcceptance> {
    if output_type.is_list() {
        return Some(StateMachineAcceptance::AcceptsSuccess);
    }

    if output_type.is_tuple() || output_type.is_pair() {
        let inner = output_type.get_inner_types();
        if inner.len() == 2 && inner.iter().all(|tipo| tipo.is_list()) {
            return Some(StateMachineAcceptance::AcceptsFailure);
        }
    }

    None
}

pub(super) fn validate_state_machine_output_schema(
    acceptance: StateMachineAcceptance,
    output_type: &Type,
    label_type: &Rc<Type>,
    event_type: &Rc<Type>,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
) -> Result<(), String> {
    let output_type = convert_opaque_type(&Rc::new(output_type.clone()), data_types, true);

    match acceptance {
        StateMachineAcceptance::AcceptsSuccess => {
            let output_inner_types = output_type.get_inner_types();
            let [output_event_type] = output_inner_types.as_slice() else {
                return Err(format!(
                    "state-machine success trace output '{}' must be '{}'",
                    pretty_print_type(output_type.as_ref()),
                    pretty_print_type(Type::list(event_type.clone()).as_ref()),
                ));
            };
            let output_event_type = convert_opaque_type(output_event_type, data_types, true);

            if output_event_type.as_ref() == event_type.as_ref() {
                Ok(())
            } else {
                Err(format!(
                    "state-machine success trace output '{}' must contain events of type '{}'",
                    pretty_print_type(output_type.as_ref()),
                    pretty_print_type(event_type.as_ref()),
                ))
            }
        }
        StateMachineAcceptance::AcceptsFailure => {
            let output_inner_types = output_type.get_inner_types();
            let [output_labels_type, output_events_type] = output_inner_types.as_slice() else {
                return Err(format!(
                    "state-machine failure trace output '{}' must be '{}'",
                    pretty_print_type(output_type.as_ref()),
                    pretty_print_type(
                        Type::tuple(vec![label_type.clone(), Type::list(event_type.clone())])
                            .as_ref(),
                    ),
                ));
            };

            let output_event_inner_types = output_events_type.get_inner_types();
            let [output_event_type] = output_event_inner_types.as_slice() else {
                return Err(format!(
                    "state-machine failure trace events output '{}' must be '{}'",
                    pretty_print_type(output_events_type.as_ref()),
                    pretty_print_type(Type::list(event_type.clone()).as_ref()),
                ));
            };

            let output_label_type = convert_opaque_type(output_labels_type, data_types, true);
            let output_event_type = convert_opaque_type(output_event_type, data_types, true);

            if output_label_type.as_ref() == label_type.as_ref()
                && output_event_type.as_ref() == event_type.as_ref()
            {
                Ok(())
            } else {
                Err(format!(
                    "state-machine failure trace output '{}' must contain labels of type '{}' and events of type '{}'",
                    pretty_print_type(output_type.as_ref()),
                    pretty_print_type(label_type.as_ref()),
                    pretty_print_type(event_type.as_ref()),
                ))
            }
        }
    }
}

pub(super) fn state_machine_trace_semantics_from_normalized(
    output_type: &Type,
    initial_state: &TypedExpr,
    step_function: &TypedExpr,
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<FuzzerSemantics> {
    let output_type = convert_opaque_type(&Rc::new(output_type.clone()), data_types, true);
    let args = make_synthetic_call_args(vec![initial_state.clone(), step_function.clone()]);

    extract_state_machine_trace_semantics_from_call(
        output_type.as_ref(),
        &args,
        data_types,
        function_index,
        constant_index,
        visiting_functions,
    )
}

pub(super) fn extract_state_machine_trace_semantics_from_call(
    output_type: &Type,
    args: &[CallArg<TypedExpr>],
    data_types: &IndexMap<&DataTypeKey, &TypedDataType>,
    function_index: &FunctionIndex<'_>,
    constant_index: &ConstantIndex<'_>,
    visiting_functions: &mut BTreeSet<(String, String)>,
) -> Option<FuzzerSemantics> {
    let acceptance = infer_state_machine_acceptance_from_output_type(output_type)?;

    if args.len() != 2 {
        return Some(opaque_semantics(format!(
            "state-machine trace lowering expects 2 arguments, got {}",
            args.len()
        )));
    }

    let state_type = convert_opaque_type(&args[0].value.tipo(), data_types, true);
    let step_type = convert_opaque_type(&args[1].value.tipo(), data_types, true);

    let Type::Fn {
        args: step_args,
        ret,
        ..
    } = step_type.as_ref()
    else {
        return Some(opaque_semantics(format!(
            "state-machine trace step argument is not a function, got '{}'",
            pretty_print_type(step_type.as_ref())
        )));
    };

    if step_args.is_empty() {
        return Some(opaque_semantics(
            "state-machine trace step function must take state as its first argument",
        ));
    }

    let step_state_type = convert_opaque_type(&step_args[0], data_types, true);
    if step_state_type.as_ref() != state_type.as_ref() {
        return Some(opaque_semantics(format!(
            "state-machine trace step state type '{}' does not match initial state type '{}'",
            pretty_print_type(step_state_type.as_ref()),
            pretty_print_type(state_type.as_ref())
        )));
    }

    let Some(transition_type) = extract_fuzzer_payload_type(ret.as_ref()) else {
        return Some(opaque_semantics(format!(
            "state-machine trace step return type '{}' is not Fuzzer<transition>",
            pretty_print_type(ret.as_ref())
        )));
    };
    let transition_type = convert_opaque_type(&transition_type, data_types, true);

    let (terminal_tag, step_tag, label_raw_type, event_raw_type, label_type, event_type) =
        match extract_state_machine_trace_fields(&transition_type, &state_type, data_types) {
            Ok(fields) => fields,
            Err(reason) => {
                return Some(opaque_semantics(format!(
                    "state-machine trace lowering cannot infer transition shape: {reason}"
                )));
            }
        };

    if let Err(reason) = validate_state_machine_output_schema(
        acceptance,
        output_type,
        &label_raw_type,
        &event_raw_type,
        data_types,
    ) {
        return Some(opaque_semantics(format!(
            "state-machine trace output schema does not match transition payloads: {reason}"
        )));
    }

    let step_input_raw_types: Vec<Rc<Type>> = step_args
        .iter()
        .skip(1)
        .map(|arg| convert_opaque_type(arg, data_types, true))
        .collect();
    let step_input_types: Vec<SemanticType> = step_input_raw_types
        .iter()
        .map(|tipo| semantic_type_from_type(tipo.as_ref()))
        .collect();
    let state_semantics = Box::new(default_semantics_for_type(state_type.as_ref(), data_types));
    let step_input_semantics = step_input_raw_types
        .iter()
        .map(|tipo| default_semantics_for_type(tipo.as_ref(), data_types))
        .collect();
    let label_semantics = Box::new(default_semantics_for_type(
        label_raw_type.as_ref(),
        data_types,
    ));
    let event_semantics = Box::new(default_semantics_for_type(
        event_raw_type.as_ref(),
        data_types,
    ));
    let transition_semantics = StateMachineTransitionSemantics {
        terminal_tag,
        step_tag,
        label_field_index: 0,
        next_state_field_index: 1,
        event_field_index: 2,
        state_semantics,
        step_input_semantics,
        label_semantics,
        event_semantics,
    };
    let output_semantics = Box::new(state_machine_trace_output_semantics(
        acceptance,
        label_raw_type.as_ref(),
        event_raw_type.as_ref(),
        data_types,
    ));
    if !state_machine_output_semantics_matches_acceptance(&acceptance, output_semantics.as_ref()) {
        return Some(opaque_semantics(format!(
            "state-machine trace output semantics '{}' do not match acceptance mode {}",
            describe_semantics(&output_semantics),
            describe_acceptance(&acceptance)
        )));
    }

    // For inline lambda step functions the expression is a `TypedExpr::Fn`
    // and we lower the body with the state argument bound to the theorem-side
    // `state` variable. For a named function reference (e.g.
    // `scenario.ok(initial_state, step)`) the expression is a `TypedExpr::Var`;
    // in that case we resolve the function body first and apply the same local
    // binding. Without this, named steps that reference their `state` parameter
    // degrade to out-of-scope `Var`s and freshen during Lean emission.
    let (step_function_ir, step_ir_unsupported_reason) = match &args[1].value {
        TypedExpr::Var { constructor, .. } => {
            if let ValueConstructorVariant::ModuleFn { module, name, .. } = &constructor.variant {
                let key = (module.clone(), name.clone());
                if !visiting_functions.insert(key.clone()) {
                    (None, None)
                } else {
                    let result = find_function(function_index, module, name)
                        .map(|function| {
                            if function.arguments.len() > 1 {
                                return (
                                    None,
                                    Some(
                                        "state-machine step functions with non-state parameters are not lowered faithfully yet"
                                            .to_string(),
                                    ),
                                );
                            }

                            let mut locals: BTreeMap<String, LocalBinding> = BTreeMap::new();
                            if let Some(state_arg) = function.arguments.first() {
                                locals.insert(
                                    state_arg.get_name(),
                                    LocalBinding::DrawnValue {
                                        lean_name: "state".to_string(),
                                        ty: shallow_ir_type(&state_arg.tipo),
                                        domain: FuzzerSemantics::Data,
                                    },
                                );
                            }
                            inject_module_constants(&mut locals, module, constant_index);


                            let mut visiting = BTreeSet::new();
                            let ir = typed_expr_to_shallow_ir_with_locals(
                                &function.body,
                                data_types,
                                &locals,
                                &mut visiting,
                            );
                            if shallow_ir_is_vacuous(&ir) {
                                (None, None)
                            } else {
                                (Some(ir), None)
                            }
                        })
                        .unwrap_or((None, None));
                    visiting_functions.remove(&key);
                    result
                }
            } else {
                (None, None)
            }
        }
        TypedExpr::Fn {
            args: step_args,
            body,
            ..
        } => {
            if step_args.len() > 1 {
                (
                    None,
                    Some(
                        "state-machine step functions with non-state parameters are not lowered faithfully yet"
                            .to_string(),
                    ),
                )
            } else {
                let mut locals: BTreeMap<String, LocalBinding> = BTreeMap::new();
                if let Some(state_arg) = step_args.first() {
                    locals.insert(
                        state_arg.get_name(),
                        LocalBinding::DrawnValue {
                            lean_name: "state".to_string(),
                            ty: shallow_ir_type(&state_arg.tipo),
                            domain: FuzzerSemantics::Data,
                        },
                    );
                }

                let mut visiting = BTreeSet::new();
                let ir = typed_expr_to_shallow_ir_with_locals(
                    body.as_ref(),
                    data_types,
                    &locals,
                    &mut visiting,
                );
                if shallow_ir_is_vacuous(&ir) {
                    (None, None)
                } else {
                    (Some(ir), None)
                }
            }
        }
        expr => {
            let ir = typed_expr_to_shallow_ir(expr, data_types);
            if shallow_ir_is_vacuous(&ir) {
                (None, None)
            } else {
                (Some(ir), None)
            }
        }
    };

    // S3 (infrastructure-only): attempt to translate the step function body
    // into a `TransitionProp`. The translation is deliberately conservative:
    // if normalization produces a `NormalizedFuzzer::Opaque` leaf (which is
    // common today because `normalize_fuzzer_from_expr` does not yet
    // recognise `return`/`and_then`/`fork*` in step-function bodies — that
    // lands in Issue S2), the resulting `TransitionProp` will be a single
    // `Unsupported` leaf. In that case we return `None` so downstream emission
    // is a no-op until S4 wires the predicate in. When S2 enriches the
    // normalizer, this field will start carrying meaningful structure without
    // further changes here.
    let transition_prop = transition_prop_from_step_function(
        &args[1].value,
        Some(&args[0].value),
        function_index,
        constant_index,
        data_types,
        visiting_functions,
    );

    // S4: capture the initial-state expression's ShallowIr so the Lean
    // emitter can pin `isValidTrace` to the concrete starting state
    // rather than an unconstrained existential.
    let initial_state_shallow_ir = {
        let ir = typed_expr_to_shallow_ir(&args[0].value, data_types);
        let ir = if shallow_ir_is_vacuous(&ir) {
            let empty_locals: BTreeMap<String, TypedExpr> = BTreeMap::new();
            resolve_function_with_applied_args(&args[0].value, "", function_index, &empty_locals)
                .map(|(resolved, resolved_locals, _)| {
                    let resolved_locals = resolved_locals
                        .into_iter()
                        .map(|(name, expr)| (name, LocalBinding::PureExpr(expr)))
                        .collect();
                    let mut visiting = BTreeSet::new();
                    typed_expr_to_shallow_ir_with_locals(
                        &resolved.function.body,
                        data_types,
                        &resolved_locals,
                        &mut visiting,
                    )
                })
                .unwrap_or(ir)
        } else {
            ir
        };
        if shallow_ir_is_vacuous(&ir) {
            None
        } else {
            Some(ir)
        }
    };

    Some(FuzzerSemantics::StateMachineTrace {
        acceptance,
        state_type: semantic_type_from_type(state_type.as_ref()),
        step_input_types,
        label_type,
        event_type,
        transition_semantics,
        output_semantics,
        step_function_ir,
        step_ir_unsupported_reason,
        transition_prop,
        initial_state_shallow_ir,
    })
}
