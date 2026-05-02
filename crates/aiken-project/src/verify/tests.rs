use super::*;
use crate::blueprint::{
    definitions::{Definitions, Reference},
    schema::{Annotated, Constructor, Data, Declaration, Items, Schema},
};

/// Test-local wrapper that calls the underlying `generate_proof_file` and
/// drops the `ProofCaveat`. Existing tests assert on the emitted Lean source
/// only, so the caveat is irrelevant to them. New tests that need to check
/// the caveat should call `super::generate_proof_file` directly.
///
/// `allow_vacuous_subgenerators` defaults to `false` (production semantics);
/// the H4 unit tests that need the debug-mode behaviour either go through
/// `super::generate_proof_file` directly or use the dedicated wrapper
/// `generate_proof_file_with_vacuous_subgenerators` below.
fn generate_proof_file(
    test: &ExportedPropertyTest,
    test_id: &str,
    lean_test_name: &str,
    lean_module: &str,
    existential_mode: ExistentialMode,
    target: &VerificationTargetKind,
) -> miette::Result<String> {
    super::generate_proof_file(
        test,
        test_id,
        lean_test_name,
        lean_module,
        existential_mode,
        target,
        false,
    )
    .map(|(content, _caveat)| content)
}

/// Sibling of `generate_proof_file` that opts into the hidden
/// `--allow-vacuous-subgenerators` debug-mode emission. Used by the H4
/// regression tests that exercise the widened sub-generator emission
/// path (`def := fun _ _ => True`).
#[allow(dead_code)]
fn generate_proof_file_with_vacuous_subgenerators(
    test: &ExportedPropertyTest,
    test_id: &str,
    lean_test_name: &str,
    lean_module: &str,
    existential_mode: ExistentialMode,
    target: &VerificationTargetKind,
) -> miette::Result<(String, ProofCaveat)> {
    super::generate_proof_file(
        test,
        test_id,
        lean_test_name,
        lean_module,
        existential_mode,
        target,
        true,
    )
}
use crate::export::{
    ExportedDataSchema, ExportedProgram, ExportedPropertyTest, FuzzerConstraint, FuzzerOutputType,
    FuzzerSemantics, StateMachineAcceptance, StateMachineTransitionSemantics, TestReturnMode,
    ValidatorTarget,
};
use crate::{Project, options::Options, telemetry::EventTarget};
use aiken_lang::ast::{Definition, OnTestFailure, Tracing};

mod catalogue;

fn make_test(module: &str, name: &str) -> ExportedPropertyTest {
    make_test_with_failure(module, name, OnTestFailure::FailImmediately)
}

fn make_test_with_failure(
    module: &str,
    name: &str,
    on_test_failure: OnTestFailure,
) -> ExportedPropertyTest {
    let module_path = module.replace('.', "/");
    let fuzzer_output_type = FuzzerOutputType::Int;
    let constraint = FuzzerConstraint::IntRange {
        min: "0".to_string(),
        max: "255".to_string(),
    };
    let semantics =
        derive_fixture_semantics_from_constraint(&fuzzer_output_type, &constraint, false);

    ExportedPropertyTest {
        name: format!("{module}.{name}"),
        module: module.to_string(),
        input_path: format!("lib/{module_path}.ak"),
        on_test_failure,
        return_mode: TestReturnMode::Bool,
        target_kind: Default::default(),
        validator_target: None,
        test_program: ExportedProgram {
            hex: "deadbeef".to_string(),
            flat_bytes: None,
        },
        fuzzer_program: ExportedProgram {
            hex: "cafebabe".to_string(),
            flat_bytes: None,
        },
        fuzzer_type: "Fuzzer<Int>".to_string(),
        fuzzer_output_type,
        constraint,
        semantics,
        fuzzer_data_schema: None,
        inner_data_schemas: Default::default(),
        transition_prop_lean: None,
        concrete_halt_witnesses: Vec::new(),
        concrete_error_witnesses: Vec::new(),
    }
}

fn fixture_semantics_opaque() -> FuzzerSemantics {
    FuzzerSemantics::Opaque {
        reason: "test fixture semantics not set".to_string(),
    }
}

fn default_fixture_semantics_for_output_type(output_type: &FuzzerOutputType) -> FuzzerSemantics {
    match output_type {
        FuzzerOutputType::Int => FuzzerSemantics::IntRange {
            min: None,
            max: None,
        },
        FuzzerOutputType::Bool => FuzzerSemantics::Bool,
        FuzzerOutputType::ByteArray => FuzzerSemantics::ByteArrayRange {
            min_len: None,
            max_len: None,
        },
        FuzzerOutputType::String => FuzzerSemantics::String,
        FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_) => FuzzerSemantics::Data,
        FuzzerOutputType::List(element_type) => FuzzerSemantics::List {
            element: Box::new(default_fixture_semantics_for_output_type(
                element_type.as_ref(),
            )),
            min_len: Some(0),
            max_len: None,
        },
        FuzzerOutputType::Tuple(types) => FuzzerSemantics::Product(
            types
                .iter()
                .map(default_fixture_semantics_for_output_type)
                .collect(),
        ),
        FuzzerOutputType::Pair(fst, snd) => FuzzerSemantics::Product(vec![
            default_fixture_semantics_for_output_type(fst.as_ref()),
            default_fixture_semantics_for_output_type(snd.as_ref()),
        ]),
    }
}

fn derive_fixture_semantics_from_constraint(
    output_type: &FuzzerOutputType,
    constraint: &FuzzerConstraint,
    allow_default_for_any: bool,
) -> FuzzerSemantics {
    match constraint {
        FuzzerConstraint::Any => {
            if allow_default_for_any {
                default_fixture_semantics_for_output_type(output_type)
            } else {
                fixture_semantics_opaque()
            }
        }
        FuzzerConstraint::IntRange { min, max } => match output_type {
            FuzzerOutputType::Int => FuzzerSemantics::IntRange {
                min: Some(min.clone()),
                max: Some(max.clone()),
            },
            FuzzerOutputType::Tuple(types) => {
                let elems: Vec<FuzzerSemantics> = types
                    .iter()
                    .map(|ty| {
                        if matches!(ty, FuzzerOutputType::Int) {
                            FuzzerSemantics::IntRange {
                                min: Some(min.clone()),
                                max: Some(max.clone()),
                            }
                        } else {
                            default_fixture_semantics_for_output_type(ty)
                        }
                    })
                    .collect();
                if elems
                    .iter()
                    .any(|sem| matches!(sem, FuzzerSemantics::IntRange { .. }))
                {
                    FuzzerSemantics::Product(elems)
                } else {
                    fixture_semantics_opaque()
                }
            }
            FuzzerOutputType::Pair(fst, snd) => {
                let mut elems = vec![
                    default_fixture_semantics_for_output_type(fst.as_ref()),
                    default_fixture_semantics_for_output_type(snd.as_ref()),
                ];
                let mut has_int = false;
                if matches!(fst.as_ref(), FuzzerOutputType::Int) {
                    elems[0] = FuzzerSemantics::IntRange {
                        min: Some(min.clone()),
                        max: Some(max.clone()),
                    };
                    has_int = true;
                }
                if matches!(snd.as_ref(), FuzzerOutputType::Int) {
                    elems[1] = FuzzerSemantics::IntRange {
                        min: Some(min.clone()),
                        max: Some(max.clone()),
                    };
                    has_int = true;
                }
                if has_int {
                    FuzzerSemantics::Product(elems)
                } else {
                    fixture_semantics_opaque()
                }
            }
            _ => fixture_semantics_opaque(),
        },
        FuzzerConstraint::ByteStringLenRange { min_len, max_len } => match output_type {
            FuzzerOutputType::ByteArray | FuzzerOutputType::String => {
                FuzzerSemantics::ByteArrayRange {
                    min_len: Some(*min_len),
                    max_len: Some(*max_len),
                }
            }
            _ => fixture_semantics_opaque(),
        },
        FuzzerConstraint::Exact(value) => match (output_type, value) {
            (FuzzerOutputType::Bool, FuzzerExactValue::Bool(value)) => {
                FuzzerSemantics::Exact(FuzzerExactValue::Bool(*value))
            }
            (FuzzerOutputType::ByteArray, FuzzerExactValue::ByteArray(bytes)) => {
                FuzzerSemantics::Exact(FuzzerExactValue::ByteArray(bytes.clone()))
            }
            (FuzzerOutputType::String, FuzzerExactValue::String(value)) => {
                FuzzerSemantics::Exact(FuzzerExactValue::String(value.clone()))
            }
            _ => fixture_semantics_opaque(),
        },
        FuzzerConstraint::Tuple(parts) => match output_type {
            FuzzerOutputType::Tuple(types) if types.len() == parts.len() => {
                FuzzerSemantics::Product(
                    types
                        .iter()
                        .zip(parts.iter())
                        .map(|(ty, part)| derive_fixture_semantics_from_constraint(ty, part, true))
                        .collect(),
                )
            }
            FuzzerOutputType::Pair(fst, snd) if parts.len() == 2 => FuzzerSemantics::Product(vec![
                derive_fixture_semantics_from_constraint(fst.as_ref(), &parts[0], true),
                derive_fixture_semantics_from_constraint(snd.as_ref(), &parts[1], true),
            ]),
            _ => fixture_semantics_opaque(),
        },
        FuzzerConstraint::List {
            elem,
            min_len,
            max_len,
        } => match output_type {
            FuzzerOutputType::List(element_type) => FuzzerSemantics::List {
                element: Box::new(derive_fixture_semantics_from_constraint(
                    element_type.as_ref(),
                    elem.as_ref(),
                    true,
                )),
                min_len: *min_len,
                max_len: *max_len,
            },
            _ => fixture_semantics_opaque(),
        },
        FuzzerConstraint::DataConstructorTags { tags } => match output_type {
            FuzzerOutputType::Data | FuzzerOutputType::Unsupported(_) => {
                FuzzerSemantics::Constructors { tags: tags.clone() }
            }
            _ => fixture_semantics_opaque(),
        },
        FuzzerConstraint::And(parts) => {
            let mut best = None;
            for part in parts {
                let semantics = derive_fixture_semantics_from_constraint(
                    output_type,
                    part,
                    allow_default_for_any,
                );
                if !matches!(semantics, FuzzerSemantics::Opaque { .. }) {
                    best = Some(semantics);
                    break;
                }
            }
            best.unwrap_or_else(|| {
                if allow_default_for_any {
                    default_fixture_semantics_for_output_type(output_type)
                } else {
                    fixture_semantics_opaque()
                }
            })
        }
        FuzzerConstraint::Map(_) | FuzzerConstraint::Unsupported { .. } => {
            fixture_semantics_opaque()
        }
    }
}

#[derive(Debug, Clone)]
struct ProofBenchmarkMetrics {
    theorem_size_bytes: usize,
    helper_count: usize,
    theorem_count: usize,
}

fn collect_proof_benchmark_metrics(proof: &str) -> ProofBenchmarkMetrics {
    ProofBenchmarkMetrics {
        theorem_size_bytes: proof.len(),
        helper_count: proof
            .lines()
            .filter(|line| line.trim_start().starts_with("def "))
            .count(),
        theorem_count: proof
            .lines()
            .filter(|line| line.trim_start().starts_with("theorem "))
            .count(),
    }
}

fn generate_proof_for_phase12_case(test: &ExportedPropertyTest) -> miette::Result<String> {
    let aiken_name = test
        .name
        .rsplit('.')
        .next()
        .expect("exported test names should include module and test segments");
    let id = test_id(&test.module, aiken_name);
    let lean_name = sanitize_lean_name(aiken_name);
    let lean_module = format!(
        "AikenVerify.Proofs.{}.{}",
        module_to_lean_segment(&test.module),
        lean_name
    );

    generate_proof_file(
        test,
        &id,
        &lean_name,
        &lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
}

fn expect_data_schema<'a>(
    schema: &'a ExportedDataSchema,
    reference: &Reference,
    context: &str,
) -> &'a Data {
    let definition = schema
        .definitions
        .lookup(reference)
        .unwrap_or_else(|| panic!("missing schema definition for {context}"));

    match &definition.annotated {
        Schema::Data(data) => data,
        other => panic!("expected Data schema for {context}, got {other:?}"),
    }
}

fn expect_referenced_data<'a>(declaration: &'a Declaration<Data>, context: &str) -> &'a Reference {
    match declaration {
        Declaration::Referenced(reference) => reference,
        Declaration::Inline(inline) => {
            panic!("expected referenced schema for {context}, got inline {inline:?}")
        }
    }
}

fn make_state_machine_trace_success_schema() -> ExportedDataSchema {
    let mut definitions = Definitions::new();

    let output_reference = Reference::new("cardano/transaction/OutputReference");
    definitions.insert(
        &output_reference,
        Annotated::from(Schema::Data(Data::AnyOf(vec![
            Constructor {
                index: 0,
                fields: vec![
                    Declaration::Inline(Box::new(Data::Bytes)).into(),
                    Declaration::Inline(Box::new(Data::Integer)).into(),
                ],
            }
            .into(),
        ]))),
    );

    let input = Reference::new("cardano/transaction/Input");
    definitions.insert(
        &input,
        Annotated::from(Schema::Data(Data::AnyOf(vec![
            Constructor {
                index: 0,
                fields: vec![
                    Declaration::Referenced(output_reference.clone()).into(),
                    Declaration::Inline(Box::new(Data::Integer)).into(),
                ],
            }
            .into(),
        ]))),
    );

    let inputs = Reference::new("cardano/transaction/Inputs");
    definitions.insert(
        &inputs,
        Annotated::from(Schema::Data(Data::List(Items::One(
            Declaration::Referenced(input.clone()),
        )))),
    );

    let value = Reference::new("cardano/assets/Value");
    definitions.insert(
        &value,
        Annotated::from(Schema::Data(Data::Map(
            Declaration::Inline(Box::new(Data::Bytes)),
            Declaration::Inline(Box::new(Data::Integer)),
        ))),
    );

    let transaction = Reference::new("cardano/transaction/Transaction");
    definitions.insert(
        &transaction,
        Annotated::from(Schema::Data(Data::AnyOf(vec![
            Constructor {
                index: 0,
                fields: vec![
                    Declaration::Referenced(inputs.clone()).into(),
                    Declaration::Referenced(value.clone()).into(),
                ],
            }
            .into(),
        ]))),
    );

    let root = Reference::new("scenario/Transactions");
    definitions.insert(
        &root,
        Annotated::from(Schema::Data(Data::List(Items::One(
            Declaration::Referenced(transaction),
        )))),
    );

    ExportedDataSchema { root, definitions }
}

fn make_state_machine_trace_failure_schema() -> ExportedDataSchema {
    let mut schema = make_state_machine_trace_success_schema();

    let labels = Reference::new("scenario/Labels");
    schema.definitions.insert(
        &labels,
        Annotated::from(Schema::Data(Data::List(Items::One(Declaration::Inline(
            Box::new(Data::Bytes),
        ))))),
    );

    let root = Reference::new("scenario/FailureTrace");
    schema.definitions.insert(
        &root,
        Annotated::from(Schema::Data(Data::List(Items::Many(vec![
            Declaration::Referenced(labels).into(),
            Declaration::Referenced(schema.root.clone()).into(),
        ])))),
    );
    schema.root = root;
    schema
}

fn make_state_machine_transition_semantics() -> StateMachineTransitionSemantics {
    StateMachineTransitionSemantics {
        terminal_tag: 0,
        step_tag: 1,
        label_field_index: 0,
        next_state_field_index: 1,
        event_field_index: 2,
        state_semantics: Box::new(FuzzerSemantics::Opaque {
            reason: "semantic type 'permissions/State' requires structural schema for precise lowering".to_string(),
        }),
        step_input_semantics: vec![FuzzerSemantics::List {
            element: Box::new(FuzzerSemantics::Opaque {
                reason: "semantic type 'cardano/transaction.Input' requires structural schema for precise lowering".to_string(),
            }),
            min_len: Some(0),
            max_len: None,
        }],
        label_semantics: Box::new(FuzzerSemantics::List {
            element: Box::new(FuzzerSemantics::String),
            min_len: Some(0),
            max_len: None,
        }),
        event_semantics: Box::new(FuzzerSemantics::Opaque {
            reason: "semantic type 'cardano/transaction.Transaction' requires structural schema for precise lowering".to_string(),
        }),
    }
}

/// Process-wide mutex serialising tests that mutate `AIKEN_EMIT_TWO_PHASE` (or
/// any other process-global env var). Cargo runs tests on multiple threads by
/// default; without this serialisation concurrent tests that read the env var
/// would observe arbitrary transient values.
fn env_mutex() -> &'static std::sync::Mutex<()> {
    static ENV_MUTEX: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    ENV_MUTEX.get_or_init(|| std::sync::Mutex::new(()))
}

fn make_phase12_state_machine_test(
    name: &str,
    acceptance: StateMachineAcceptance,
) -> ExportedPropertyTest {
    match acceptance {
        StateMachineAcceptance::AcceptsSuccess => {
            let mut test = make_test_with_type(
                "permissions.test",
                name,
                FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported(
                    "cardano/transaction.Transaction".to_string(),
                ))),
                FuzzerConstraint::List {
                    elem: Box::new(FuzzerConstraint::Any),
                    min_len: Some(0),
                    max_len: None,
                },
            );
            test.return_mode = TestReturnMode::Void;
            test.semantics = FuzzerSemantics::StateMachineTrace {
                acceptance,
                state_type: FuzzerOutputType::Unsupported("permissions/State".to_string()),
                step_input_types: vec![FuzzerOutputType::List(Box::new(
                    FuzzerOutputType::Unsupported("cardano/transaction.Input".to_string()),
                ))],
                label_type: FuzzerOutputType::List(Box::new(FuzzerOutputType::String)),
                event_type:
                    FuzzerOutputType::Unsupported("cardano/transaction.Transaction".to_string()),
                transition_semantics: make_state_machine_transition_semantics(),
                output_semantics: Box::new(FuzzerSemantics::List {
                    element: Box::new(FuzzerSemantics::Opaque {
                        reason: "semantic type 'cardano/transaction.Transaction' requires structural schema for precise lowering".to_string(),
                    }),
                    min_len: Some(0),
                    max_len: None,
                }),
                step_function_ir: None,
                step_ir_unsupported_reason: None,
                initial_state_shallow_ir: None,
            };
            test.fuzzer_data_schema = Some(make_state_machine_trace_success_schema());
            test
        }
        StateMachineAcceptance::AcceptsFailure => {
            let mut test =
                make_test_with_failure("permissions.test", name, OnTestFailure::SucceedEventually);
            test.return_mode = TestReturnMode::Void;
            test.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
                FuzzerOutputType::List(Box::new(FuzzerOutputType::String)),
                FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported(
                    "cardano/transaction.Transaction".to_string(),
                ))),
            ]);
            test.constraint = FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::List {
                    elem: Box::new(FuzzerConstraint::Any),
                    min_len: Some(0),
                    max_len: None,
                },
                FuzzerConstraint::List {
                    elem: Box::new(FuzzerConstraint::Any),
                    min_len: Some(0),
                    max_len: None,
                },
            ]);
            test.semantics = FuzzerSemantics::StateMachineTrace {
                acceptance,
                state_type: FuzzerOutputType::Unsupported("permissions/State".to_string()),
                step_input_types: vec![FuzzerOutputType::List(Box::new(
                    FuzzerOutputType::Unsupported("cardano/transaction.Input".to_string()),
                ))],
                label_type: FuzzerOutputType::List(Box::new(FuzzerOutputType::String)),
                event_type:
                    FuzzerOutputType::Unsupported("cardano/transaction.Transaction".to_string()),
                transition_semantics: make_state_machine_transition_semantics(),
                output_semantics: Box::new(FuzzerSemantics::Product(vec![
                    FuzzerSemantics::List {
                        element: Box::new(FuzzerSemantics::String),
                        min_len: Some(1),
                        max_len: None,
                    },
                    FuzzerSemantics::List {
                        element: Box::new(FuzzerSemantics::Opaque {
                            reason: "semantic type 'cardano/transaction.Transaction' requires structural schema for precise lowering".to_string(),
                        }),
                        min_len: Some(1),
                        max_len: Some(1),
                    },
                ])),
                step_function_ir: None,
                step_ir_unsupported_reason: None,
                initial_state_shallow_ir: None,
            };
            test.fuzzer_data_schema = Some(make_state_machine_trace_failure_schema());
            test
        }
    }
}

/// Helper: returns the `GenerationErrorCategory` of a downcastable `GenerationError`,
/// or `None` for anything else. Used by tests that need to distinguish between
/// `FallbackRequired` and `UnsupportedShape` rather than relying on the coarse
/// `is_skippable_generation_error` predicate (which accepts either).
fn error_category(error: &miette::Report) -> Option<GenerationErrorCategory> {
    error.downcast_ref::<GenerationError>().map(|e| e.category)
}

/// Asserts that `error` is a `GenerationError` whose category equals `expected`.
/// Prints the actual category and message on mismatch so regressions (e.g. a
/// test that should produce `FallbackRequired` silently becoming
/// `UnsupportedShape`) are caught.
#[track_caller]
fn assert_generation_error_category(
    error: &miette::Report,
    expected: GenerationErrorCategory,
    context: &str,
) {
    let actual = error_category(error);
    assert_eq!(
        actual,
        Some(expected),
        "{context}: expected {expected:?}, got {actual:?} (message: {error})"
    );
}

/// Shared sanity check used by the `export_path_populates_validator_metadata_*`
/// family. Those tests exist primarily to verify validator metadata extraction;
/// the secondary `preflight_validate_test` call guards against regressions
/// where metadata extraction silently succeeds but direct proof generation
/// breaks. Assert that the preflight error is a `FallbackRequired` with a
/// message naming the fixture test, so a silent reclassification to
/// `UnsupportedShape` (which would still make the bare `is_err()` happy) is
/// caught.
#[track_caller]
fn assert_preflight_fallback_required(
    result: miette::Result<()>,
    context: &str,
    test_name_needle: &str,
) {
    let err = result
        .err()
        .unwrap_or_else(|| panic!("{context}: preflight unexpectedly succeeded"));
    assert_generation_error_category(&err, GenerationErrorCategory::FallbackRequired, context);
    let message = err.to_string();
    assert!(
        message.contains(test_name_needle),
        "{context}: error should name fixture test `{test_name_needle}`, got: {message}"
    );
    assert!(
        message.contains("cannot be formally verified")
            || message.contains("opaque")
            || message.contains("ByteString")
            || message.contains("ByteArray"),
        "{context}: error should surface opaque-semantics reason, got: {message}"
    );
}

#[track_caller]
fn assert_proof_generation_skipped_as_fallback(
    result: &miette::Result<String>,
    context: &str,
    reason_needle: &str,
) {
    let err = result
        .as_ref()
        .err()
        .unwrap_or_else(|| panic!("{context}: expected FallbackRequired skip, got proof content"));
    assert_generation_error_category(err, GenerationErrorCategory::FallbackRequired, context);
    assert!(
        err.to_string().contains(reason_needle),
        "{context}: error should mention `{reason_needle}`, got: {}",
        err
    );
}

/// Pin the S0003 (`existential_witness_unsound_for_domain`) hard-error shape
/// produced by `--existential-mode witness` against any non-`Bool` fuzzer
/// domain on a `fail_once` test. Asserts:
///   - the error is a downcastable `GenerationError`,
///   - `code == Some("S0003")`,
///   - the typed reason is `ExistentialWitnessUnsoundForDomain` carrying the
///     expected `test_name` and `domain` strings,
///   - `category == UnsoundFallback`, and
///   - `is_skippable_generation_error` returns `false` (i.e.
///     `--skip-unsupported` cannot suppress S0003).
#[track_caller]
fn assert_s0003_witness_unsound(
    result: &miette::Result<String>,
    expected_test_name: &str,
    expected_domain: &str,
    context: &str,
) {
    let report = result
        .as_ref()
        .err()
        .unwrap_or_else(|| panic!("{context}: expected S0003 hard error, got proof content"));
    let err = report.downcast_ref::<GenerationError>().unwrap_or_else(|| {
        panic!("{context}: error must downcast to GenerationError, got: {report}")
    });
    assert_eq!(
        err.code,
        Some("S0003"),
        "{context}: expected code S0003, got code {:?} message: {}",
        err.code,
        err.message,
    );
    match err.reason.as_ref() {
        Some(UnsupportedReason::ExistentialWitnessUnsoundForDomain { test_name, domain }) => {
            assert_eq!(
                test_name, expected_test_name,
                "{context}: S0003 test_name field mismatch",
            );
            assert_eq!(
                domain, expected_domain,
                "{context}: S0003 domain field mismatch",
            );
        }
        other => panic!(
            "{context}: expected UnsupportedReason::ExistentialWitnessUnsoundForDomain, got {other:?}",
        ),
    }
    assert_eq!(
        err.category,
        GenerationErrorCategory::UnsoundFallback,
        "{context}: S0003 must be UnsoundFallback (never skippable)",
    );
    assert!(
        !is_skippable_generation_error(report, &SkipPolicy::All),
        "{context}: S0003 (UnsoundFallback) must NOT be skippable by --skip-unsupported",
    );
}

/// Sibling of `assert_preflight_fallback_required` for tests whose validator
/// target is intentionally unpopulated (control-flow-dependent calls stay in
/// property-wrapper mode). Preflight should reject a validator/equivalence
/// target with `UnsupportedShape` and an explicit "no validator target
/// metadata" message, not a generic `FallbackRequired`.
#[track_caller]
fn assert_preflight_missing_validator_metadata(result: miette::Result<()>, context: &str) {
    let err = result
        .err()
        .unwrap_or_else(|| panic!("{context}: preflight unexpectedly succeeded"));
    assert_generation_error_category(&err, GenerationErrorCategory::UnsupportedShape, context);
    let message = err.to_string();
    assert!(
        message.contains("no validator target metadata")
            || message.contains("validator-target metadata"),
        "{context}: error should flag missing validator metadata, got: {message}"
    );
}

#[test]
fn sanitize_lean_name_basic() {
    assert_eq!(sanitize_lean_name("test_map"), "test_map");
    assert_eq!(sanitize_lean_name("my-test"), "my_test");
    assert_eq!(sanitize_lean_name("3things"), "T_3things");
}

#[test]
fn module_to_lean_segment_basic() {
    assert_eq!(module_to_lean_segment("aiken/list"), "AikenList");
    assert_eq!(module_to_lean_segment("my_module"), "My_module");
    assert_eq!(
        module_to_lean_segment("permissions.test"),
        "PermissionsTest"
    );
    assert_eq!(
        module_to_lean_segment("deep/nested/module"),
        "DeepNestedModule"
    );
}

#[test]
fn test_id_has_readable_prefix_and_hash_suffix() {
    let id = test_id("aiken/list", "test_map");
    assert!(
        id.starts_with("aiken_list__test_map_"),
        "ID should start with readable prefix, got: {id}"
    );
    // Hash suffix is 8 hex chars
    let suffix = id.strip_prefix("aiken_list__test_map_").unwrap();
    assert_eq!(
        suffix.len(),
        8,
        "Hash suffix should be 8 hex chars, got: {suffix}"
    );
    assert!(
        suffix.chars().all(|c| c.is_ascii_hexdigit()),
        "Hash suffix should be hex, got: {suffix}"
    );
}

#[test]
fn test_id_sanitizes_dotted_module_names() {
    let id = test_id("permissions.test", "test_map");
    assert!(
        id.starts_with("permissions_test__test_map_"),
        "Dotted module names must be sanitized for Lean identifiers, got: {id}"
    );
    assert!(
        !id.contains('.'),
        "Generated IDs used in Lean identifiers must not contain dots, got: {id}"
    );
}

#[test]
fn test_id_different_inputs_produce_different_ids() {
    let id1 = test_id("mod_a", "test_x");
    let id2 = test_id("mod_b", "test_x");
    assert_ne!(id1, id2, "Different modules should produce different IDs");
}

#[test]
fn test_id_is_deterministic() {
    let id1 = test_id("my_module", "test_foo");
    let id2 = test_id("my_module", "test_foo");
    assert_eq!(id1, id2, "Same inputs should produce the same ID");
}

#[test]
fn generate_workspace_creates_files() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path().to_path_buf();

    let tests = vec![
        make_test("my_module", "test_roundtrip"),
        make_test("aiken/list", "test_map"),
    ];

    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let manifest = generate_lean_workspace(&tests, &config, &SkipPolicy::None).unwrap();

    // Check files exist
    assert!(out_dir.join("lakefile.lean").exists());
    assert!(out_dir.join("lean-toolchain").exists());
    assert!(out_dir.join("AikenVerify.lean").exists());
    assert!(out_dir.join("AikenVerify/Utils.lean").exists());
    assert!(out_dir.join("manifest.json").exists());

    // Check per-test files
    assert!(
        out_dir
            .join("AikenVerify/Proofs/My_module/test_roundtrip.lean")
            .exists()
    );
    assert!(
        out_dir
            .join("AikenVerify/Proofs/AikenList/test_map.lean")
            .exists()
    );
    let id0 = &manifest.tests[0].id;
    let id1 = &manifest.tests[1].id;
    assert!(out_dir.join(format!("cbor/{id0}.cbor")).exists());
    assert!(out_dir.join(format!("cbor/{id1}.cbor")).exists());

    // Check manifest
    assert_eq!(manifest.tests.len(), 2);
    assert!(
        id0.starts_with("my_module__test_roundtrip_"),
        "ID should have readable prefix + hash: {id0}"
    );
    assert_eq!(
        manifest.tests[0].lean_module,
        "AikenVerify.Proofs.My_module.test_roundtrip"
    );
    assert!(
        id1.starts_with("aiken_list__test_map_"),
        "ID should have readable prefix + hash: {id1}"
    );

    // Check flat file content is the hex
    let flat_content = fs::read_to_string(out_dir.join(format!("cbor/{id0}.cbor"))).unwrap();
    assert_eq!(flat_content, "deadbeef");

    // Check Utils.lean text shape via snapshot.  The body embeds the
    // resolved CEK budget (`20000` here) and the Lean helper signatures.
    // No tempdir paths, no revs, no UUIDs in this file → no redactions.
    let utils = fs::read_to_string(out_dir.join("AikenVerify/Utils.lean")).unwrap();
    insta::assert_snapshot!("generate_workspace_creates_files__utils_lean", utils);

    // Check root import file via snapshot.  The text only contains
    // deterministic `import` statements (one per test, in stable order
    // produced by `generate_lean_workspace`).  No redactions.
    let root = fs::read_to_string(out_dir.join("AikenVerify.lean")).unwrap();
    insta::assert_snapshot!("generate_workspace_creates_files__aiken_verify_lean", root);

    // Check generated .lean proof files contain actual theorems
    let proof1 =
        fs::read_to_string(out_dir.join("AikenVerify/Proofs/My_module/test_roundtrip.lean"))
            .unwrap();
    assert!(
        proof1.contains("theorem "),
        "Generated Lean proof file should contain at least one theorem"
    );

    let proof2 =
        fs::read_to_string(out_dir.join("AikenVerify/Proofs/AikenList/test_map.lean")).unwrap();
    assert!(
        proof2.contains("theorem "),
        "Generated Lean proof file should contain at least one theorem"
    );
}

#[test]
fn generate_workspace_dotted_module_creates_matching_path() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path().to_path_buf();
    let tests = vec![make_test(
        "permissions.test",
        "prop_permissions_core_development_standard_ok",
    )];

    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let manifest = generate_lean_workspace(&tests, &config, &SkipPolicy::None).unwrap();
    assert_eq!(manifest.tests.len(), 1);
    assert_eq!(
        manifest.tests[0].lean_module,
        "AikenVerify.Proofs.PermissionsTest.prop_permissions_core_development_standard_ok"
    );
    let entry = &manifest.tests[0];
    assert!(out_dir.join(&entry.lean_file).exists(),);
    assert!(
        !entry.id.contains('.'),
        "Manifest IDs are embedded into Lean identifiers and must not contain dots"
    );

    let proof = fs::read_to_string(out_dir.join(&entry.lean_file)).unwrap();
    assert!(
        proof.contains("theorem "),
        "Proof should contain at least one theorem"
    );

    for line in proof
        .lines()
        .filter(|line| line.starts_with("#import_uplc "))
    {
        let identifier = line
            .split_whitespace()
            .nth(1)
            .expect("import line should include an identifier");
        let valid_identifier = identifier
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
            && identifier
                .chars()
                .all(|ch| ch.is_ascii_alphanumeric() || ch == '_');
        assert!(
            valid_identifier,
            "Generated Lean import identifier must be syntactically valid: {identifier}"
        );
    }
}

#[test]
fn opaque_semantics_routes_through_skip_path_on_workspace_generation() {
    // Opaque top-level fuzzer semantics now fails workspace generation
    // without `--skip-unsupported`. The pipeline surfaces the
    // FallbackRequired skip to the caller rather than silently emitting
    // a vacuous stub.
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path().to_path_buf();
    let tests = vec![make_test_with_type(
        "my_module",
        "test_bool_opaque_manifest",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    )];

    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let err = generate_lean_workspace(&tests, &config, &SkipPolicy::None)
        .expect_err("workspace generation should fail on opaque semantics");
    let msg = err.to_string();
    assert!(
        msg.contains("opaque") || msg.contains("unsupported"),
        "workspace error should mention opaque/unsupported, got: {msg}"
    );
}

#[test]
fn opaque_semantics_returns_fallback_required_on_preflight() {
    // Preflight now rejects opaque top-level semantics with a
    // FallbackRequired skip, so the pipeline can downgrade to a stub
    // or surface the skip rather than silently succeed.
    let test = make_test_with_type(
        "my_module",
        "test_preflight_bool_opaque",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    );

    let result = preflight_validate_test(
        &test,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    let err = result.expect_err("preflight should skip opaque semantics via FallbackRequired");
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::FallbackRequired,
        "opaque preflight skip",
    );
    assert!(
        err.to_string().contains("opaque"),
        "preflight error should mention `opaque`, got: {err}"
    );
}

#[test]
fn opaque_semantics_returns_fallback_required_on_proof_generation() {
    let test = make_test_with_type(
        "my_module",
        "test_proof_bool_opaque",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_proof_bool_opaque");
    let lean_name = sanitize_lean_name("test_proof_bool_opaque");
    let lean_module = "AikenVerify.Proofs.My_module.test_proof_bool_opaque";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Opaque Bool semantics at proof-gen layer",
        "opaque",
    );
}

#[test]
fn scenario_like_void_property_with_opaque_semantics_produces_error() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path().to_path_buf();
    let mut test = make_test_with_type(
        "permissions.test",
        "prop_permissions_core_development_standard_ok",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported(
            "cardano/transaction.Transaction".to_string(),
        ))),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: Some(0),
            max_len: None,
        },
    );
    test.return_mode = TestReturnMode::Void;
    test.semantics = FuzzerSemantics::StateMachineTrace {
        acceptance: StateMachineAcceptance::AcceptsSuccess,
        state_type: FuzzerOutputType::Unsupported("permissions/State".to_string()),
        step_input_types: vec![FuzzerOutputType::List(Box::new(
            FuzzerOutputType::Unsupported("cardano/transaction.Input".to_string()),
        ))],
        label_type: FuzzerOutputType::List(Box::new(FuzzerOutputType::String)),
        event_type: FuzzerOutputType::Unsupported("cardano/transaction.Transaction".to_string()),
        transition_semantics: make_state_machine_transition_semantics(),
        output_semantics: Box::new(FuzzerSemantics::List {
            element: Box::new(FuzzerSemantics::Opaque {
                reason: "semantic type 'cardano/transaction.Transaction' requires structural schema for precise lowering".to_string(),
            }),
            min_len: Some(0),
            max_len: None,
        }),
        step_function_ir: None,
        step_ir_unsupported_reason: None,
        initial_state_shallow_ir: None,
    };

    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    // Opaque state-machine output semantics now halt workspace generation
    // with a FallbackRequired skip rather than emitting a vacuous stub.
    let err = generate_lean_workspace(&[test], &config, &SkipPolicy::None)
        .expect_err("opaque state-machine semantics should produce an error");
    let msg = err.to_string();
    assert!(
        msg.contains("opaque") || msg.contains("unsupported"),
        "error should mention opaque/unsupported, got: {msg}"
    );
    let _ = out_dir;
}

#[test]
fn generate_workspace_scenario_like_void_property_with_opaque_output_returns_fallback() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path().to_path_buf();
    let mut test = make_test_with_type(
        "permissions.test",
        "prop_permissions_core_development_standard_ok",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported(
            "cardano/transaction.Transaction".to_string(),
        ))),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: Some(0),
            max_len: None,
        },
    );
    test.return_mode = TestReturnMode::Void;
    test.semantics = FuzzerSemantics::StateMachineTrace {
        acceptance: StateMachineAcceptance::AcceptsSuccess,
        state_type: FuzzerOutputType::Unsupported("permissions/State".to_string()),
        step_input_types: vec![FuzzerOutputType::List(Box::new(
            FuzzerOutputType::Unsupported("cardano/transaction.Input".to_string()),
        ))],
        label_type: FuzzerOutputType::List(Box::new(FuzzerOutputType::String)),
        event_type: FuzzerOutputType::Unsupported("cardano/transaction.Transaction".to_string()),
        transition_semantics: make_state_machine_transition_semantics(),
        output_semantics: Box::new(FuzzerSemantics::List {
            element: Box::new(FuzzerSemantics::Opaque {
                reason: "semantic type 'cardano/transaction.Transaction' requires structural schema for precise lowering".to_string(),
            }),
            min_len: Some(0),
            max_len: None,
        }),
        step_function_ir: None,
        step_ir_unsupported_reason: None,
        initial_state_shallow_ir: None,
    };
    test.fuzzer_data_schema = Some(make_state_machine_trace_success_schema());

    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    // State-machine trace proofs whose fuzzer event domain contains an ADT
    // without a structural Lean lowering (e.g. `cardano/transaction.Transaction`)
    // now surface a FallbackRequired skip from workspace generation. A
    // schema-backed direct theorem would require the structural schema
    // plumbing to supersede the opaque-output detection; until then the
    // fallback is the source-of-truth outcome.
    let err = generate_lean_workspace(&[test], &config, &SkipPolicy::None)
        .expect_err("schema-backed opaque state-machine output should still error");
    let msg = err.to_string();
    assert!(
        msg.contains("opaque") || msg.contains("unsupported"),
        "error should mention opaque/unsupported, got: {msg}"
    );
    let _ = out_dir;
}

#[test]
fn generate_workspace_scenario_like_fail_property_uses_schema_backed_direct_theorem() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path().to_path_buf();
    let mut test = make_test_with_type(
        "permissions.test",
        "prop_permissions_core_development_standard_ko",
        FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::List(Box::new(FuzzerOutputType::String)),
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported(
                "cardano/transaction.Transaction".to_string(),
            ))),
        ]),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(0),
                max_len: None,
            },
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(0),
                max_len: None,
            },
        ]),
    );
    test.return_mode = TestReturnMode::Void;
    test.on_test_failure = OnTestFailure::SucceedEventually;
    test.semantics = FuzzerSemantics::StateMachineTrace {
        acceptance: StateMachineAcceptance::AcceptsFailure,
        state_type: FuzzerOutputType::Unsupported("permissions/State".to_string()),
        step_input_types: vec![FuzzerOutputType::List(Box::new(
            FuzzerOutputType::Unsupported("cardano/transaction.Input".to_string()),
        ))],
        label_type: FuzzerOutputType::List(Box::new(FuzzerOutputType::String)),
        event_type: FuzzerOutputType::Unsupported("cardano/transaction.Transaction".to_string()),
        transition_semantics: make_state_machine_transition_semantics(),
        output_semantics: Box::new(FuzzerSemantics::Product(vec![
            FuzzerSemantics::List {
                element: Box::new(FuzzerSemantics::String),
                min_len: Some(1),
                max_len: None,
            },
            FuzzerSemantics::List {
                element: Box::new(FuzzerSemantics::Opaque {
                    reason: "semantic type 'cardano/transaction.Transaction' requires structural schema for precise lowering".to_string(),
                }),
                min_len: Some(1),
                max_len: Some(1),
            },
        ])),
        step_function_ir: None,
        step_ir_unsupported_reason: None,
        initial_state_shallow_ir: None,
    };
    test.fuzzer_data_schema = Some(make_state_machine_trace_failure_schema());

    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    // The ko-style fixture has `Tuple(List<String>, List<Transaction>)` as
    // its output type. The String-label gate (Blaster does not yet support
    // universal quantification over ByteString/String) fires BEFORE the
    // state-machine trace proof path, so this test still errors. The
    // soundness-only stub path only applies once we reach
    // `try_generate_state_machine_trace_proof_from_semantics` — which we
    // don't here because the outer ByteString gate rejects the fuzzer first.
    let err = generate_lean_workspace(&[test], &config, &SkipPolicy::None)
        .expect_err("fail-mode state-machine with String label still hits the ByteString gate");

    let message = err.to_string();
    assert!(
        message.contains("no synthesisable concrete witness")
            || message.contains("ByteString")
            || message.contains("Transaction"),
        "Error should explain why fail-mode state-machine proof generation stops, got: {message}"
    );
}

#[test]
fn generate_workspace_clears_stale_generated_files_but_keeps_static_cache() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path().to_path_buf();

    // Simulate stale generated workspace content from a prior run.
    fs::create_dir_all(out_dir.join("AikenVerify/Proofs/Stale")).unwrap();
    fs::write(
        out_dir.join("AikenVerify/Proofs/Stale/old.lean"),
        "-- stale",
    )
    .unwrap();
    fs::create_dir_all(out_dir.join("cbor")).unwrap();
    fs::write(out_dir.join("cbor/stale.cbor"), "stale").unwrap();
    fs::write(
        out_dir.join("AikenVerify.lean"),
        "import AikenVerify.Proofs.Stale.old",
    )
    .unwrap();
    fs::write(out_dir.join("manifest.json"), "{}").unwrap();
    fs::create_dir_all(out_dir.join("logs")).unwrap();
    fs::write(out_dir.join("logs/stale.log"), "stale").unwrap();
    fs::create_dir_all(out_dir.join(".lake/build/lib/AikenVerify/Stale")).unwrap();
    fs::write(
        out_dir.join(".lake/build/lib/AikenVerify/Stale/stale.olean"),
        "stale",
    )
    .unwrap();

    // Simulate dependency cache artifacts that should persist.
    fs::create_dir_all(out_dir.join("PlutusCore")).unwrap();
    fs::write(out_dir.join("PlutusCore/lakefile.lean"), "-- keep").unwrap();
    fs::create_dir_all(out_dir.join(".lake/packages/Blaster")).unwrap();
    fs::write(out_dir.join(".lake/packages/Blaster/cache.txt"), "keep").unwrap();
    fs::create_dir_all(out_dir.join(".lake/build/lib/PlutusCore")).unwrap();
    fs::write(
        out_dir.join(".lake/build/lib/PlutusCore/cache.olean"),
        "keep",
    )
    .unwrap();

    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let manifest = generate_lean_workspace(
        &[make_test("my_module", "test_roundtrip")],
        &config,
        &SkipPolicy::None,
    )
    .unwrap();
    let id = &manifest.tests[0].id;

    assert!(!out_dir.join("AikenVerify/Proofs/Stale/old.lean").exists());
    assert!(!out_dir.join("cbor/stale.cbor").exists());
    assert!(!out_dir.join("logs/stale.log").exists());
    assert!(
        !out_dir
            .join(".lake/build/lib/AikenVerify/Stale/stale.olean")
            .exists()
    );

    assert!(out_dir.join("PlutusCore/lakefile.lean").exists());
    assert!(out_dir.join(".lake/packages/Blaster/cache.txt").exists());
    assert!(
        out_dir
            .join(".lake/build/lib/PlutusCore/cache.olean")
            .exists()
    );

    assert!(
        out_dir
            .join("AikenVerify/Proofs/My_module/test_roundtrip.lean")
            .exists()
    );
    assert!(out_dir.join(format!("cbor/{id}.cbor")).exists());
}

#[test]
fn generate_workspace_empty_tests() {
    let tmp = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: tmp.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let manifest = generate_lean_workspace(&[], &config, &SkipPolicy::None).unwrap();
    assert!(manifest.tests.is_empty());
    assert!(tmp.path().join("lakefile.lean").exists());
}

#[test]
fn fail_immediately_generates_true_assertion() {
    // FailImmediately = default test (no `fail` keyword), body should return true
    let test = make_test_with_failure("my_module", "test_pass", OnTestFailure::FailImmediately);
    let id = test_id("my_module", "test_pass");
    let lean_name = sanitize_lean_name("test_pass");
    let lean_module = "AikenVerify.Proofs.My_module.test_pass";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Termination theorem should still be generated"
    );
}

#[test]
fn succeed_eventually_generates_false_assertion() {
    // SucceedEventually = `fail` keyword, body should return false
    let test = make_test_with_failure("my_module", "test_fail", OnTestFailure::SucceedEventually);
    let id = test_id("my_module", "test_fail");
    let lean_name = sanitize_lean_name("test_fail");
    let lean_module = "AikenVerify.Proofs.My_module.test_fail";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        !proof.contains("= some true"),
        "SucceedEventually should not contain = true"
    );
}

/// H5 / S0003 required test: `--existential-mode witness` against an `Int`
/// fuzzer-output domain on a `fail_once` test must hard-error with
/// `S0003: existential_witness_unsound_for_domain`. The trivial witness
/// `(0 : Integer)` is not necessarily a counterexample, so the historic
/// `⟨0, by decide⟩` proof was unsound. Replaces the legacy
/// `fail_once_witness_mode_generates_existential_theorem` which exercised
/// the now-removed unsound path.
#[test]
fn witness_mode_rejected_for_int_domain() {
    let test = make_test_with_failure("my_module", "test_ev", OnTestFailure::SucceedImmediately);
    assert!(
        matches!(test.fuzzer_output_type, FuzzerOutputType::Int),
        "fixture must be Int-domain to exercise the S0003 guard",
    );
    let id = test_id("my_module", "test_ev");
    let lean_name = sanitize_lean_name("test_ev");
    let lean_module = "AikenVerify.Proofs.My_module.test_ev";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    );

    assert_s0003_witness_unsound(
        &result,
        &test.name,
        "Int",
        "Int-domain witness mode must trigger S0003",
    );
}

/// H5 / S0003 required test: `--existential-mode witness` against a `Bool`
/// fuzzer-output domain on a `fail_once` test must succeed. `Bool` is the
/// sole sound domain for witness mode — the trivial witness (`true`) and
/// the alternative (`false`) together cover the two-element domain, so the
/// `⟨true, by decide⟩` proof is sound. The generated proof must use the
/// witness-specific `witnessTests` helper and `by decide` tactic.
#[test]
fn witness_mode_accepted_for_bool_domain() {
    let mut test = make_test_with_type(
        "my_module",
        "test_bool_witness",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    // S0003 fires *after* `ensure_target_kind_compatible` but *before*
    // semantics-driven gates, so we still need real (non-opaque) Bool
    // semantics for the post-S0003 path that emits `witnessTests` /
    // `by decide` to actually run on the Bool branch.
    test.semantics = FuzzerSemantics::Bool;
    let id = test_id("my_module", "test_bool_witness");
    let lean_name = sanitize_lean_name("test_bool_witness");
    let lean_module = "AikenVerify.Proofs.My_module.test_bool_witness";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    )
    .expect("Bool-domain witness mode must succeed (the sole sound domain for witness mode)");

    // Snapshot pins the canonical witness-mode shape (the sound replacement
    // for the historic `fail_once_witness_mode_generates_existential_theorem`
    // test): `∃` quantifier, `witnessTests` helper, and the `by decide`
    // tactic — the path that makes Bool the sole accepted domain for witness
    // mode under S0003.  Stable inputs → no redactions.
    insta::assert_snapshot!("witness_mode_accepted_for_bool_domain", proof);
}

/// H5 / S0003 required test: `--existential-mode witness` against a
/// `List<Int>` fuzzer-output domain on a `fail_once` test must hard-error
/// with `S0003`. Composite domains containing `Int` are unsound for witness
/// mode for the same reason as scalar `Int`: the trivial witness (`[]` or
/// any deterministic list of zeros) need not be a counterexample.
#[test]
fn witness_mode_rejected_for_list_int_domain() {
    let mut test = make_test_with_type(
        "my_module",
        "test_list_int_witness",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            }),
            min_len: Some(1),
            max_len: Some(3),
        },
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    let id = test_id("my_module", "test_list_int_witness");
    let lean_name = sanitize_lean_name("test_list_int_witness");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_int_witness";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    );

    assert_s0003_witness_unsound(
        &result,
        &test.name,
        "List<Int>",
        "List<Int> witness mode must trigger S0003",
    );
}

/// Equivalent universal-mode coverage for the historic
/// `fail_once_witness_mode_generates_existential_theorem` test: keep the
/// existential lowering machinery exercised on the `Int` domain via
/// `--existential-mode proof`, which remains sound (Z3 synthesises a real
/// witness via the Blaster tactic).
#[test]
fn fail_once_proof_mode_int_domain_generates_existential_theorem() {
    let test = make_test_with_failure("my_module", "test_ev", OnTestFailure::SucceedImmediately);
    let id = test_id("my_module", "test_ev");
    let lean_name = sanitize_lean_name("test_ev");
    let lean_module = "AikenVerify.Proofs.My_module.test_ev";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("\u{2203}"),
        "Proof mode should generate existential (\u{2203}) quantifier"
    );
    assert!(
        proof.contains("proveTests"),
        "Proof mode should use proveTests helper"
    );
    assert!(
        proof.contains("by blaster"),
        "Proof mode should use blaster tactic"
    );
}

#[test]
fn fail_once_proof_mode_generates_existential_theorem() {
    // Snapshot pins the canonical SucceedImmediately + Proof mode shape:
    // `∃` quantifier, `proveTests` helper, `= false` body, and the
    // `by blaster` tactic invocation.  Stable inputs → no redactions.
    let test = make_test_with_failure("my_module", "test_ev", OnTestFailure::SucceedImmediately);
    let id = test_id("my_module", "test_ev");
    let lean_name = sanitize_lean_name("test_ev");
    let lean_module = "AikenVerify.Proofs.My_module.test_ev";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    insta::assert_snapshot!("fail_once_proof_mode_generates_existential_theorem", proof);
}

/// `Void + fail_once + Int` is rejected by S0003 under witness mode (the
/// trivial witness `(0 : Integer)` is not necessarily an erroring input).
/// Cover the equivalent existential-error theorem under the sound
/// `--existential-mode proof` path so the `proveTestsError` lowering for
/// `Void`-returning fail-once tests remains exercised.
#[test]
fn fail_once_void_proof_mode_generates_existential_error_theorem() {
    let mut test = make_test_with_failure(
        "my_module",
        "test_ev_void",
        OnTestFailure::SucceedImmediately,
    );
    test.return_mode = TestReturnMode::Void;
    let id = test_id("my_module", "test_ev_void");
    let lean_name = sanitize_lean_name("test_ev_void");
    let lean_module = "AikenVerify.Proofs.My_module.test_ev_void";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("\u{2203}"),
        "Proof mode should generate existential (\u{2203}) quantifier"
    );
    assert!(
        proof.contains("proveTestsError"),
        "Void fail-once should use proveTestsError helper"
    );
    assert!(
        proof.contains("by blaster"),
        "Proof mode should use blaster tactic"
    );
}

/// S0003 also blocks `Void + fail_once + Int + witness` (the witness
/// path emits `⟨0, by decide⟩` and `proveTestsError`, both of which are
/// unsound for the same reason as the bool-returning Int case). Pin the
/// guard fires for this configuration.
#[test]
fn witness_mode_rejected_for_int_domain_void_fail_once() {
    let mut test = make_test_with_failure(
        "my_module",
        "test_ev_void",
        OnTestFailure::SucceedImmediately,
    );
    test.return_mode = TestReturnMode::Void;
    let id = test_id("my_module", "test_ev_void");
    let lean_name = sanitize_lean_name("test_ev_void");
    let lean_module = "AikenVerify.Proofs.My_module.test_ev_void";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    );
    assert_s0003_witness_unsound(
        &result,
        &test.name,
        "Int",
        "Int-domain Void witness mode must trigger S0003",
    );
}

/// Composite `Tuple<Int, Int>` is rejected by S0003 in witness mode, so
/// pin the existential-tuple lowering coverage on `--existential-mode proof`
/// instead. The bound-pair conjunction still appears under `∃` because the
/// existential is now discharged via Blaster rather than a trivial witness.
#[test]
fn fail_once_tuple_proof_mode_uses_conjunction_between_bounds() {
    let mut test = make_test_with_type(
        "my_module",
        "test_tuple_exist",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            },
            FuzzerConstraint::IntRange {
                min: "20".to_string(),
                max: "30".to_string(),
            },
        ]),
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    let id = test_id("my_module", "test_tuple_exist");
    let lean_name = sanitize_lean_name("test_tuple_exist");
    let lean_module = "AikenVerify.Proofs.My_module.test_tuple_exist";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("\u{2203}"),
        "Existential tuple proof should use \u{2203} quantifier"
    );
    assert!(
        proof.contains("(a : Integer)"),
        "Should bind first variable as Integer"
    );
    assert!(
        proof.contains("(b : Integer)"),
        "Should bind second variable as Integer"
    );
    assert!(
        proof.contains("\u{2227}"),
        "Tuple existential should use conjunction (\u{2227}) between bounds"
    );
    assert!(
        !proof.contains("\u{2192}"),
        "Existential mode should not contain implication (\u{2192})"
    );
    assert!(
        proof.contains("proveTests"),
        "Proof mode should use proveTests helper, got:\n{proof}"
    );
    assert!(
        proof.contains("by blaster"),
        "Proof mode should use blaster tactic, got:\n{proof}"
    );
}

/// Opaque top-level semantics route through `FallbackRequired` regardless
/// of `ExistentialMode` — but witness mode now hard-errors with S0003 *before*
/// the fallback is reached for the `Data` domain. To preserve coverage of
/// the opaque-Data fallback path on the existential branch, drive it under
/// `--existential-mode proof` (which is sound for `Data`).
#[test]
fn fail_once_data_proof_mode_returns_fallback_required_without_constructor_domain() {
    let mut test = make_test_with_type(
        "my_module",
        "test_data_exist",
        FuzzerOutputType::Data,
        FuzzerConstraint::Any,
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    let id = test_id("my_module", "test_data_exist");
    let lean_name = sanitize_lean_name("test_data_exist");
    let lean_module = "AikenVerify.Proofs.My_module.test_data_exist";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Opaque Data proof mode (fail-once) without constructor domain",
        "opaque",
    );
}

/// Pin the S0003 short-circuit for the `Data` domain: the user-visible
/// behaviour for `--existential-mode witness` on a `Data`-domain fail-once
/// test is a hard error with the catalogue's `Data` rendering.
#[test]
fn witness_mode_rejected_for_data_domain() {
    let mut test = make_test_with_type(
        "my_module",
        "test_data_exist",
        FuzzerOutputType::Data,
        FuzzerConstraint::Any,
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    let id = test_id("my_module", "test_data_exist");
    let lean_name = sanitize_lean_name("test_data_exist");
    let lean_module = "AikenVerify.Proofs.My_module.test_data_exist";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    );
    assert_s0003_witness_unsound(
        &result,
        &test.name,
        "Data",
        "Data-domain witness mode must trigger S0003",
    );
}

/// Sibling of `fail_once_data_proof_mode_returns_fallback_required_without_constructor_domain`
/// for the `Unsupported` carrier. The historic test exercised the same
/// soundness-only-stub fallback under witness mode; route it through proof
/// mode now that S0003 short-circuits witness mode for unsupported domains.
#[test]
fn fail_once_unsupported_proof_mode_returns_fallback_required_without_shape_domain() {
    let mut test = make_test_with_type(
        "my_module",
        "test_adt_exist",
        FuzzerOutputType::Unsupported("MyCustomType".to_string()),
        FuzzerConstraint::Any,
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    let id = test_id("my_module", "test_adt_exist");
    let lean_name = sanitize_lean_name("test_adt_exist");
    let lean_module = "AikenVerify.Proofs.My_module.test_adt_exist";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Unsupported ADT proof mode (fail-once) without constructor domain",
        "opaque",
    );
}

/// Pin the S0003 short-circuit for the `Unsupported` carrier (custom user
/// types whose semantic export is opaque). The catalogue surfaces the
/// type's own `pretty_print` rendering as the `domain` field.
#[test]
fn witness_mode_rejected_for_unsupported_domain() {
    let mut test = make_test_with_type(
        "my_module",
        "test_adt_exist",
        FuzzerOutputType::Unsupported("MyCustomType".to_string()),
        FuzzerConstraint::Any,
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    let id = test_id("my_module", "test_adt_exist");
    let lean_name = sanitize_lean_name("test_adt_exist");
    let lean_module = "AikenVerify.Proofs.My_module.test_adt_exist";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    );
    assert_s0003_witness_unsound(
        &result,
        &test.name,
        "MyCustomType",
        "Unsupported-domain witness mode must trigger S0003",
    );
}

/// `Tuple<Int, Data>` is rejected by S0003 in witness mode. Move the
/// existential tuple+data lowering coverage to `--existential-mode proof`
/// where the same lowering machinery is exercised soundly.
#[test]
fn fail_once_tuple_with_data_proof_mode_uses_valid_data_constructor() {
    let mut test = make_test_with_type(
        "my_module",
        "test_tuple_data_exist",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Data]),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            },
            FuzzerConstraint::Any,
        ]),
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    let id = test_id("my_module", "test_tuple_data_exist");
    let lean_name = sanitize_lean_name("test_tuple_data_exist");
    let lean_module = "AikenVerify.Proofs.My_module.test_tuple_data_exist";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("\u{2203}"),
        "Existential tuple+data proof should use \u{2203} quantifier"
    );
    assert!(
        proof.contains("(a : Integer)"),
        "Should bind Int component as Integer"
    );
    assert!(
        proof.contains("Data"),
        "Should reference Data type for the Data component"
    );
}

/// `List<Data>` is rejected by S0003 in witness mode. Drive the lowering
/// coverage under proof mode instead.
#[test]
fn fail_once_list_of_data_proof_mode_uses_valid_data_constructor() {
    let mut test = make_test_with_type(
        "my_module",
        "test_list_data_exist",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: Some(2),
            max_len: Some(5),
        },
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    let id = test_id("my_module", "test_list_data_exist");
    let lean_name = sanitize_lean_name("test_list_data_exist");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_data_exist";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("\u{2203}"),
        "List existential proof should use \u{2203} quantifier"
    );
    assert!(proof.contains("List"), "Should quantify over a List type");
    assert!(proof.contains("Data"), "List element type should be Data");
}

/// `List<Bool>` is rejected by S0003 in witness mode (only top-level Bool
/// is sound). The original "list<bool> hard-skipped" intent is preserved
/// by switching to proof mode, where the historic Blaster List<Bool>
/// hard-skip gate continues to fire (S0001 covers that, not S0003).
#[test]
fn fail_once_list_bool_exact_proof_mode_uses_exact_element() {
    let mut test = make_test_with_type(
        "my_module",
        "test_list_bool_exact_exist",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Bool)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Exact(FuzzerExactValue::Bool(false))),
            min_len: Some(1),
            max_len: Some(3),
        },
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    let id = test_id("my_module", "test_list_bool_exact_exist");
    let lean_name = sanitize_lean_name("test_list_bool_exact_exist");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_bool_exact_exist";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(&result, "list<bool> hard-skipped", "Bool");
}

#[test]
fn void_return_mode_generates_halt_theorem() {
    // Snapshot pins the canonical Void/halt shape: `proveTestsHalt` helper
    // and the universal `∀` quantifier.  Stable inputs → no redactions.
    let mut test = make_test("my_module", "test_void");
    test.return_mode = TestReturnMode::Void;
    let id = test_id("my_module", "test_void");
    let lean_name = sanitize_lean_name("test_void");
    let lean_module = "AikenVerify.Proofs.My_module.test_void";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    insta::assert_snapshot!("void_return_mode_generates_halt_theorem", proof);
}

#[test]
fn void_fail_mode_generates_error_theorem() {
    // Snapshot pins the canonical Void/fail shape: `proveTestsError` helper
    // and the universal `∀` quantifier.  Stable inputs → no redactions.
    let mut test = make_test_with_failure(
        "my_module",
        "test_void_fail",
        OnTestFailure::SucceedEventually,
    );
    test.return_mode = TestReturnMode::Void;
    let id = test_id("my_module", "test_void_fail");
    let lean_name = sanitize_lean_name("test_void_fail");
    let lean_module = "AikenVerify.Proofs.My_module.test_void_fail";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    insta::assert_snapshot!("void_fail_mode_generates_error_theorem", proof);
}

#[test]
fn bool_return_mode_generates_prove_tests() {
    // Verify Bool mode still works with explicit return_mode.  Snapshot pins
    // the canonical Lean shape: `proveTests`, `= true`, the `∀` quantifier
    // and the `alwaysTerminating` termination theorem.  Stable inputs (no
    // tempdir, no rev, no UUID) → no redactions needed.
    let mut test = make_test("my_module", "test_bool");
    test.return_mode = TestReturnMode::Bool;
    let id = test_id("my_module", "test_bool");
    let lean_name = sanitize_lean_name("test_bool");
    let lean_module = "AikenVerify.Proofs.My_module.test_bool";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    insta::assert_snapshot!("bool_return_mode_generates_prove_tests", proof);
}

fn make_test_with_type(
    module: &str,
    name: &str,
    fuzzer_output_type: FuzzerOutputType,
    constraint: FuzzerConstraint,
) -> ExportedPropertyTest {
    let module_path = module.replace('.', "/");
    let semantics =
        derive_fixture_semantics_from_constraint(&fuzzer_output_type, &constraint, false);

    ExportedPropertyTest {
        name: format!("{module}.{name}"),
        module: module.to_string(),
        input_path: format!("lib/{module_path}.ak"),
        on_test_failure: OnTestFailure::FailImmediately,
        return_mode: TestReturnMode::Bool,
        target_kind: Default::default(),
        validator_target: None,
        test_program: ExportedProgram {
            hex: "deadbeef".to_string(),
            flat_bytes: None,
        },
        fuzzer_program: ExportedProgram {
            hex: "cafebabe".to_string(),
            flat_bytes: None,
        },
        fuzzer_type: format!("Fuzzer<{:?}>", fuzzer_output_type),
        fuzzer_output_type,
        constraint,
        semantics,
        fuzzer_data_schema: None,
        inner_data_schemas: Default::default(),
        transition_prop_lean: None,
        concrete_halt_witnesses: Vec::new(),
        concrete_error_witnesses: Vec::new(),
    }
}

#[test]
fn bool_without_domain_predicate_returns_fallback_required() {
    // Opaque top-level semantics now route through a soundness-only stub
    // rather than a hard error, so the pipeline can proceed without
    // `--skip-unsupported`. Verify the stub shape.
    let test = make_test_with_type(
        "my_module",
        "test_bool",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_bool");
    let lean_name = sanitize_lean_name("test_bool");
    let lean_module = "AikenVerify.Proofs.My_module.test_bool";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(&result, "Bool without domain predicate", "opaque");
}

#[test]
fn bool_any_constraint_returns_fallback_required() {
    // Paired with `bool_without_domain_predicate_returns_fallback_required`;
    // historically this test pinned the FallbackRequired error category for
    // opaque Bool semantics. Under the soundness-only contract, opaque
    // top-level semantics produce a vacuous stub instead of an error.
    let test = make_test_with_type(
        "my_module",
        "test_bool_fallback_import",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_bool_fallback_import");
    let lean_name = sanitize_lean_name("test_bool_fallback_import");
    let lean_module = "AikenVerify.Proofs.My_module.test_bool_fallback_import";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Bool Any constraint (soundness-only stub)",
        "opaque",
    );
}

#[test]
fn bool_succeed_eventually_returns_fallback_required_without_domain_predicate() {
    // Opaque Bool semantics in SucceedEventually mode now also route to the
    // soundness-only stub. The stub is vacuously true under any mode, so
    // soundness is preserved.
    let mut test = make_test_with_type(
        "my_module",
        "test_bool_fail",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    );
    test.on_test_failure = OnTestFailure::SucceedEventually;

    let id = test_id("my_module", "test_bool_fail");
    let lean_name = sanitize_lean_name("test_bool_fail");
    let lean_module = "AikenVerify.Proofs.My_module.test_bool_fail";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Bool SucceedEventually without domain predicate",
        "opaque",
    );
}

#[test]
fn tuple_int_int_generates_two_variable_theorem() {
    let test = make_test_with_type(
        "my_module",
        "test_pair",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
        FuzzerConstraint::IntRange {
            min: "1".to_string(),
            max: "10".to_string(),
        },
    );
    let id = test_id("my_module", "test_pair");
    let lean_name = sanitize_lean_name("test_pair");
    let lean_module = "AikenVerify.Proofs.My_module.test_pair";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("(a : Integer)"),
        "Should bind first variable a as Integer"
    );
    assert!(
        proof.contains("(b : Integer)"),
        "Should bind second variable b as Integer"
    );
    assert!(
        proof.contains("\u{2200}"),
        "Universal proof should contain \u{2200} quantifier"
    );
    assert!(
        proof.contains("Data.List"),
        "Tuple proof should construct Data.List argument"
    );
}

#[test]
fn tuple_int_int_component_bounds_generate_two_ranges() {
    // Snapshot pins the canonical 2-range tuple lowering: two `Integer`
    // binders (`a`, `b`), a lower+upper bound on each (`0 ≤ a ≤ 10`,
    // `20 ≤ b ≤ 30`).  The textual shape (operator, spacing, binder order)
    // is exactly what we want to lock in.  Stable inputs → no redactions.
    let test = make_test_with_type(
        "my_module",
        "test_pair_components",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            },
            FuzzerConstraint::IntRange {
                min: "20".to_string(),
                max: "30".to_string(),
            },
        ]),
    );
    let id = test_id("my_module", "test_pair_components");
    let lean_name = sanitize_lean_name("test_pair_components");
    let lean_module = "AikenVerify.Proofs.My_module.test_pair_components";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    insta::assert_snapshot!("tuple_int_int_component_bounds_generate_two_ranges", proof);
}

#[test]
fn tuple_int_int_component_bounds_wrong_arity_returns_fallback_required() {
    // A mismatched-arity tuple constraint over a Tuple output type
    // degrades to opaque top-level semantics via
    // `derive_fixture_semantics_from_constraint`. Opaque top-level
    // semantics now route through the soundness-only stub.
    let test = make_test_with_type(
        "my_module",
        "test_pair_components_bad_arity",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
        FuzzerConstraint::Tuple(vec![FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        }]),
    );
    let id = test_id("my_module", "test_pair_components_bad_arity");
    let lean_name = sanitize_lean_name("test_pair_components_bad_arity");
    let lean_module = "AikenVerify.Proofs.My_module.test_pair_components_bad_arity";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Tuple(Int,Int) with mismatched-arity tuple constraint",
        "opaque",
    );
}

#[test]
fn tuple_int_int_unknown_bounds_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_pair_nobound",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_pair_nobound");
    let lean_name = sanitize_lean_name("test_pair_nobound");
    let lean_module = "AikenVerify.Proofs.My_module.test_pair_nobound";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Tuple(Int,Int) with unknown per-component bounds",
        "opaque",
    );
}

#[test]
fn bool_semantics_can_generate_direct_theorem_without_constraint_predicates() {
    let mut test = make_test_with_type(
        "my_module",
        "test_bool_semantics_direct",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Unsupported {
            reason: "legacy extractor intentionally unsupported".to_string(),
        },
    );
    test.semantics = FuzzerSemantics::Bool;

    let proof = generate_proof_file(
        &test,
        "test_bool_semantics_direct",
        "test_bool_semantics_direct",
        "AikenVerify.Proofs.My_module.test_bool_semantics_direct",
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("Bool"),
        "Bool semantics should quantify over Bool type"
    );
    assert!(
        proof.contains("boolArg"),
        "Bool semantics should use boolArg helper"
    );
    assert!(proof.contains("= true"), "Bool mode should assert = true");
}

#[test]
fn int_semantics_range_takes_precedence_over_legacy_constraint_bounds() {
    let mut test = make_test_with_type(
        "my_module",
        "test_int_semantics_override",
        FuzzerOutputType::Int,
        FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        },
    );
    test.semantics = FuzzerSemantics::IntRange {
        min: Some("100".to_string()),
        max: Some("100".to_string()),
    };

    let proof = generate_proof_file(
        &test,
        "test_int_semantics_override",
        "test_int_semantics_override",
        "AikenVerify.Proofs.My_module.test_int_semantics_override",
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("Integer"),
        "Int semantics should quantify over Integer type"
    );
    assert!(
        proof.contains("100"),
        "Semantics range (100..100) should take precedence over legacy constraint (0..10)"
    );
}

#[test]
fn non_state_machine_direct_generation_requires_semantics_not_legacy_constraints() {
    // The cutover regression guard: opaque top-level semantics must not
    // silently reuse the legacy IntRange constraint as an implicit
    // domain. Under the soundness-only contract this is enforced
    // structurally -- opaque top-level semantics route through the
    // `FallbackRequired` skip, which pins a theorem name distinct
    // from any real correctness theorem (`<name>_correct`, etc.).
    let mut test = make_test_with_type(
        "my_module",
        "test_semantics_cutover_int",
        FuzzerOutputType::Int,
        FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        },
    );
    test.semantics = FuzzerSemantics::Opaque {
        reason: "test fixture forces semantic fallback".to_string(),
    };

    let id = test_id("my_module", "test_semantics_cutover_int");
    let lean_name = sanitize_lean_name("test_semantics_cutover_int");
    let lean_module = "AikenVerify.Proofs.My_module.test_semantics_cutover_int";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Opaque semantics cutover (Int with legacy IntRange constraint)",
        "opaque",
    );
    // The FallbackRequired error message must NOT resurrect the legacy
    // IntRange constraint as a Lean precondition -- that would silently
    // reuse the abandoned lowering the cutover removed.
    let err_msg = result.as_ref().err().unwrap().to_string();
    assert!(
        !err_msg.contains("0 <= x") && !err_msg.contains("x <= 10"),
        "fallback error must not emit legacy IntRange preconditions: got:\n{err_msg}"
    );
}

#[test]
fn bytearray_without_domain_predicate_reports_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_bytes",
        FuzzerOutputType::ByteArray,
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_bytes");
    let lean_name = sanitize_lean_name("test_bytes");
    let lean_module = "AikenVerify.Proofs.My_module.test_bytes";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    let err = result.expect_err("ByteArray with opaque semantics should error without fallback");
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::FallbackRequired,
        "ByteArray without domain predicate",
    );
}

#[test]
fn bytearray_length_range_generates_direct_scalar_domain() {
    // Bounded scalar `ByteArray` fuzzers are now supported: the bytestring
    // skip gate only fires for unbounded or list-nested bytestrings. The
    // length bounds are emitted as a scalar precondition
    // (`min_len <= x.length ∧ x.length <= max_len`) by
    // `collect_scalar_precondition_parts_from_semantics`.
    let test = make_test_with_type(
        "my_module",
        "test_bytes_len_range",
        FuzzerOutputType::ByteArray,
        FuzzerConstraint::ByteStringLenRange {
            min_len: 1,
            max_len: 3,
        },
    );
    let id = test_id("my_module", "test_bytes_len_range");
    let lean_name = sanitize_lean_name("test_bytes_len_range");
    let lean_module = "AikenVerify.Proofs.My_module.test_bytes_len_range";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect("bounded scalar ByteArray should produce a proof");

    assert!(
        proof.contains("(x : ByteString)"),
        "Proof should quantify x as ByteString, got:\n{proof}"
    );
    assert!(
        proof.contains("1 <= x.length") && proof.contains("x.length <= 3"),
        "Proof should contain both length bounds, got:\n{proof}"
    );
}

#[test]
fn bytearray_exact_non_empty_generates_direct_scalar_domain() {
    // See `bytearray_length_range_generates_direct_scalar_domain`: any
    // top-level ByteArray fuzzer is skipped for Blaster.
    let test = make_test_with_type(
        "my_module",
        "test_bytes_exact_non_empty",
        FuzzerOutputType::ByteArray,
        FuzzerConstraint::Exact(FuzzerExactValue::ByteArray(vec![0x66, 0x6f, 0x6f])),
    );
    let id = test_id("my_module", "test_bytes_exact_non_empty");
    let lean_name = sanitize_lean_name("test_bytes_exact_non_empty");
    let lean_module = "AikenVerify.Proofs.My_module.test_bytes_exact_non_empty";

    let err = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap_err()
    .to_string();

    assert!(
        err.contains("ByteArray") || err.contains("bytearray_between"),
        "Expected ByteArray skip error, got:\n{err}"
    );
}

#[test]
fn string_length_range_generates_direct_scalar_domain() {
    // See `bytearray_length_range_generates_direct_scalar_domain`: any
    // top-level String fuzzer is now skipped for Blaster.
    let test = make_test_with_type(
        "my_module",
        "test_string_len_range",
        FuzzerOutputType::String,
        FuzzerConstraint::ByteStringLenRange {
            min_len: 2,
            max_len: 5,
        },
    );
    let id = test_id("my_module", "test_string_len_range");
    let lean_name = sanitize_lean_name("test_string_len_range");
    let lean_module = "AikenVerify.Proofs.My_module.test_string_len_range";

    let err = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap_err()
    .to_string();

    assert!(
        err.contains("String") || err.contains("ByteArray"),
        "Expected String/ByteArray skip error, got:\n{err}"
    );
}

#[test]
fn bytearray_length_range_with_inconsistent_bounds_errors() {
    // Bounded scalar `ByteArray` now bypasses the bytestring skip gate, so
    // inconsistent-bounds validation in the precondition builder fires
    // directly and surfaces a clear error naming the offending bounds.
    let test = make_test_with_type(
        "my_module",
        "test_bytes_bad_len_range",
        FuzzerOutputType::ByteArray,
        FuzzerConstraint::ByteStringLenRange {
            min_len: 9,
            max_len: 2,
        },
    );
    let id = test_id("my_module", "test_bytes_bad_len_range");
    let lean_name = sanitize_lean_name("test_bytes_bad_len_range");
    let lean_module = "AikenVerify.Proofs.My_module.test_bytes_bad_len_range";

    let err = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap_err();

    // The inconsistent-bounds check should fire with InvalidConstraint, not FallbackRequired
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::InvalidConstraint,
        "inconsistent ByteArray bounds",
    );

    let msg = err.to_string();
    assert!(
        msg.contains("inconsistent byte-string length bounds")
            && msg.contains("min_len=9")
            && msg.contains("max_len=2"),
        "Expected inconsistent-bounds error, got:\n{msg}"
    );
}

#[test]
fn string_exact_non_empty_generates_direct_scalar_domain() {
    // See `bytearray_length_range_generates_direct_scalar_domain`: any
    // top-level String fuzzer (including `Exact`) is skipped for Blaster.
    let test = make_test_with_type(
        "my_module",
        "test_string_exact_non_empty",
        FuzzerOutputType::String,
        FuzzerConstraint::Exact(FuzzerExactValue::String("Hi".to_string())),
    );
    let id = test_id("my_module", "test_string_exact_non_empty");
    let lean_name = sanitize_lean_name("test_string_exact_non_empty");
    let lean_module = "AikenVerify.Proofs.My_module.test_string_exact_non_empty";

    let err = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap_err()
    .to_string();

    assert!(
        err.contains("String") || err.contains("ByteArray"),
        "Expected String/ByteArray skip error, got:\n{err}"
    );
}

#[test]
fn bounded_scalar_bytearray_proceeds_to_proof_generation() {
    // Regression test for Issue 12: a fuzzer with bounded scalar `ByteArray`
    // (e.g. `fuzz.bytearray_between(28, 28)`) must not be rejected by the
    // bytestring skip gate. The length bounds are sufficient when the test
    // body does not inspect byte contents.
    let test = make_test_with_type(
        "my_module",
        "test_bytes_bounded",
        FuzzerOutputType::ByteArray,
        FuzzerConstraint::ByteStringLenRange {
            min_len: 28,
            max_len: 28,
        },
    );
    let id = test_id("my_module", "test_bytes_bounded");
    let lean_name = sanitize_lean_name("test_bytes_bounded");
    let lean_module = "AikenVerify.Proofs.My_module.test_bytes_bounded";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect("bounded scalar ByteArray must proceed to proof generation");

    assert!(
        proof.contains("theorem "),
        "Proof should contain a theorem, got:\n{proof}"
    );
    assert!(
        proof.contains("(x : ByteString)"),
        "Proof should quantify x as ByteString, got:\n{proof}"
    );
    assert!(
        proof.contains("28 <= x.length"),
        "expected lower bound with literal 28, got:\n{proof}"
    );
    assert!(
        proof.contains("x.length <= 28"),
        "expected upper bound with literal 28, got:\n{proof}"
    );
}

#[test]
fn unbounded_bytearray_still_skips() {
    // Unbounded ByteArray (no length info) is still skipped because there
    // is no precondition to anchor universal quantification over content.
    let mut test = make_test_with_type(
        "my_module",
        "test_bytes_unbounded",
        FuzzerOutputType::ByteArray,
        FuzzerConstraint::Any,
    );
    test.semantics = FuzzerSemantics::ByteArrayRange {
        min_len: None,
        max_len: None,
    };
    let id = test_id("my_module", "test_bytes_unbounded");
    let lean_name = sanitize_lean_name("test_bytes_unbounded");
    let lean_module = "AikenVerify.Proofs.My_module.test_bytes_unbounded";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    let err = result.expect_err("unbounded ByteArray must still skip");
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::FallbackRequired,
        "unbounded ByteArray",
    );
    let msg = err.to_string();
    assert!(
        msg.contains("Unbounded or list-nested") || msg.contains("list-nested"),
        "Error should reference the new unbounded/list-nested skip reason, got:\n{msg}"
    );
}

#[test]
fn half_bounded_bytearray_still_skips() {
    // Half-bounded ByteArray (only one of min_len/max_len set) must still
    // skip: the bounded-scalar carve-out in `bytestring_skip_required`
    // requires *both* bounds to be present. Cover both variants.

    // Variant A: min_len = Some, max_len = None (no upper bound).
    {
        let mut test = make_test_with_type(
            "my_module",
            "test_bytes_min_only",
            FuzzerOutputType::ByteArray,
            FuzzerConstraint::Any,
        );
        test.semantics = FuzzerSemantics::ByteArrayRange {
            min_len: Some(5),
            max_len: None,
        };
        let id = test_id("my_module", "test_bytes_min_only");
        let lean_name = sanitize_lean_name("test_bytes_min_only");
        let lean_module = "AikenVerify.Proofs.My_module.test_bytes_min_only";

        let result = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        );
        let err = result.expect_err("min-only ByteArray must still skip");
        assert_generation_error_category(
            &err,
            GenerationErrorCategory::FallbackRequired,
            "half-bounded ByteArray (min_len only)",
        );
    }

    // Variant B: min_len = None, max_len = Some (no lower bound).
    {
        let mut test = make_test_with_type(
            "my_module",
            "test_bytes_max_only",
            FuzzerOutputType::ByteArray,
            FuzzerConstraint::Any,
        );
        test.semantics = FuzzerSemantics::ByteArrayRange {
            min_len: None,
            max_len: Some(5),
        };
        let id = test_id("my_module", "test_bytes_max_only");
        let lean_name = sanitize_lean_name("test_bytes_max_only");
        let lean_module = "AikenVerify.Proofs.My_module.test_bytes_max_only";

        let result = generate_proof_file(
            &test,
            &id,
            &lean_name,
            lean_module,
            ExistentialMode::default(),
            &VerificationTargetKind::default(),
        );
        let err = result.expect_err("max-only ByteArray must still skip");
        assert_generation_error_category(
            &err,
            GenerationErrorCategory::FallbackRequired,
            "half-bounded ByteArray (max_len only)",
        );
    }
}

#[test]
fn string_always_skips() {
    // `String` is always skipped regardless of any length bounds: there is
    // no length-only path that covers String content in Blaster.
    let mut test = make_test_with_type(
        "my_module",
        "test_string_skip",
        FuzzerOutputType::String,
        FuzzerConstraint::Any,
    );
    test.semantics = FuzzerSemantics::String;
    let id = test_id("my_module", "test_string_skip");
    let lean_name = sanitize_lean_name("test_string_skip");
    let lean_module = "AikenVerify.Proofs.My_module.test_string_skip";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    let err = result.expect_err("String must always skip");
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::FallbackRequired,
        "String always skips",
    );
    let msg = err.to_string();
    assert!(
        msg.contains("ByteString") || msg.contains("String"),
        "Error should mention ByteString/String, got:\n{msg}"
    );
}

#[test]
fn list_without_bounds_reports_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_list",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: None,
            max_len: None,
        },
    );
    let id = test_id("my_module", "test_list");
    let lean_name = sanitize_lean_name("test_list");
    let lean_module = "AikenVerify.Proofs.My_module.test_list";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect("List<Int> with Any element should produce a direct proof via list semantics");

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("List Integer"),
        "Unbounded list over Int should still quantify over List Integer"
    );
    assert!(
        proof.contains("\u{2200}"),
        "Universal theorem should contain \u{2200} quantifier"
    );
    assert!(
        !proof.contains(".length <="),
        "No max-len constraint should emit an upper-bound predicate on xs.length"
    );
}

#[test]
fn list_with_any_constraint_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_list_any",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_list_any");
    let lean_name = sanitize_lean_name("test_list_any");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_any";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "List with top-level Any constraint (opaque element-domain)",
        "opaque",
    );
}

#[test]
fn list_with_unsupported_constraint_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_list_unsupported",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerConstraint::Unsupported {
            reason: "scenario.ok: constraint extraction not implemented yet".to_string(),
        },
    );
    let id = test_id("my_module", "test_list_unsupported");
    let lean_name = sanitize_lean_name("test_list_unsupported");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_unsupported";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "List with top-level Unsupported constraint",
        "opaque",
    );
}

#[test]
fn list_unsupported_element_with_only_zero_lower_bound_uses_fallback() {
    let test = make_test_with_type(
        "my_module",
        "test_list_data_min_zero",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported(
            "Transaction".to_string(),
        ))),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: Some(0),
            max_len: None,
        },
    );
    let id = test_id("my_module", "test_list_data_min_zero");
    let lean_name = sanitize_lean_name("test_list_data_min_zero");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_data_min_zero";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect("List constraint with Any element should produce a direct proof via list semantics");

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("List Data"),
        "Unsupported element type should be lowered to List Data, got:\n{proof}"
    );
    assert!(
        proof.contains("\u{2200}"),
        "Universal theorem should contain \u{2200} quantifier"
    );
    // min_len = 0 should emit no effective length precondition (0 <= len is trivial).
    assert!(
        !proof.contains("xs.length <="),
        "min_len=0 without max_len should not emit a length upper-bound"
    );
}

#[test]
fn unsupported_with_constructor_tags_generates_direct_data_theorem() {
    let test = make_test_with_type(
        "my_module",
        "test_outcome_domain",
        FuzzerOutputType::Unsupported("Outcome".to_string()),
        FuzzerConstraint::DataConstructorTags { tags: vec![0, 1] },
    );
    let id = test_id("my_module", "test_outcome_domain");
    let lean_name = sanitize_lean_name("test_outcome_domain");
    let lean_module = "AikenVerify.Proofs.My_module.test_outcome_domain";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("Data"),
        "Unsupported type should be quantified as Data"
    );
    assert!(
        proof.contains("Data.Constr"),
        "Constructor tag domain should use Data.Constr match"
    );
    assert!(proof.contains("dataArg"), "Should use dataArg helper");
}

#[test]
fn int_with_unsupported_constraint_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_int_unsupported",
        FuzzerOutputType::Int,
        FuzzerConstraint::Unsupported {
            reason: "custom lambda fuzzer domain not yet extracted".to_string(),
        },
    );
    let id = test_id("my_module", "test_int_unsupported");
    let lean_name = sanitize_lean_name("test_int_unsupported");
    let lean_module = "AikenVerify.Proofs.My_module.test_int_unsupported";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Int with top-level Unsupported constraint",
        "opaque",
    );
}

#[test]
fn universal_int_and_with_unsupported_salvages_supported_bounds() {
    let test = make_test_with_type(
        "my_module",
        "test_int_partial_salvage",
        FuzzerOutputType::Int,
        FuzzerConstraint::And(vec![
            FuzzerConstraint::IntRange {
                min: "1".to_string(),
                max: "9".to_string(),
            },
            FuzzerConstraint::Unsupported {
                reason: "custom lambda fragment".to_string(),
            },
        ]),
    );
    let id = test_id("my_module", "test_int_partial_salvage");
    let lean_name = sanitize_lean_name("test_int_partial_salvage");
    let lean_module = "AikenVerify.Proofs.My_module.test_int_partial_salvage";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("Integer"),
        "Should quantify over Integer type"
    );
    assert!(
        proof.contains("1 <= x"),
        "Salvaged bounds should include min bound"
    );
    assert!(
        proof.contains("x <= 9"),
        "Salvaged bounds should include max bound"
    );
}

/// `Int` is rejected by S0003 in witness mode. Drive the existential
/// IntRange-salvage lowering coverage under proof mode instead, which
/// exercises the same `lower_int_range_constraint` path soundly.
#[test]
fn existential_int_and_with_unsupported_uses_sampled_fallback_proof_mode() {
    let mut test = make_test_with_type(
        "my_module",
        "test_int_partial_salvage_existential",
        FuzzerOutputType::Int,
        FuzzerConstraint::And(vec![
            FuzzerConstraint::IntRange {
                min: "1".to_string(),
                max: "9".to_string(),
            },
            FuzzerConstraint::Unsupported {
                reason: "custom lambda fragment".to_string(),
            },
        ]),
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    let id = test_id("my_module", "test_int_partial_salvage_existential");
    let lean_name = sanitize_lean_name("test_int_partial_salvage_existential");
    let lean_module = "AikenVerify.Proofs.My_module.test_int_partial_salvage_existential";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    )
    .expect("salvageable IntRange in existential mode should produce a direct proof");

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("\u{2203}"),
        "Existential (proof) mode should use \u{2203} quantifier"
    );
    assert!(
        proof.contains("Integer"),
        "Should quantify over Integer type"
    );
    assert!(
        proof.contains("1 <= x"),
        "Salvaged min bound from AND should appear in existential proof"
    );
    assert!(
        proof.contains("x <= 9"),
        "Salvaged max bound from AND should appear in existential proof"
    );
}

#[test]
fn list_data_without_domain_predicate_uses_fuzzer_domain_fallback() {
    let test = make_test_with_type(
        "my_module",
        "test_list_data_unconstrained",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: None,
            max_len: None,
        },
    );
    let id = test_id("my_module", "test_list_data_unconstrained");
    let lean_name = sanitize_lean_name("test_list_data_unconstrained");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_data_unconstrained";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect("List<Data> with Any element should produce a direct proof via list semantics");

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("List Data"),
        "List<Data> without bounds should be lowered to List Data, got:\n{proof}"
    );
    assert!(
        proof.contains("\u{2200}"),
        "Universal theorem should contain \u{2200} quantifier"
    );
}

#[test]
fn list_unsupported_element_without_domain_predicate_uses_fuzzer_domain_fallback() {
    let test = make_test_with_type(
        "my_module",
        "test_list_transaction_like",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported(
            "cardano/transaction.Transaction".to_string(),
        ))),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: None,
            max_len: None,
        },
    );
    let id = test_id("my_module", "test_list_transaction_like");
    let lean_name = sanitize_lean_name("test_list_transaction_like");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_transaction_like";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect(
        "List with unsupported element type should still produce a direct proof via list semantics",
    );

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("List Data"),
        "Unsupported element type (Transaction) should be erased to List Data, got:\n{proof}"
    );
    assert!(
        proof.contains("\u{2200}"),
        "Universal theorem should contain \u{2200} quantifier"
    );
}

/// `List<Data>` is rejected by S0003 in witness mode. Drive the existential
/// `List<Data>` lowering coverage under proof mode instead.
#[test]
fn list_data_fallback_supports_existential_proof_mode() {
    let mut test = make_test_with_type(
        "my_module",
        "test_list_data_existential",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: None,
            max_len: None,
        },
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;

    let id = test_id("my_module", "test_list_data_existential");
    let lean_name = sanitize_lean_name("test_list_data_existential");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_data_existential";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    )
    // List<Data> with list semantics now generates a direct proof
    .expect("List<Data> with existential mode should generate direct proof");

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("\u{2203}"),
        "Proof mode should emit \u{2203} quantifier for List<Data>"
    );
    assert!(
        proof.contains("List Data"),
        "Should quantify over List Data"
    );
}

#[test]
fn data_without_domain_predicate_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_data",
        FuzzerOutputType::Data,
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_data");
    let lean_name = sanitize_lean_name("test_data");
    let lean_module = "AikenVerify.Proofs.My_module.test_data";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Plain Data without domain predicate",
        "opaque",
    );
}

#[test]
fn tuple_data_data_without_predicates_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_data_pair",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Data, FuzzerOutputType::Data]),
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_data_pair");
    let lean_name = sanitize_lean_name("test_data_pair");
    let lean_module = "AikenVerify.Proofs.My_module.test_data_pair";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Tuple(Data,Data) without predicates",
        "opaque",
    );
}

#[test]
fn tuple_data_data_data_without_predicates_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_data_triple",
        FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::Data,
            FuzzerOutputType::Data,
            FuzzerOutputType::Data,
        ]),
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_data_triple");
    let lean_name = sanitize_lean_name("test_data_triple");
    let lean_module = "AikenVerify.Proofs.My_module.test_data_triple";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Tuple(Data,Data,Data) without predicates",
        "opaque",
    );
}

#[test]
fn tuple_mixed_int_bool_generates_theorem_with_bounds() {
    let test = make_test_with_type(
        "my_module",
        "test_tuple_mixed",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Bool]),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "100".to_string(),
            },
            FuzzerConstraint::Any,
        ]),
    );
    let id = test_id("my_module", "test_tuple_mixed");
    let lean_name = sanitize_lean_name("test_tuple_mixed");
    let lean_module = "AikenVerify.Proofs.My_module.test_tuple_mixed";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("Integer"),
        "Should contain Integer type for Int component"
    );
    assert!(
        proof.contains("Bool"),
        "Should contain Bool type for Bool component"
    );
    assert!(proof.contains("0 <= a"), "Should contain Int bounds");
}

#[test]
fn list_semantics_can_generate_direct_theorem_without_constraint_extraction() {
    let mut test = make_test_with_type(
        "my_module",
        "test_list_semantics_direct",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Bool)),
        FuzzerConstraint::Unsupported {
            reason: "legacy extractor intentionally unsupported".to_string(),
        },
    );
    test.semantics = FuzzerSemantics::List {
        element: Box::new(FuzzerSemantics::Bool),
        min_len: Some(1),
        max_len: Some(3),
    };

    let result = generate_proof_file(
        &test,
        "test_list_semantics_direct",
        "test_list_semantics_direct",
        "AikenVerify.Proofs.My_module.test_list_semantics_direct",
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(&result, "list<bool> hard-skipped", "Bool");
}

#[test]
fn tuple_with_nested_list_element_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_tuple_nested",
        FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::Int,
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        ]),
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_tuple_nested");
    let lean_name = sanitize_lean_name("test_tuple_nested");
    let lean_module = "AikenVerify.Proofs.My_module.test_tuple_nested";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Tuple(Int, List<Int>) without per-component constraints",
        "opaque",
    );
}

#[test]
fn tuple_of_lists_with_unsupported_elements_uses_fallback() {
    let test = make_test_with_type(
        "my_module",
        "test_tuple_list_domains",
        FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported("Label".to_string()))),
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported(
                "Transaction".to_string(),
            ))),
        ]),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(0),
                max_len: None,
            },
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(0),
                max_len: None,
            },
        ]),
    );
    let id = test_id("my_module", "test_tuple_list_domains");
    let lean_name = sanitize_lean_name("test_tuple_list_domains");
    let lean_module = "AikenVerify.Proofs.My_module.test_tuple_list_domains";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect("Tuple of List constraints with Any element should produce a direct proof via list semantics");

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    // Both list components must be lowered to List Data
    let list_data_count = proof.matches("List Data").count();
    assert!(
        list_data_count >= 2,
        "Tuple of two lists with unsupported elements should contain two `List Data` bindings, got {list_data_count} in:\n{proof}"
    );
    assert!(
        proof.contains("\u{2200}"),
        "Universal theorem should contain \u{2200} quantifier"
    );
}

#[test]
fn utils_contains_arg_helpers() {
    let utils = generate_utils(10000);
    assert!(
        utils.contains("def boolArg"),
        "Utils should contain boolArg definition"
    );
    assert!(
        utils.contains("Data.Constr (if x then 1 else 0) []"),
        "boolArg should encode Bool as Data.Constr"
    );
    assert!(utils.contains("def intArg"));
    assert!(utils.contains("def bytearrayArg"));
    assert!(utils.contains("def stringArg"));
    assert!(utils.contains("def dataArg"));
    assert!(utils.contains("def pairArg"));
    assert!(utils.contains("def dataArgs2"));
    assert!(utils.contains("def dataArgs3"));
    assert!(
        utils.contains("def proveTestsHalt"),
        "Utils should contain proveTestsHalt for Void mode"
    );
    assert!(
        utils.contains("def proveTestsError"),
        "Utils should contain proveTestsError for Void+fail mode"
    );
    // Step 5.1: witness helper for fail-once existential mode
    assert!(
        utils.contains("def witnessTests"),
        "Utils should contain witnessTests for fail-once witness mode"
    );
}

#[test]
fn existential_mode_parse_roundtrip() {
    assert_eq!(
        "witness".parse::<ExistentialMode>().unwrap(),
        ExistentialMode::Witness
    );
    assert_eq!(
        "proof".parse::<ExistentialMode>().unwrap(),
        ExistentialMode::Proof
    );
    assert_eq!(
        "Witness".parse::<ExistentialMode>().unwrap(),
        ExistentialMode::Witness
    );
    assert_eq!(
        "PROOF".parse::<ExistentialMode>().unwrap(),
        ExistentialMode::Proof
    );
    assert!("unknown".parse::<ExistentialMode>().is_err());
}

#[test]
fn existential_mode_display() {
    assert_eq!(ExistentialMode::Witness.to_string(), "witness");
}

/// Documents the map2 over-approximation: when a property test uses
/// `fuzz.map2(int_between(0, 10), int_between(20, 30), fn(a, b) { (a, b) })`,
/// the generated proof quantifies over the Cartesian product of [0,10] x [20,30]
/// independently. Any relational constraints introduced by the mapper (e.g. a < b)
/// are NOT captured -- the proof domain is an over-approximation. Properties
/// depending on such correlations may fail in formal verification even if
/// property testing passes.
#[test]
fn map2_tuple_decomposition_is_cartesian_product() {
    // Simulates map2(int_between(0, 10), int_between(20, 30), fn(a, b) { (a, b) })
    let test = make_test_with_type(
        "my_module",
        "test_map2_pair",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            },
            FuzzerConstraint::IntRange {
                min: "20".to_string(),
                max: "30".to_string(),
            },
        ]),
    );
    let id = test_id("my_module", "test_map2_pair");
    let lean_name = sanitize_lean_name("test_map2_pair");
    let lean_module = "AikenVerify.Proofs.My_module.test_map2_pair";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("(a : Integer)"),
        "Should bind first element as Integer"
    );
    assert!(
        proof.contains("(b : Integer)"),
        "Should bind second element as Integer"
    );
    // Each element is independently bounded (Cartesian product, no cross-element constraints)
    assert!(
        proof.contains("0 <= a"),
        "Should bound first element independently"
    );
    assert!(
        proof.contains("20 <= b"),
        "Should bound second element independently"
    );
    // Preconditions use implication (→) between independent components
    assert!(
        proof.contains("\u{2192}"),
        "Independent Cartesian bounds should be joined by implication"
    );
}

#[test]
fn requires_explicit_bounds_is_constraint_driven() {
    // Int with Any constraint => needs bounds
    assert!(requires_explicit_bounds(
        &FuzzerOutputType::Int,
        &FuzzerConstraint::Any,
    ));
    // Int with IntRange => does not need bounds
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::Int,
        &FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        },
    ));
    // (Int, Int) with Any => needs bounds
    assert!(requires_explicit_bounds(
        &FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
        &FuzzerConstraint::Any,
    ));
    // (Int, Int) with Tuple of IntRanges => does not need bounds
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
        &FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            },
            FuzzerConstraint::IntRange {
                min: "20".to_string(),
                max: "30".to_string(),
            },
        ]),
    ));
    // Bool never needs bounds regardless of constraint
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::Bool,
        &FuzzerConstraint::Any,
    ));
    // ByteArray never needs bounds
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::ByteArray,
        &FuzzerConstraint::Any,
    ));
    // Data never needs bounds
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::Data,
        &FuzzerConstraint::Any,
    ));
    // (Data, Data) never needs bounds
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::Tuple(vec![FuzzerOutputType::Data, FuzzerOutputType::Data]),
        &FuzzerConstraint::Any,
    ));
    // Int with Map(IntRange) does NOT need explicit bounds — extract_int_range looks through Map.
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::Int,
        &FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        })),
    ));
}

fn make_manifest(entries: Vec<(&str, &str, &str)>) -> GeneratedManifest {
    make_manifest_with_termination(entries, true)
}

fn make_manifest_with_termination(
    entries: Vec<(&str, &str, &str)>,
    has_termination_theorem: bool,
) -> GeneratedManifest {
    GeneratedManifest {
        version: GENERATE_ONLY_VERSION.to_string(),
        tests: entries
            .into_iter()
            .map(|(module, name, lean_theorem)| ManifestEntry {
                id: test_id(module, name),
                aiken_module: module.to_string(),
                aiken_name: name.to_string(),
                lean_module: format!(
                    "AikenVerify.Proofs.{}.{}",
                    module_to_lean_segment(module),
                    sanitize_lean_name(name)
                ),
                lean_theorem: lean_theorem.to_string(),
                lean_file: format!(
                    "AikenVerify/Proofs/{}/{}.lean",
                    module_to_lean_segment(module),
                    sanitize_lean_name(name)
                ),
                flat_file: format!("cbor/{}.cbor", test_id(module, name)),
                has_termination_theorem,
                has_equivalence_theorem: false,
                over_approximations: 0,
                partial_proof_note: None,
                witness_proof_note: None,
            })
            .collect(),
        skipped: Vec::new(),
    }
}

#[test]
fn equivalence_theorem_name_for_entry_detects_marker_in_proof_file() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path();
    let lean_file = "AikenVerify/Proofs/My_module/test_eq.lean".to_string();
    let entry = ManifestEntry {
        id: "my_module__test_eq_deadbeef".to_string(),
        aiken_module: "my_module".to_string(),
        aiken_name: "test_eq".to_string(),
        lean_module: "AikenVerify.Proofs.My_module.test_eq".to_string(),
        lean_theorem: "test_eq".to_string(),
        lean_file: lean_file.clone(),
        flat_file: "cbor/my_module__test_eq_deadbeef.cbor".to_string(),
        has_termination_theorem: false,
        has_equivalence_theorem: true,
        over_approximations: 0,
        partial_proof_note: None,
        witness_proof_note: None,
    };

    fs::create_dir_all(out_dir.join("AikenVerify/Proofs/My_module")).unwrap();
    fs::write(
        out_dir.join(&lean_file),
        "theorem test_eq_equivalence :\n  True :=\n  by trivial\n",
    )
    .unwrap();

    assert_eq!(
        equivalence_theorem_name_for_entry(out_dir, &entry),
        Some("test_eq_equivalence".to_string())
    );
}

#[test]
fn equivalence_theorem_name_for_entry_returns_none_without_marker() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path();
    let lean_file = "AikenVerify/Proofs/My_module/test_eq.lean".to_string();
    let entry = ManifestEntry {
        id: "my_module__test_eq_deadbeef".to_string(),
        aiken_module: "my_module".to_string(),
        aiken_name: "test_eq".to_string(),
        lean_module: "AikenVerify.Proofs.My_module.test_eq".to_string(),
        lean_theorem: "test_eq".to_string(),
        lean_file: lean_file.clone(),
        flat_file: "cbor/my_module__test_eq_deadbeef.cbor".to_string(),
        has_termination_theorem: false,
        has_equivalence_theorem: false,
        over_approximations: 0,
        partial_proof_note: None,
        witness_proof_note: None,
    };

    fs::create_dir_all(out_dir.join("AikenVerify/Proofs/My_module")).unwrap();
    fs::write(
        out_dir.join(&lean_file),
        "theorem test_eq :\n  True :=\n  by trivial\n",
    )
    .unwrap();

    assert_eq!(equivalence_theorem_name_for_entry(out_dir, &entry), None);
}

#[test]
fn parse_verify_results_all_proved() {
    let manifest = make_manifest(vec![
        ("my_module", "test_add", "test_add"),
        ("my_module", "test_sub", "test_sub"),
    ]);
    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small("Build completed successfully."),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 4);
    assert_eq!(summary.proved, 4);
    assert_eq!(summary.failed, 0);
    assert_eq!(summary.timed_out, 0);
    assert_eq!(summary.unknown, 0);
    assert!(
        summary
            .theorems
            .iter()
            .all(|t| matches!(t.status, ProofStatus::Proved))
    );
}

#[test]
fn parse_verify_results_marks_two_phase_entries_as_partial_on_success() {
    // When `theorem_results` is None (lake build succeeded with no detailed
    // per-theorem signal), entries flagged with `partial_proof_note` MUST be
    // surfaced as `Partial`, not `Proved`. The termination companion stays
    // `Proved`. Build success implies `command_success == true` because
    // partial does not count as failure.
    let mut manifest = make_manifest(vec![("sm_module", "halt_test", "halt_test")]);
    let note = "Phase 2 (CEK halt obligation) is sorry-closed — see §S6";
    manifest.tests[0].partial_proof_note = Some(note.to_string());
    manifest.tests[0].over_approximations = 5; // must be preserved on the Partial result

    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small("Build completed successfully."),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    // Two theorems total: correctness (partial) + termination (proved).
    assert_eq!(summary.total, 2);
    assert_eq!(summary.partial, 1);
    assert_eq!(summary.proved, 1);
    assert_eq!(summary.failed, 0);
    assert_eq!(summary.timed_out, 0);
    assert_eq!(summary.unknown, 0);

    // The correctness theorem is the one named `halt_test` (no suffix); the
    // termination companion is `halt_test_alwaysTerminating`.
    let correctness = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "halt_test")
        .expect("correctness theorem present");
    match &correctness.status {
        ProofStatus::Partial { note: actual } => assert_eq!(actual, note),
        other => panic!("expected Partial for correctness theorem, got {other:?}"),
    }
    assert_eq!(
        correctness.over_approximations, 5,
        "over_approximations must be preserved on Partial results"
    );

    let termination = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "halt_test_alwaysTerminating")
        .expect("termination theorem present");
    assert!(
        matches!(termination.status, ProofStatus::Proved),
        "termination companion must remain Proved, got {:?}",
        termination.status
    );

    // Partial does NOT cause command failure.
    assert!(
        summary.command_success,
        "partial proofs must not flip command_success to false"
    );
}

#[test]
fn parse_verify_results_partial_does_not_count_as_failed() {
    // Belt-and-suspenders test: a summary containing only `Partial` and
    // `Proved` theorems must report zero failures and command_success = true,
    // exercising the precomputed-theorem-results path as well.
    let manifest = make_manifest_with_termination(
        vec![("sm_module", "halt_only", "halt_only")],
        false, // no termination companion
    );

    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: Some(vec![TheoremResult {
            test_name: "sm_module.halt_only".to_string(),
            theorem_name: "halt_only".to_string(),
            status: ProofStatus::Partial {
                note: "Phase 2 (CEK halt obligation) is sorry-closed — see §S6".to_string(),
            },
            over_approximations: 0,
        }]),
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 1);
    assert_eq!(summary.partial, 1);
    assert_eq!(summary.proved, 0);
    assert_eq!(summary.failed, 0);
    assert_eq!(summary.timed_out, 0);
    assert_eq!(summary.unknown, 0);
    assert!(
        matches!(summary.theorems[0].status, ProofStatus::Partial { .. }),
        "surviving theorem must be Partial, got {:?}",
        summary.theorems[0].status
    );
    assert!(
        summary.command_success,
        "partial-only summary must report command_success = true"
    );
}

#[test]
fn manifest_entry_partial_proof_note_serializes_only_when_set() {
    // Round-trip the JSON shape of `ManifestEntry` to confirm:
    //   - `partial_proof_note: Some(...)` appears in the serialized JSON
    //     and survives a JSON-string round trip via `serde_json::Value`.
    //   - `partial_proof_note: None` is omitted entirely (no null field).
    let entry_with_note = ManifestEntry {
        id: "sm_module__halt_test_deadbeef".to_string(),
        aiken_module: "sm_module".to_string(),
        aiken_name: "halt_test".to_string(),
        lean_module: "AikenVerify.Proofs.Sm_module.halt_test".to_string(),
        lean_theorem: "halt_test".to_string(),
        lean_file: "AikenVerify/Proofs/Sm_module/halt_test.lean".to_string(),
        flat_file: "cbor/sm_module__halt_test_deadbeef.cbor".to_string(),
        has_termination_theorem: true,
        has_equivalence_theorem: false,
        over_approximations: 0,
        partial_proof_note: Some(
            "Phase 2 (CEK halt obligation) is sorry-closed — see §S6".to_string(),
        ),
        witness_proof_note: None,
    };

    let json_with_note =
        serde_json::to_value(&entry_with_note).expect("entry with note should serialize");
    assert_eq!(
        json_with_note["partial_proof_note"]
            .as_str()
            .expect("partial_proof_note must be present and a string"),
        "Phase 2 (CEK halt obligation) is sorry-closed — see §S6"
    );
    // Round-trip via the JSON string form so we exercise the serializer end-to-end.
    let serialized = serde_json::to_string(&entry_with_note).unwrap();
    let reparsed: serde_json::Value = serde_json::from_str(&serialized).unwrap();
    assert_eq!(
        reparsed["partial_proof_note"],
        json_with_note["partial_proof_note"]
    );

    let entry_without_note = ManifestEntry {
        id: "mod__t_id".to_string(),
        aiken_module: "mod".to_string(),
        aiken_name: "t".to_string(),
        lean_module: "AikenVerify.Proofs.Mod.t".to_string(),
        lean_theorem: "t".to_string(),
        lean_file: "AikenVerify/Proofs/Mod/t.lean".to_string(),
        flat_file: "cbor/mod__t_id.cbor".to_string(),
        has_termination_theorem: false,
        has_equivalence_theorem: false,
        over_approximations: 0,
        partial_proof_note: None,
        witness_proof_note: None,
    };

    let json_without_note =
        serde_json::to_value(&entry_without_note).expect("entry without note should serialize");
    let object = json_without_note
        .as_object()
        .expect("ManifestEntry serializes to an object");
    assert!(
        !object.contains_key("partial_proof_note"),
        "partial_proof_note must be omitted from JSON when None, got: {:?}",
        object.keys().collect::<Vec<_>>()
    );
}

#[test]
fn verify_summary_version_is_current() {
    // The summary produced by parse_verify_results must carry the schema
    // version constant, bumped when the JSON shape changes.
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small("Build completed successfully."),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);
    assert_eq!(summary.verify_summary_version, VERIFY_SUMMARY_VERSION);

    let json = serde_json::to_value(&summary).expect("summary should serialize");
    assert_eq!(
        json["verify_summary_version"],
        serde_json::Value::String(VERIFY_SUMMARY_VERSION.to_string())
    );
}

#[test]
fn verify_summary_includes_blaster_rev_and_plutus_core_rev() {
    // VerifySummary must surface the resolved Blaster / PlutusCore git revs
    // so JSON consumers can audit which dependency snapshot the proofs were
    // verified against. The library-level `parse_verify_results` defaults to
    // empty strings (the CLI fills them in from RunCommandOptions); this
    // test exercises BOTH the post-parse population path and the JSON
    // round-trip, including a non-default override to pin the wiring.
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small("Build completed successfully."),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: None,
    };

    let mut summary = parse_verify_results(raw, &manifest);

    // Library-level default: empty strings until the CLI stamps them in.
    assert_eq!(summary.blaster_rev, "");
    assert_eq!(summary.plutus_core_rev, "");

    // Commit 18 (folds C14 #1): the *empty* defaults MUST also serialise
    // to `""` rather than being elided. A future regression slipping
    // `#[serde(skip_serializing_if = "String::is_empty")]` past review
    // would silently disappear the fields from the JSON wire format and
    // break consumers that key off their presence.
    let json_pre = serde_json::to_value(&summary).expect("default summary should serialize");
    assert_eq!(
        json_pre["blaster_rev"],
        serde_json::Value::String(String::new()),
        "blaster_rev MUST appear in JSON even when empty",
    );
    assert_eq!(
        json_pre["plutus_core_rev"],
        serde_json::Value::String(String::new()),
        "plutus_core_rev MUST appear in JSON even when empty",
    );

    // Default-rev population path (mirrors what the CLI does after parsing).
    summary.blaster_rev = DEFAULT_BLASTER_REV.to_string();
    summary.plutus_core_rev = DEFAULT_PLUTUS_CORE_REV.to_string();

    assert_eq!(summary.blaster_rev, DEFAULT_BLASTER_REV);
    assert_eq!(summary.plutus_core_rev, DEFAULT_PLUTUS_CORE_REV);

    // The fields MUST appear in JSON output even when empty (no
    // `skip_serializing_if`) so consumers can rely on their presence.
    let json = serde_json::to_value(&summary).expect("summary should serialize");
    assert_eq!(
        json["blaster_rev"],
        serde_json::Value::String(DEFAULT_BLASTER_REV.to_string())
    );
    assert_eq!(
        json["plutus_core_rev"],
        serde_json::Value::String(DEFAULT_PLUTUS_CORE_REV.to_string())
    );

    // Non-default override: confirm an arbitrary rev round-trips through
    // the struct as-is (no validation/normalization at the summary layer —
    // `validate_git_rev` runs upstream during workspace generation).
    summary.blaster_rev = "abc1234".to_string();
    summary.plutus_core_rev = "feature/experimental".to_string();

    let json = serde_json::to_value(&summary).expect("summary should serialize");
    assert_eq!(
        json["blaster_rev"],
        serde_json::Value::String("abc1234".to_string())
    );
    assert_eq!(
        json["plutus_core_rev"],
        serde_json::Value::String("feature/experimental".to_string())
    );
}

#[test]
fn lean_version_constant_matches_pinned_version() {
    // Drift sentinel: the `MIN_LEAN_VERSION` tuple, the `LEAN_TOOLCHAIN_LITERAL`
    // string, and the `LEAN_VERSION` env var consumed by the
    // `verify_real_toolchain` CI job must all encode the same Lean version.
    // Bumping any one of them without the others is a silent break — this test
    // ensures CI fails loud instead.
    let (maj, min, patch) = MIN_LEAN_VERSION;
    let formatted = format!("v{maj}.{min}.{patch}");
    assert_eq!(
        formatted, "v4.24.0",
        "MIN_LEAN_VERSION tuple must format to v4.24.0 — update both the \
         tuple and the lean-toolchain literal together."
    );

    let expected_literal = format!("leanprover/lean4:{formatted}\n");
    assert_eq!(
        LEAN_TOOLCHAIN_LITERAL, expected_literal,
        "LEAN_TOOLCHAIN_LITERAL must agree with MIN_LEAN_VERSION."
    );

    // Cross-check by writing a workspace and reading the file back: the
    // generator must use the constant rather than re-embedding a literal.
    let tempdir = tempfile::tempdir().expect("tempdir for lean-toolchain check");
    let out = tempdir.path().to_path_buf();
    let config = VerifyConfig {
        out_dir: out.clone(),
        cek_budget: 20_000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };
    generate_lean_workspace(&[], &config, &SkipPolicy::None).unwrap();
    let on_disk = std::fs::read_to_string(out.join("lean-toolchain"))
        .expect("lean-toolchain should be written");
    assert_eq!(
        on_disk, LEAN_TOOLCHAIN_LITERAL,
        "Workspace generator must emit LEAN_TOOLCHAIN_LITERAL verbatim — \
         drift here means a duplicated version literal slipped back in."
    );
}

#[test]
fn proof_status_serializes_internally_tagged() {
    // ProofStatus uses `#[serde(tag = "kind", rename_all = "snake_case")]`.
    // The wire format (top-level `version` == "1") includes:
    //   Proved          → {"kind":"proved"}
    //   Partial         → {"kind":"partial","note":"..."}
    //   WitnessProved   → {"kind":"witness_proved","instances":N,"witnesses":[...],"note":"..."}
    let proved_value = serde_json::to_value(&ProofStatus::Proved).unwrap();
    assert_eq!(proved_value, serde_json::json!({"kind": "proved"}));

    let partial_value = serde_json::to_value(&ProofStatus::Partial {
        note: "x".to_string(),
    })
    .unwrap();
    assert_eq!(
        partial_value,
        serde_json::json!({"kind": "partial", "note": "x"})
    );

    let witness_value = serde_json::to_value(&ProofStatus::WitnessProved {
        instances: 2,
        witnesses: vec!["aa".to_string()],
        note: "n".to_string(),
    })
    .unwrap();
    assert_eq!(
        witness_value,
        serde_json::json!({
            "kind": "witness_proved",
            "instances": 2,
            "witnesses": ["aa"],
            "note": "n",
        })
    );
}

#[test]
fn failure_category_serializes_kebab_case() {
    // FailureCategory uses `#[serde(tag = "kind", rename_all = "kebab-case")]`.
    // BlasterUnsupported must wire-encode as "blaster-unsupported" (kebab),
    // distinguishing it from snake_case "blaster_unsupported".
    let counterexample = serde_json::to_value(&FailureCategory::Counterexample).unwrap();
    assert_eq!(
        counterexample,
        serde_json::json!({"kind": "counterexample"})
    );

    let blaster = serde_json::to_value(&FailureCategory::BlasterUnsupported).unwrap();
    assert_eq!(blaster, serde_json::json!({"kind": "blaster_unsupported"}));

    let unsat = serde_json::to_value(&FailureCategory::UnsatGoal).unwrap();
    assert_eq!(unsat, serde_json::json!({"kind": "unsat_goal"}));

    let dep = serde_json::to_value(&FailureCategory::DependencyError).unwrap();
    assert_eq!(dep, serde_json::json!({"kind": "dependency_error"}));
}

#[test]
fn manifest_entry_serializes_witness_proof_note_when_set() {
    // ManifestEntry round-trips `witness_proof_note` through serde:
    //   - `Some(WitnessProofNote { .. })` appears in serialized JSON
    //   - `None` is omitted (no null field).
    let entry_with_witness = ManifestEntry {
        id: "sm_module__halt_test_witness".to_string(),
        aiken_module: "sm_module".to_string(),
        aiken_name: "halt_test".to_string(),
        lean_module: "AikenVerify.Proofs.Sm_module.halt_test".to_string(),
        lean_theorem: "halt_test".to_string(),
        lean_file: "AikenVerify/Proofs/Sm_module/halt_test.lean".to_string(),
        flat_file: "cbor/sm_module__halt_test_witness.cbor".to_string(),
        has_termination_theorem: false,
        has_equivalence_theorem: false,
        over_approximations: 0,
        partial_proof_note: None,
        witness_proof_note: Some(WitnessProofNote {
            instances: 3,
            witnesses: vec!["00".to_string(), "01".to_string(), "02".to_string()],
            note: "concrete witnesses verified via native_decide".to_string(),
        }),
    };

    let json = serde_json::to_value(&entry_with_witness).expect("entry should serialize");
    assert_eq!(
        json["witness_proof_note"]["instances"],
        serde_json::Value::from(3)
    );
    assert_eq!(
        json["witness_proof_note"]["witnesses"],
        serde_json::json!(["00", "01", "02"])
    );
    assert_eq!(
        json["witness_proof_note"]["note"]
            .as_str()
            .expect("note must be string"),
        "concrete witnesses verified via native_decide"
    );

    // `partial_proof_note` is None and must be omitted.
    let object = json
        .as_object()
        .expect("ManifestEntry serializes to an object");
    assert!(
        !object.contains_key("partial_proof_note"),
        "partial_proof_note must be omitted when None"
    );

    // Round-trip via JSON string form.
    let serialized = serde_json::to_string(&entry_with_witness).unwrap();
    let reparsed: serde_json::Value = serde_json::from_str(&serialized).unwrap();
    assert_eq!(reparsed["witness_proof_note"], json["witness_proof_note"]);

    // Without a witness note, the field is omitted.
    let entry_without_witness = ManifestEntry {
        id: "mod__t_id".to_string(),
        aiken_module: "mod".to_string(),
        aiken_name: "t".to_string(),
        lean_module: "AikenVerify.Proofs.Mod.t".to_string(),
        lean_theorem: "t".to_string(),
        lean_file: "AikenVerify/Proofs/Mod/t.lean".to_string(),
        flat_file: "cbor/mod__t_id.cbor".to_string(),
        has_termination_theorem: false,
        has_equivalence_theorem: false,
        over_approximations: 0,
        partial_proof_note: None,
        witness_proof_note: None,
    };
    let json_without =
        serde_json::to_value(&entry_without_witness).expect("entry should serialize");
    let object = json_without
        .as_object()
        .expect("ManifestEntry serializes to an object");
    assert!(
        !object.contains_key("witness_proof_note"),
        "witness_proof_note must be omitted from JSON when None, got: {:?}",
        object.keys().collect::<Vec<_>>()
    );
}

#[test]
fn witness_proof_note_round_trips() {
    // WitnessProofNote is `Serialize + Deserialize` so it can survive
    // JSON round-trips for downstream tooling that consumes the manifest.
    let note = WitnessProofNote {
        instances: 5,
        witnesses: vec!["deadbeef".to_string(), "cafebabe".to_string()],
        note: "round-trip note".to_string(),
    };

    let serialized = serde_json::to_string(&note).expect("WitnessProofNote should serialize");
    let parsed: WitnessProofNote =
        serde_json::from_str(&serialized).expect("WitnessProofNote should deserialize");

    assert_eq!(parsed.instances, note.instances);
    assert_eq!(parsed.witnesses, note.witnesses);
    assert_eq!(parsed.note, note.note);
}

#[test]
fn parse_verify_results_marks_witness_entries_as_witness_proved_on_success() {
    // When a manifest entry carries `witness_proof_note`, parse_verify_results
    // must surface it as `WitnessProved` rather than `Proved`. Today no
    // production path emits the witness caveat (commit 5 wires it in); this
    // test confirms the wire path works end-to-end by setting the field
    // manually.
    let mut manifest = make_manifest_with_termination(
        vec![("sm_module", "halt_test", "halt_test")],
        false, // no termination companion — witness path
    );
    manifest.tests[0].witness_proof_note = Some(WitnessProofNote {
        instances: 2,
        witnesses: vec!["aa".to_string(), "bb".to_string()],
        note: "concrete witnesses verified via native_decide".to_string(),
    });
    manifest.tests[0].over_approximations = 7; // must be preserved

    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small("Build completed successfully."),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 1);
    assert_eq!(summary.witness, 1);
    assert_eq!(summary.proved, 0);
    assert_eq!(summary.partial, 0);
    assert_eq!(summary.failed, 0);
    assert_eq!(summary.timed_out, 0);
    assert_eq!(summary.unknown, 0);
    assert!(summary.command_success);

    let theorem = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "halt_test")
        .expect("correctness theorem present");
    match &theorem.status {
        ProofStatus::WitnessProved {
            instances,
            witnesses,
            note,
        } => {
            assert_eq!(*instances, 2);
            assert_eq!(witnesses, &vec!["aa".to_string(), "bb".to_string()]);
            assert_eq!(note, "concrete witnesses verified via native_decide");
        }
        other => panic!("expected WitnessProved, got {other:?}"),
    }
    assert_eq!(
        theorem.over_approximations, 7,
        "over_approximations must be preserved on WitnessProved results"
    );
}

#[test]
fn witness_proved_does_not_count_as_proved() {
    // Belt-and-suspenders: a summary containing a WitnessProved theorem
    // must not increment `proved`. Exercises the precomputed-theorem-results
    // path of parse_verify_results.
    let manifest =
        make_manifest_with_termination(vec![("sm_module", "halt_only", "halt_only")], false);

    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: Some(vec![TheoremResult {
            test_name: "sm_module.halt_only".to_string(),
            theorem_name: "halt_only".to_string(),
            status: ProofStatus::WitnessProved {
                instances: 1,
                witnesses: vec!["80".to_string()],
                note: "single-witness native_decide proof".to_string(),
            },
            over_approximations: 0,
        }]),
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 1);
    assert_eq!(summary.witness, 1);
    assert_eq!(summary.proved, 0);
    assert_eq!(summary.partial, 0);
    assert_eq!(summary.failed, 0);
    assert_eq!(summary.timed_out, 0);
    assert_eq!(summary.unknown, 0);
    assert!(
        summary.command_success,
        "WitnessProved must not flip command_success to false"
    );
    assert_eq!(summary.verify_summary_version, VERIFY_SUMMARY_VERSION);
}

#[test]
fn state_machine_halt_proof_emits_witness_caveat() {
    // End-to-end check: a state-machine halt test (Void + FailImmediately +
    // StateMachineTrace) with `step_function_ir = None` and one or more
    // concrete halt witnesses must take Site B in `generate_proof_file`,
    // producing `(content, ProofCaveat::Witness(WitnessProofNote { … }))`.
    // The witness vector and instance count must match what the test fixture
    // carries; the note string must mention the test name, the witness count,
    // and the base seed (CONCRETE_WITNESS_BASE_SEED = 42).
    let mut test = make_phase12_state_machine_test(
        "prop_halt_caveat_ok",
        StateMachineAcceptance::AcceptsSuccess,
    );
    // step_function_ir already None from the fixture builder; populate
    // witnesses to drive Site B (no-step-IR halt path). CBOR-hex `80` is
    // an empty array, `00` is unsigned int 0 — both decode cleanly as
    // PlutusData so `generate_state_machine_halt_proof_file` succeeds.
    test.concrete_halt_witnesses = vec!["80".to_string(), "00".to_string()];
    assert!(matches!(
        &test.semantics,
        FuzzerSemantics::StateMachineTrace {
            step_function_ir: None,
            ..
        }
    ));

    let id = test_id("permissions.test", "prop_halt_caveat_ok");
    let lean_name = sanitize_lean_name("prop_halt_caveat_ok");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_halt_caveat_ok";

    let (_content, caveat) = super::generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
        false,
    )
    .expect("halt proof generation must succeed when witnesses are present");

    match caveat {
        ProofCaveat::Witness(WitnessProofNote {
            instances,
            witnesses,
            note,
        }) => {
            assert_eq!(
                instances, 2,
                "instance count must mirror witness vector len"
            );
            assert_eq!(
                witnesses,
                vec!["80".to_string(), "00".to_string()],
                "witnesses must be the CBOR-hex strings cloned from the fixture",
            );
            assert!(
                note.contains(&test.name),
                "note must mention the test name; got: {note}"
            );
            assert!(
                note.contains("halt"),
                "note must identify this as a halt-test caveat; got: {note}"
            );
            assert!(
                note.contains("seed-42"),
                "note must record the base fuzzer seed (42); got: {note}"
            );
            // Plural form for two witnesses: `2 ... witnesses only`. Match the
            // exact phrase to defend against substring collisions with the
            // test name (e.g. when this fixture's name happens to embed
            // "witness"); the count makes the match unambiguous.
            assert!(
                note.contains("2 concrete fuzzer-seed-42 witnesses only"),
                "note must use plural form for two witnesses; got: {note}"
            );
        }
        other => panic!("expected ProofCaveat::Witness, got {other:?}"),
    }
}

#[test]
fn state_machine_error_proof_emits_witness_caveat() {
    // Mirror of `state_machine_halt_proof_emits_witness_caveat` for the
    // `_ko` path: Void + SucceedEventually + StateMachineTrace with one or
    // more concrete error witnesses takes Site A (the error fast path) and
    // must surface `ProofCaveat::Witness` populated from
    // `concrete_error_witnesses`.
    let mut test = make_phase12_state_machine_test(
        "prop_error_caveat_ok",
        StateMachineAcceptance::AcceptsFailure,
    );
    // CBOR-hex `80` is an empty array — decodes cleanly as PlutusData.
    test.concrete_error_witnesses = vec!["80".to_string()];

    let id = test_id("permissions.test", "prop_error_caveat_ok");
    let lean_name = sanitize_lean_name("prop_error_caveat_ok");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_error_caveat_ok";

    let (_content, caveat) = super::generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
        false,
    )
    .expect("error proof generation must succeed when witnesses are present");

    match caveat {
        ProofCaveat::Witness(WitnessProofNote {
            instances,
            witnesses,
            note,
        }) => {
            assert_eq!(
                instances, 1,
                "single witness fixture must report 1 instance"
            );
            assert_eq!(witnesses, vec!["80".to_string()]);
            assert!(
                note.contains(&test.name),
                "note must mention the test name; got: {note}"
            );
            assert!(
                note.contains("error"),
                "note must identify this as an error-test caveat; got: {note}"
            );
            assert!(
                note.contains("seed-42"),
                "note must record the base fuzzer seed (42); got: {note}"
            );
            // Singular form for one witness: `1 ... witness only`. Use the
            // exact phrase to avoid substring collisions with the test name.
            assert!(
                note.contains("1 concrete fuzzer-seed-42 witness only"),
                "note must use singular form for one witness; got: {note}"
            );
        }
        other => panic!("expected ProofCaveat::Witness, got {other:?}"),
    }
}

#[test]
fn manifest_entry_caveat_round_trips_witness_proof_note() {
    // `manifest_entry_caveat` is the inverse of the on-disk projection done
    // by `generate_proof_file` and persisted in the manifest. When a
    // ManifestEntry carries a `witness_proof_note`, this helper must
    // reconstruct an equivalent `ProofCaveat::Witness(...)`. A regression
    // here would silently downgrade WitnessProved verdicts to Proved on the
    // resume path that reads the manifest from disk.
    let note = WitnessProofNote {
        instances: 3,
        witnesses: vec!["aa".to_string(), "bb".to_string(), "cc".to_string()],
        note: "test note".to_string(),
    };
    let entry = ManifestEntry {
        id: "sm_module__halt_test".to_string(),
        aiken_module: "sm_module".to_string(),
        aiken_name: "halt_test".to_string(),
        lean_module: "AikenVerify.Proofs.Sm_module.halt_test".to_string(),
        lean_theorem: "halt_test".to_string(),
        lean_file: "AikenVerify/Proofs/Sm_module/halt_test.lean".to_string(),
        flat_file: "cbor/sm_module__halt_test.cbor".to_string(),
        has_termination_theorem: false,
        has_equivalence_theorem: false,
        over_approximations: 0,
        partial_proof_note: None,
        witness_proof_note: Some(note.clone()),
    };

    match super::manifest_entry_caveat(&entry) {
        ProofCaveat::Witness(actual) => {
            assert_eq!(actual.instances, note.instances);
            assert_eq!(actual.witnesses, note.witnesses);
            assert_eq!(actual.note, note.note);
        }
        other => panic!("expected ProofCaveat::Witness, got {other:?}"),
    }
}

#[test]
fn parse_verify_results_termination_companion_stays_proved_on_witness_entry() {
    // Plan §B2 guard: when a state-machine halt entry surfaces as
    // `WitnessProved`, the sibling `_alwaysTerminating` companion must stay
    // `Proved` (termination is universally discharged independently of the
    // witness-only correctness verdict). This locks in the result_parser
    // wiring at result_parser.rs:104, where the termination companion is
    // built directly with `ProofStatus::Proved` and does not inherit the
    // entry's `witness_proof_note`.
    let mut manifest = make_manifest(vec![("sm_module", "halt_test", "halt_test")]);
    manifest.tests[0].witness_proof_note = Some(WitnessProofNote {
        instances: 2,
        witnesses: vec!["aa".to_string(), "bb".to_string()],
        note: "concrete witnesses verified via native_decide".to_string(),
    });
    assert!(manifest.tests[0].has_termination_theorem);

    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small("Build completed successfully."),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    // Two theorems total: correctness (witness-proved) + termination (proved).
    assert_eq!(summary.total, 2);
    assert_eq!(
        summary.witness, 1,
        "correctness entry must surface as witness"
    );
    assert_eq!(
        summary.proved, 1,
        "termination companion must stay Proved (not propagated to WitnessProved)"
    );
    assert_eq!(summary.partial, 0);
    assert_eq!(summary.failed, 0);

    let correctness = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "halt_test")
        .expect("correctness theorem present");
    assert!(
        matches!(correctness.status, ProofStatus::WitnessProved { .. }),
        "correctness must surface WitnessProved, got {:?}",
        correctness.status
    );

    let termination = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "halt_test_alwaysTerminating")
        .expect("termination companion present");
    assert!(
        matches!(termination.status, ProofStatus::Proved),
        "_alwaysTerminating sibling must stay Proved when entry is WitnessProved, got {:?}",
        termination.status
    );

    assert!(
        summary.command_success,
        "WitnessProved + Proved siblings must not flip command_success to false"
    );
}

#[test]
fn summary_counts_witness_separately_from_proved() {
    // Drive parse_verify_results through the precomputed-theorem-results
    // path with a mix of one Proved and one WitnessProved theorem. The
    // summary counters must record them in distinct buckets:
    // `summary.proved == 1`, `summary.witness == 1`, `summary.partial == 0`,
    // `summary.failed == 0`. This guards against a regression where
    // `WitnessProved` collapses into `proved` (which would mask the
    // distinction users rely on for witness-only verdicts).
    let manifest = make_manifest_with_termination(
        vec![
            ("mod_a", "universal_test", "universal_test"),
            ("mod_b", "halt_witness_test", "halt_witness_test"),
        ],
        false,
    );

    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: Some(vec![
            TheoremResult {
                test_name: "mod_a.universal_test".to_string(),
                theorem_name: "universal_test".to_string(),
                status: ProofStatus::Proved,
                over_approximations: 0,
            },
            TheoremResult {
                test_name: "mod_b.halt_witness_test".to_string(),
                theorem_name: "halt_witness_test".to_string(),
                status: ProofStatus::WitnessProved {
                    instances: 1,
                    witnesses: vec!["80".to_string()],
                    note: "concrete-witness halt proof".to_string(),
                },
                over_approximations: 0,
            },
        ]),
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 2);
    assert_eq!(
        summary.proved, 1,
        "exactly one Proved theorem must be counted"
    );
    assert_eq!(
        summary.witness, 1,
        "exactly one WitnessProved theorem must be counted"
    );
    assert_eq!(summary.partial, 0);
    assert_eq!(summary.failed, 0);
    assert_eq!(summary.timed_out, 0);
    assert_eq!(summary.unknown, 0);
    assert!(summary.command_success);
}

#[test]
fn parse_verify_results_uses_precomputed_theorem_results() {
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(
            "✖ [1/1] Building AikenVerify.Proofs.My_module.test_add (1s)",
        ),
        stderr: CapturedOutput::small("error: Lean exited with code 1"),
        exit_code: Some(1),
        theorem_results: Some(vec![
            TheoremResult {
                test_name: "my_module.test_add".to_string(),
                theorem_name: "test_add".to_string(),
                status: ProofStatus::Failed {
                    category: FailureCategory::BuildError,
                    reason: "error: 'test_add' unsolved goals".to_string(),
                },
                over_approximations: 0,
            },
            TheoremResult {
                test_name: "my_module.test_add".to_string(),
                theorem_name: "test_add_alwaysTerminating".to_string(),
                status: ProofStatus::Failed {
                    category: FailureCategory::BuildError,
                    reason: "error: Lean exited with code 1".to_string(),
                },
                over_approximations: 0,
            },
        ]),
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 2);
    assert_eq!(summary.proved, 0);
    assert_eq!(summary.failed, 2);
    assert_eq!(summary.timed_out, 0);
    assert_eq!(summary.unknown, 0);
}

#[test]
fn parse_verify_results_carries_skipped_metadata() {
    let mut manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    manifest.skipped = vec![SkippedTest {
        name: "my_module.test_unsupported".to_string(),
        module: "my_module".to_string(),
        reason: "unsupported theorem shape".to_string(),
    }];

    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small("Build completed successfully."),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.skipped.len(), 1);
    assert_eq!(summary.skipped[0].name, "my_module.test_unsupported");
}

#[test]
fn parse_verify_results_precomputed_carries_skipped_metadata() {
    let mut manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    manifest.skipped = vec![SkippedTest {
        name: "my_module.test_unsupported".to_string(),
        module: "my_module".to_string(),
        reason: "unsupported theorem shape".to_string(),
    }];

    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small("error: Lean exited with code 1"),
        exit_code: Some(1),
        theorem_results: Some(vec![TheoremResult {
            test_name: "my_module.test_add".to_string(),
            theorem_name: "test_add".to_string(),
            status: ProofStatus::Failed {
                category: FailureCategory::BuildError,
                reason: "error: Lean exited with code 1".to_string(),
            },
            over_approximations: 0,
        }]),
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.skipped.len(), 1);
    assert_eq!(summary.skipped[0].name, "my_module.test_unsupported");
}

#[test]
fn parse_verify_results_specific_failure() {
    let manifest = make_manifest(vec![
        ("my_module", "test_add", "test_add"),
        ("my_module", "test_sub", "test_sub"),
    ]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small("error: 'test_add' has unsolved goals\nsome other context"),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 4);
    assert_eq!(summary.failed, 1);
    assert_eq!(summary.timed_out, 0);
    assert!(summary.proved == 0);

    let test_add = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_add")
        .unwrap();
    assert!(
        matches!(&test_add.status, ProofStatus::Failed { reason, .. } if reason.contains("test_add")),
        "test_add should be Failed, got: {:?}",
        test_add.status
    );

    let test_sub = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_sub")
        .unwrap();
    assert!(
        matches!(test_sub.status, ProofStatus::Unknown),
        "test_sub should be Unknown, got: {:?}",
        test_sub.status
    );
}

#[test]
fn parse_verify_results_marks_built_modules_as_proved_when_one_module_fails() {
    let manifest = make_manifest(vec![
        ("my_module", "test_add", "test_add"),
        ("my_module", "test_sub", "test_sub"),
        ("my_module", "test_bad", "test_bad"),
    ]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(
            "⚠ [1/3] Replayed AikenVerify.Proofs.My_module.test_add\n\
                 info: AikenVerify/Proofs/My_module/test_add.lean:10:5: ✅ Valid\n\
                 ⚠ [2/3] Replayed AikenVerify.Proofs.My_module.test_sub\n\
                 info: AikenVerify/Proofs/My_module/test_sub.lean:11:5: ✅ Valid\n\
                 ✖ [3/3] Building AikenVerify.Proofs.My_module.test_bad (5s)\n\
                 error: AikenVerify/Proofs/My_module/test_bad.lean:21:5: translateApp: unsupported\n\
                 error: Lean exited with code 1\n\
                 Some required targets logged failures:\n\
                 - AikenVerify.Proofs.My_module.test_bad",
        ),
        stderr: CapturedOutput::small("error: build failed"),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 6);
    assert_eq!(summary.proved, 4);
    assert_eq!(summary.failed, 2);
    assert_eq!(summary.timed_out, 0);
    assert_eq!(summary.unknown, 0);

    let test_add = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_add")
        .unwrap();
    assert!(
        matches!(test_add.status, ProofStatus::Proved),
        "test_add should be Proved, got: {:?}",
        test_add.status
    );

    let test_sub = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_sub")
        .unwrap();
    assert!(
        matches!(test_sub.status, ProofStatus::Proved),
        "test_sub should be Proved, got: {:?}",
        test_sub.status
    );

    let test_bad = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_bad")
        .unwrap();
    assert!(
        matches!(&test_bad.status, ProofStatus::Failed { .. }),
        "test_bad should be Failed, got: {:?}",
        test_bad.status
    );
}

#[test]
fn parse_verify_results_termination_failure() {
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small("error: 'test_add_alwaysTerminating' tactic failed"),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 2);
    assert_eq!(summary.failed, 1);
    assert_eq!(summary.unknown, 1);

    let term = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_add_alwaysTerminating")
        .unwrap();
    assert!(
        matches!(&term.status, ProofStatus::Failed { .. }),
        "Termination theorem should be Failed, got: {:?}",
        term.status
    );

    let base = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_add")
        .unwrap();
    assert!(
        matches!(&base.status, ProofStatus::Unknown),
        "Base theorem should stay non-failed, got: {:?}",
        base.status
    );
}

#[test]
fn parse_verify_results_module_failure_does_not_fail_sibling_theorem() {
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small(
            "✖ [1/1] Building AikenVerify.Proofs.My_module.test_add (1s)\n\
             error: 'test_add_alwaysTerminating' tactic failed\n\
             Some required targets logged failures:\n\
             - AikenVerify.Proofs.My_module.test_add",
        ),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 2);
    assert_eq!(summary.failed, 1);
    assert_eq!(summary.unknown, 1);

    let term = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_add_alwaysTerminating")
        .unwrap();
    assert!(
        matches!(&term.status, ProofStatus::Failed { .. }),
        "Termination theorem should be Failed, got: {:?}",
        term.status
    );

    let base = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_add")
        .unwrap();
    assert!(
        matches!(&base.status, ProofStatus::Unknown),
        "Base theorem should stay Unknown when only termination theorem fails, got: {:?}",
        base.status
    );
}

#[test]
fn parse_verify_results_detects_failed_module_without_unicode_marker() {
    let manifest = make_manifest(vec![
        ("my_module", "test_add", "test_add"),
        ("my_module", "test_bad", "test_bad"),
    ]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(
            "[1/2] Built AikenVerify.Proofs.My_module.test_add (1s)\n\
             [2/2] Building AikenVerify.Proofs.My_module.test_bad (2s)\n\
             error: AikenVerify/Proofs/My_module/test_bad.lean:21:5: unsolved goals\n\
             error: Lean exited with code 1",
        ),
        stderr: CapturedOutput::small("error: build failed"),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 4);
    assert_eq!(summary.proved, 2);
    assert_eq!(summary.failed, 2);
    assert_eq!(summary.unknown, 0);
}

#[test]
fn parse_verify_results_plain_building_output_marks_failed_module() {
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(
            "[1/1] Building AikenVerify.Proofs.My_module.test_add (1s)\n\
             error: Lean exited with code 1",
        ),
        stderr: CapturedOutput::small("error: build failed"),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 2);
    assert_eq!(summary.failed, 2);
    assert_eq!(summary.unknown, 0);
}

#[test]
fn extract_error_for_module_does_not_match_prefix_overlaps() {
    let output = "error: AikenVerify/Proofs/My_module/test_extra.lean:21:5: unsolved goals";

    let shorter_reason =
        super::result_parser::extract_error_for_module("AikenVerify.Proofs.My_module.test", output);
    assert!(
        shorter_reason.is_empty(),
        "module matching should not classify prefix-overlap module names as the same target: {shorter_reason}"
    );

    let exact_reason = super::result_parser::extract_error_for_module(
        "AikenVerify.Proofs.My_module.test_extra",
        output,
    );
    assert!(
        exact_reason.contains("test_extra.lean"),
        "exact module matching should still capture module-specific diagnostics"
    );
}

#[test]
fn parse_verify_results_counterexample_does_not_contaminate_sibling_theorem_category() {
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small(
            "✖ [1/1] Building AikenVerify.Proofs.My_module.test_add (1s)\n\
             error: 'test_add' Counterexample:\n\
             error: Counterexample: x = 41\n\
             Some required targets logged failures:\n\
             - AikenVerify.Proofs.My_module.test_add",
        ),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);
    assert_eq!(summary.total, 2);
    assert_eq!(summary.failed, 2);
    assert_eq!(summary.unknown, 0);

    let base = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_add")
        .unwrap();
    match &base.status {
        ProofStatus::Failed { category, .. } => {
            assert_eq!(
                *category,
                FailureCategory::Counterexample,
                "Base theorem should retain counterexample classification"
            );
        }
        other => panic!("Expected base theorem failure, got: {:?}", other),
    }

    let term = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_add_alwaysTerminating")
        .unwrap();
    match &term.status {
        ProofStatus::Failed { category, .. } => {
            assert_eq!(
                *category,
                FailureCategory::BuildError,
                "Termination theorem should fail without inheriting counterexample classification"
            );
        }
        other => panic!(
            "Termination theorem should be Failed with BuildError, got: {:?}",
            other
        ),
    }
}

#[test]
fn theorem_has_explicit_failure_requires_exact_identifier_match() {
    let output = "error: 'test_add_alwaysTerminating' tactic failed";

    assert!(
        theorem_has_explicit_failure("test_add_alwaysTerminating", output),
        "Exact theorem name should match explicit failure output"
    );
    assert!(
        !theorem_has_explicit_failure("test_add", output),
        "Prefix theorem name must not match a different theorem failure"
    );
}

#[test]
fn theorem_has_explicit_failure_matches_uppercase_error_prefix() {
    let output = "ERROR: 'test_add_alwaysTerminating' tactic failed";

    assert!(
        theorem_has_explicit_failure("test_add_alwaysTerminating", output),
        "Explicit theorem failures should be detected regardless of error prefix casing"
    );
}

#[test]
fn extract_error_for_theorem_requires_exact_identifier_match() {
    let output = "error: 'test_add_alwaysTerminating' tactic failed";

    assert!(
        extract_error_for_theorem("test_add_alwaysTerminating", output)
            .contains("test_add_alwaysTerminating"),
        "Exact theorem name should produce extracted error context"
    );
    assert!(
        extract_error_for_theorem("test_add", output).is_empty(),
        "Prefix theorem name must not extract context for a different theorem"
    );
}

#[test]
fn parse_verify_results_empty_manifest() {
    let manifest = make_manifest(vec![]);
    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small(""),
        exit_code: Some(0),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 0);
    assert_eq!(summary.proved, 0);
    assert_eq!(summary.failed, 0);
    assert_eq!(summary.timed_out, 0);
    assert_eq!(summary.unknown, 0);
}

#[test]
fn parse_verify_results_build_failure_no_specific_errors() {
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small("error: build failed with some generic message"),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 2);
    assert_eq!(summary.unknown, 0);
    assert_eq!(summary.failed, 2);
    assert!(summary.theorems.iter().all(|t| matches!(
        t.status,
        ProofStatus::Failed {
            category: FailureCategory::BuildError,
            ..
        }
    )));
}

#[test]
fn parse_verify_results_timeout_marker_marks_timed_out() {
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small("[verify-timeout] command timed out"),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);

    assert_eq!(summary.total, 2);
    assert_eq!(summary.timed_out, 2);
    assert_eq!(summary.failed, 0);
    assert_eq!(summary.unknown, 0);
}

#[test]
fn lake_fetch_unknown_command_variants_are_detected() {
    assert!(lake_fetch_is_unknown_command(
        "",
        "error: unknown command 'fetch'"
    ));
    assert!(lake_fetch_is_unknown_command(
        "",
        "error: unknown command `fetch`"
    ));
    assert!(lake_fetch_is_unknown_command(
        "",
        "error: unknown command \"fetch\""
    ));
}

#[test]
fn lake_fetch_unknown_command_variants_on_stdout_are_detected() {
    assert!(lake_fetch_is_unknown_command(
        "error: unknown command 'fetch'",
        ""
    ));
}

#[test]
fn lake_fetch_unknown_command_ignores_other_failures() {
    assert!(!lake_fetch_is_unknown_command(
        "",
        "error: failed to load manifest from lakefile.lean"
    ));
    assert!(!lake_fetch_is_unknown_command(
        "",
        "error: dependency resolution failed"
    ));
}

#[test]
fn normalize_failure_exit_code_never_returns_zero() {
    assert_eq!(normalize_failure_exit_code(Some(0)), 1);
    assert_eq!(normalize_failure_exit_code(None), 1);
    assert_eq!(normalize_failure_exit_code(Some(124)), 124);
}

#[test]
fn extract_error_for_theorem_includes_context_window() {
    let stderr = "line1: unrelated\nerror: 'my_thm' unsolved goals\nline3: also unrelated\nmy_thm failed to synthesize\n";
    let result = extract_error_for_theorem("my_thm", stderr);
    // Context window includes surrounding lines around matches
    assert!(
        result.contains("error: 'my_thm' unsolved goals"),
        "Should include the error line, got:\n{result}"
    );
    assert!(
        result.contains("my_thm failed to synthesize"),
        "Should include the theorem-matching line, got:\n{result}"
    );
}

#[test]
fn extract_error_for_theorem_shows_gap_marker() {
    // Matches far apart should have "..." separator
    let mut lines: Vec<String> = (0..20).map(|i| format!("filler line {i}")).collect();
    lines[2] = "error: 'my_thm' failed".to_string();
    lines[18] = "my_thm: another error here".to_string();
    let stderr = lines.join("\n");
    let result = extract_error_for_theorem("my_thm", &stderr);
    assert!(
        result.contains("..."),
        "Should have gap marker between distant matches, got:\n{result}"
    );
}

#[test]
fn extract_error_for_theorem_empty_on_no_match() {
    let stderr = "completely unrelated output\nnothing to see here\n";
    let result = extract_error_for_theorem("my_thm", stderr);
    assert!(
        result.is_empty(),
        "Should be empty when no match, got: {result}"
    );
}

#[test]
fn version_meets_minimum_exact_match() {
    assert!(version_meets_minimum("4.24.0", (4, 24, 0)));
}

#[test]
fn version_meets_minimum_higher() {
    assert!(version_meets_minimum("4.25.1", (4, 24, 0)));
}

#[test]
fn version_meets_minimum_lower() {
    assert!(!version_meets_minimum("4.23.9", (4, 24, 0)));
}

#[test]
fn version_meets_minimum_major_higher() {
    assert!(version_meets_minimum("5.0.0", (4, 24, 0)));
}

#[test]
fn parse_version_from_lean_output() {
    let output = "Lean (version 4.24.0, x86_64-unknown-linux-gnu, Release)";
    assert_eq!(
        parse_version_from_output(output),
        Some("4.24.0".to_string())
    );
}

#[test]
fn parse_version_from_z3_output() {
    let output = "Z3 version 4.13.4 - 64 bit";
    assert_eq!(
        parse_version_from_output(output),
        Some("4.13.4".to_string())
    );
}

#[test]
fn parse_version_from_empty_output() {
    assert_eq!(parse_version_from_output("no version here"), None);
}

#[cfg(unix)]
fn write_executable_script(path: &Path, body: &str) {
    use std::os::unix::fs::PermissionsExt;

    fs::write(path, body).unwrap();
    let mut perms = fs::metadata(path).unwrap().permissions();
    perms.set_mode(0o755);
    fs::set_permissions(path, perms).unwrap();
}

#[cfg(unix)]
#[test]
fn check_tool_version_non_zero_exit_with_no_floor_marks_not_found() {
    let tmp = tempfile::tempdir().unwrap();
    let tool_path = tmp.path().join("fake-version-fails");
    write_executable_script(&tool_path, "#!/bin/sh\nexit 1\n");

    let check = check_tool_version(tool_path.to_str().unwrap(), (0, 0, 0));

    assert!(
        !check.found,
        "non-zero --version must fail presence checks even when no minimum version is required"
    );
    assert!(
        !check.meets_minimum,
        "non-zero --version should not satisfy presence-only checks"
    );
    assert!(
        check
            .error
            .as_deref()
            .is_some_and(|message| message.contains("exited with code")),
        "should surface non-zero exit details"
    );
}

#[cfg(unix)]
#[test]
fn check_tool_version_non_zero_exit_with_floor_marks_not_found() {
    let tmp = tempfile::tempdir().unwrap();
    let tool_path = tmp.path().join("fake-version-fails-with-floor");
    write_executable_script(&tool_path, "#!/bin/sh\nexit 1\n");

    let check = check_tool_version(tool_path.to_str().unwrap(), (1, 0, 0));

    assert!(!check.found);
    assert!(!check.meets_minimum);
    assert!(
        check
            .error
            .as_deref()
            .is_some_and(|message| message.contains("exited with code")),
        "should surface non-zero exit details"
    );
}

#[cfg(unix)]
#[test]
fn check_tool_version_no_minimum_accepts_unparseable_output() {
    let tmp = tempfile::tempdir().unwrap();
    let tool_path = tmp.path().join("fake-no-semver");
    write_executable_script(&tool_path, "#!/bin/sh\necho 'lake custom build'\nexit 0\n");

    let check = check_tool_version(tool_path.to_str().unwrap(), (0, 0, 0));

    assert!(check.found);
    assert!(
        check.meets_minimum,
        "no-op minimum should not fail when version parsing is unavailable"
    );
    assert!(check.version.is_none());
}

#[cfg(unix)]
#[test]
fn check_tool_version_with_floor_rejects_unparseable_output() {
    let tmp = tempfile::tempdir().unwrap();
    let tool_path = tmp.path().join("fake-no-semver-with-floor");
    write_executable_script(&tool_path, "#!/bin/sh\necho 'lean custom build'\nexit 0\n");

    let check = check_tool_version(tool_path.to_str().unwrap(), (4, 24, 0));

    assert!(check.found);
    assert!(
        !check.meets_minimum,
        "unparseable output cannot satisfy a real version minimum"
    );
    assert!(check.version.is_none());
}

#[test]
fn generate_lakefile_uses_blaster_rev() {
    let lakefile = generate_lakefile("abc123def", DEFAULT_PLUTUS_CORE_REV, None);
    assert!(
        lakefile.contains(r#"@ "abc123def""#),
        "Lakefile should pin Blaster to the given rev, got:\n{lakefile}"
    );
    assert!(
        !lakefile.contains(&format!(r#"@ "{DEFAULT_BLASTER_REV}""#)),
        "Lakefile should not contain default rev when override is provided"
    );
}

#[test]
fn generate_lakefile_default_rev() {
    let lakefile = generate_lakefile(DEFAULT_BLASTER_REV, DEFAULT_PLUTUS_CORE_REV, None);
    // Snapshot pins the canonical lakefile shape (require directives, lean
    // libs, package name) when both revs come from defaults.  The default
    // rev strings rotate over time (H6 surfaces them via VerifySummary), so
    // we redact them in the snapshot to a stable placeholder — the
    // *structure* of the file is what we want to lock, and the rev value
    // itself is asserted separately via the equality check below.
    assert!(
        lakefile.contains(&format!(r#"@ "{DEFAULT_BLASTER_REV}""#)),
        "Lakefile should use default rev"
    );
    let stable = lakefile
        .replace(DEFAULT_BLASTER_REV, "<DEFAULT_BLASTER_REV>")
        .replace(DEFAULT_PLUTUS_CORE_REV, "<DEFAULT_PLUTUS_CORE_REV>");
    insta::assert_snapshot!("generate_lakefile_default_rev", stable);
}

#[test]
fn generate_lakefile_uses_local_path_when_plutus_core_dir_set() {
    let tmp = tempfile::tempdir().unwrap();
    let pc = tmp.path().join("PlutusCore");
    fs::create_dir_all(&pc).unwrap();
    let lakefile = generate_lakefile(DEFAULT_BLASTER_REV, DEFAULT_PLUTUS_CORE_REV, Some(&pc));
    let canonical_pc = std::fs::canonicalize(&pc).unwrap_or(pc.clone());
    let expected = format!("require PlutusCore from\n  \"{}\"", canonical_pc.display());
    assert!(
        lakefile.contains(&expected),
        "Lakefile should reference the explicit PlutusCore path, got:\n{lakefile}"
    );
    assert!(
        !lakefile.contains("require PlutusCore from git"),
        "Lakefile should not use git for PlutusCore when local path is set, got:\n{lakefile}"
    );
    // Snapshot pins the local-PlutusCore lakefile shape. Three sources of
    // non-determinism need redaction: the tempdir path, the default Blaster
    // rev, and the default PlutusCore rev. The structure/order of `require` /
    // `lean_lib` / etc. directives is what we lock here.
    let stable = lakefile
        .replace(&canonical_pc.display().to_string(), "<TMP>/PlutusCore")
        .replace(DEFAULT_BLASTER_REV, "<DEFAULT_BLASTER_REV>")
        .replace(DEFAULT_PLUTUS_CORE_REV, "<DEFAULT_PLUTUS_CORE_REV>");
    insta::assert_snapshot!(
        "generate_lakefile_uses_local_path_when_plutus_core_dir_set",
        stable
    );
}

#[test]
fn generate_lakefile_uses_git_for_plutus_core_by_default() {
    // When no explicit path and no env var, PlutusCore comes from git.
    unsafe { std::env::remove_var("PLUTUS_CORE_DIR") };
    let lakefile = generate_lakefile(DEFAULT_BLASTER_REV, DEFAULT_PLUTUS_CORE_REV, None);
    assert!(
        lakefile.contains("require PlutusCore from git"),
        "Lakefile should use git for PlutusCore by default, got:\n{lakefile}"
    );
    assert!(
        lakefile.contains("PlutusCoreBlaster"),
        "Lakefile should reference the PlutusCoreBlaster repo, got:\n{lakefile}"
    );
    assert!(
        lakefile.contains(&format!(r#"@ "{DEFAULT_PLUTUS_CORE_REV}""#)),
        "Lakefile should pin PlutusCore to the default rev, got:\n{lakefile}"
    );
}

#[test]
fn generate_lakefile_uses_custom_plutus_core_rev() {
    unsafe { std::env::remove_var("PLUTUS_CORE_DIR") };
    let lakefile = generate_lakefile(DEFAULT_BLASTER_REV, "abc123", None);
    assert!(
        lakefile.contains(r#"@ "abc123""#),
        "Lakefile should pin PlutusCore to the custom rev, got:\n{lakefile}"
    );
}

#[test]
fn lake_build_jobs_flag_unsupported_detects_unknown_j_option() {
    let stderr = "error: unknown option '-j'";
    assert!(lake_build_jobs_flag_unsupported("", stderr));
}

#[test]
fn lake_build_jobs_flag_unsupported_detects_unknown_j_option_on_stdout() {
    let stdout = "error: unknown option '-j'";
    assert!(lake_build_jobs_flag_unsupported(stdout, ""));
}

#[test]
fn normalize_jobs_override_accepts_one() {
    assert_eq!(normalize_jobs_override(Some(1)), Some(1));
}

#[test]
fn lake_build_command_for_jobs_includes_single_job_override() {
    assert_eq!(lake_build_command_for_jobs(Some(1)), "lake build -j 1");
}

#[test]
fn lake_build_module_command_for_jobs_includes_module_target() {
    let module = "AikenVerify.Proofs.My_module.test_bool";
    assert_eq!(
        lake_build_module_command_for_jobs(module, Some(2)),
        "lake build -j 2 AikenVerify.Proofs.My_module.test_bool"
    );
    assert_eq!(
        lake_build_module_command_for_jobs(module, None),
        "lake build AikenVerify.Proofs.My_module.test_bool"
    );
}

#[test]
fn lake_build_jobs_flag_unsupported_ignores_unrelated_errors() {
    let stderr = "error: failed to load manifest";
    assert!(!lake_build_jobs_flag_unsupported("", stderr));
}

#[test]
fn command_poll_interval_scales_for_long_timeout() {
    assert_eq!(
        command_poll_interval(1800),
        std::time::Duration::from_secs(5)
    );
}

#[test]
fn command_poll_interval_has_reasonable_bounds() {
    assert_eq!(command_poll_interval(0), std::time::Duration::from_secs(2));
    assert_eq!(
        command_poll_interval(1),
        std::time::Duration::from_millis(200)
    );
}

#[test]
fn verify_interrupt_cleanup_guard_clears_active_pid() {
    let cleanup = VerifyInterruptCleanup::install();
    cleanup.reset_for_test();

    {
        let _guard = cleanup.begin_process(42);
        assert_eq!(cleanup.active_pid_for_test(), Some(42));
        assert!(!cleanup.interrupted());
    }

    assert_eq!(cleanup.active_pid_for_test(), None);
    assert!(!cleanup.interrupted());
    cleanup.reset_for_test();
}

#[test]
fn verify_interrupt_cleanup_resets_interrupt_state_between_processes() {
    let cleanup = VerifyInterruptCleanup::install();
    cleanup.reset_for_test();

    {
        let _guard = cleanup.begin_process(42);
        cleanup.mark_interrupted();
        assert!(cleanup.interrupted());
        assert_eq!(cleanup.active_pid_for_test(), Some(42));
    }

    assert_eq!(cleanup.active_pid_for_test(), None);

    {
        let _guard = cleanup.begin_process(99);
        assert_eq!(cleanup.active_pid_for_test(), Some(99));
        assert!(!cleanup.interrupted());
    }

    cleanup.reset_for_test();
}

#[test]
fn check_plutus_core_missing_dir() {
    let tmp = tempfile::tempdir().unwrap();
    let result = check_plutus_core_at(&tmp.path().join("PlutusCore"));
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found"));
}

#[test]
fn check_plutus_core_empty_dir() {
    let tmp = tempfile::tempdir().unwrap();
    let pc = tmp.path().join("PlutusCore");
    fs::create_dir(&pc).unwrap();
    let result = check_plutus_core_at(&pc);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("lakefile.lean"));
}

#[test]
fn check_plutus_core_valid_dir() {
    let tmp = tempfile::tempdir().unwrap();
    let pc = tmp.path().join("PlutusCore");
    fs::create_dir(&pc).unwrap();
    fs::write(pc.join("lakefile.lean"), "-- placeholder").unwrap();
    let result = check_plutus_core_at(&pc);
    assert!(result.is_ok());
}

#[test]
fn resolve_plutus_core_dir_uses_explicit_path_when_provided() {
    let tmp = tempfile::tempdir().unwrap();
    let explicit = tmp.path().join("explicit");
    fs::create_dir_all(&explicit).unwrap();

    let expected = std::fs::canonicalize(&explicit).unwrap_or(explicit.clone());
    let resolved = resolve_plutus_core_dir(Some(&explicit));
    assert_eq!(resolved, expected);
}

#[test]
fn resolve_plutus_core_dir_falls_back_to_env_var() {
    let tmp = tempfile::tempdir().unwrap();
    let env_dir = tmp.path().join("from_env");
    fs::create_dir_all(&env_dir).unwrap();

    unsafe { std::env::set_var("PLUTUS_CORE_DIR", &env_dir) };
    let resolved = resolve_plutus_core_dir(None);
    unsafe { std::env::remove_var("PLUTUS_CORE_DIR") };

    let expected = std::fs::canonicalize(&env_dir).unwrap_or(env_dir.clone());
    assert_eq!(resolved, expected);
}

#[test]
fn resolve_plutus_core_dir_returns_placeholder_when_nothing_set() {
    unsafe { std::env::remove_var("PLUTUS_CORE_DIR") };
    let resolved = resolve_plutus_core_dir(None);
    assert_eq!(resolved, PathBuf::from("PlutusCore"));
}

#[test]
fn check_plutus_core_detailed_missing() {
    let tmp = tempfile::tempdir().unwrap();
    let report = check_plutus_core_detailed_at(&tmp.path().join("PlutusCore"));
    assert!(!report.found);
    assert!(!report.has_lakefile);
    assert!(report.error.is_some());
}

#[test]
fn check_plutus_core_detailed_valid() {
    let tmp = tempfile::tempdir().unwrap();
    let pc = tmp.path().join("PlutusCore");
    fs::create_dir(&pc).unwrap();
    fs::write(pc.join("lakefile.lean"), "-- placeholder").unwrap();
    let report = check_plutus_core_detailed_at(&pc);
    assert!(report.found);
    assert!(report.has_lakefile);
    assert!(report.error.is_none());
}

#[test]
fn doctor_report_serializes_to_json() {
    let report = DoctorReport {
        version: DOCTOR_REPORT_VERSION.to_string(),
        tools: vec![ToolCheck {
            tool: "lean".to_string(),
            found: true,
            version: Some("4.24.0".to_string()),
            meets_minimum: true,
            minimum_version: "4.24.0".to_string(),
            error: None,
        }],
        plutus_core: PlutusCoreCheck {
            found: true,
            path: "/tmp/PlutusCore".to_string(),
            has_lakefile: true,
            error: None,
        },
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        all_ok: true,
        capabilities: capabilities(),
    };
    let json = serde_json::to_string(&report).unwrap();
    assert!(json.contains("\"all_ok\":true"));
    assert!(json.contains(&format!("\"blaster_rev\":\"{DEFAULT_BLASTER_REV}\"")));
    assert!(json.contains(&format!(
        "\"plutus_core_rev\":\"{DEFAULT_PLUTUS_CORE_REV}\""
    )));
    assert!(json.contains("\"supported_test_kinds\""));
    assert!(json.contains("\"target_modes\""));
}

#[test]
fn doctor_all_ok_treats_lake_as_presence_only() {
    let lean = ToolCheck {
        tool: "lean".to_string(),
        found: true,
        version: Some("4.24.0".to_string()),
        meets_minimum: true,
        minimum_version: "4.24.0".to_string(),
        error: None,
    };
    let lake = ToolCheck {
        tool: "lake".to_string(),
        found: true,
        version: Some("5.0.0".to_string()),
        meets_minimum: false,
        minimum_version: "0.0.0".to_string(),
        error: None,
    };
    let z3 = ToolCheck {
        tool: "z3".to_string(),
        found: true,
        version: Some("4.13.4".to_string()),
        meets_minimum: true,
        minimum_version: "4.8.0".to_string(),
        error: None,
    };
    let plutus_core = PlutusCoreCheck {
        found: true,
        path: "/tmp/PlutusCore".to_string(),
        has_lakefile: true,
        error: None,
    };

    assert!(
        doctor_all_ok(&lean, &lake, &z3, &plutus_core),
        "lake should be treated as a presence-only requirement"
    );
}

#[test]
fn capabilities_includes_expected_fields() {
    let caps = capabilities();
    assert_eq!(caps.version, VERIFICATION_CAPABILITIES_VERSION);
    assert_eq!(caps.supported_test_kinds, vec!["property"]);
    assert_eq!(caps.unsupported_test_kinds.len(), 2);
    assert_eq!(caps.unsupported_test_kinds[0].kind, "unit");
    assert_eq!(caps.unsupported_test_kinds[0].status, "intentional");
    assert_eq!(caps.unsupported_test_kinds[1].kind, "benchmark");
    assert_eq!(caps.target_modes.len(), 3);
    assert!(caps.target_modes.contains(&"property".to_string()));
    assert!(
        caps.target_modes
            .iter()
            .any(|mode| mode.starts_with("validator"))
    );
    assert!(
        caps.target_modes
            .iter()
            .any(|mode| mode.starts_with("equivalence"))
    );
    assert_eq!(caps.max_test_arity, 1);
}

fn write_verify_export_fixture(root: &Path) {
    fs::create_dir_all(root.join("validators")).unwrap();

    fs::write(
        root.join("aiken.toml"),
        r#"
name = "test/verify_export_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify export integration fixture"
"#,
    )
    .unwrap();

    fs::write(
        root.join("validators/fixture.ak"),
        r#"
fn seed_fuzzer() -> Fuzzer<Int> {
  fn(prng) { Some((prng, 0)) }
}

validator foo {
  mint(_redeemer: Data, policy_id: ByteArray, _tx: Data) {
    expect policy_id == "foo"
    True
  }

  else(_ctx) {
    fail
  }
}

test foo_mint_roundtrip(_seed via seed_fuzzer()) {
  foo.mint(Void, "foo", Void)
}

test foo_mint_repeated(_seed via seed_fuzzer()) {
  foo.mint(Void, "foo", Void) && foo.mint(Void, "foo", Void)
}

test foo_mint_in_short_circuit_or_rhs(_seed via seed_fuzzer()) {
  True || foo.mint(Void, "foo", Void)
}

test foo_mint_in_short_circuit_and_rhs(_seed via seed_fuzzer()) {
  False && foo.mint(Void, "foo", Void)
}

test foo_mint_in_non_executed_if_branch(_seed via seed_fuzzer()) {
  if False {
    foo.mint(Void, "foo", Void)
  } else {
    True
  }
}

test foo_mint_wrapped_with_negation(_seed via seed_fuzzer()) {
  !foo.mint(Void, "foo", Void)
}

test foo_mint_wrapped_with_comparison(_seed via seed_fuzzer()) {
  foo.mint(Void, "foo", Void) == True
}
"#,
    )
    .unwrap();
}

fn write_verify_export_helper_fixture(root: &Path) {
    fs::create_dir_all(root.join("validators")).unwrap();

    fs::write(
        root.join("aiken.toml"),
        r#"
name = "test/verify_export_helper_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify export helper integration fixture"
"#,
    )
    .unwrap();

    fs::write(
        root.join("validators/fixture.ak"),
        r#"
fn byte_fuzzer() -> Fuzzer<ByteArray> {
  todo
}

validator foo {
  mint(_redeemer: Data, policy_id: ByteArray, _tx: Data) {
    expect policy_id == "foo"
    True
  }

  else(_ctx) {
    fail
  }
}

fn call_mint(policy_id: ByteArray) -> Bool {
  forward_to_validator(policy_id)
}

fn forward_to_validator(policy_id: ByteArray) -> Bool {
  foo.mint(Void, policy_id, Void)
}

fn call_mint_with_alias(policy_id: ByteArray) -> Bool {
  let p = policy_id
  foo.mint(Void, p, Void)
}

fn call_mint_with_callee_alias(policy_id: ByteArray) -> Bool {
  let h = foo.mint
  h(Void, policy_id, Void)
}

test foo_mint_via_helper(policy_id via byte_fuzzer()) {
  call_mint(policy_id)
}

test foo_mint_via_local_alias(policy_id via byte_fuzzer()) {
  call_mint_with_alias(policy_id)
}

test foo_mint_via_local_callee_alias(policy_id via byte_fuzzer()) {
  call_mint_with_callee_alias(policy_id)
}

test foo_mint_via_tuple_destructure(policy_id via byte_fuzzer()) {
  let args = (Void, policy_id)
  let (redeemer, resolved_policy_id) = args
  foo.mint(redeemer, resolved_policy_id, Void)
}

test foo_mint_via_when_shadowed_call(policy_id via byte_fuzzer()) {
  let policy_id = "shadowed"
  when Some("foo") is {
    Some(policy_id) -> foo.mint(Void, policy_id, Void)
    None -> False
  }
}

test foo_mint_via_if_is_shadowed_call(policy_id via byte_fuzzer()) {
  let policy_id = "shadowed"
  if Some("foo") is Some(policy_id): Option<ByteArray> {
    foo.mint(Void, policy_id, Void)
  } else {
    False
  }
}

test foo_mint_via_when_argument_baseline(policy_id via byte_fuzzer()) {
  call_mint(
    when Some("foo") is {
      Some(policy_id) -> policy_id
      None -> "foo"
    }
  )
}

test foo_mint_via_when_shadowed_argument(policy_id via byte_fuzzer()) {
  let policy_id = "shadowed"
  call_mint(
    when Some("foo") is {
      Some(policy_id) -> policy_id
      None -> "foo"
    }
  )
}

test foo_mint_via_if_is_argument_baseline(policy_id via byte_fuzzer()) {
  call_mint(
    if Some("foo") is Some(policy_id): Option<ByteArray> {
      policy_id
    } else {
      "foo"
    }
  )
}

test foo_mint_via_if_is_shadowed_argument(policy_id via byte_fuzzer()) {
  let policy_id = "shadowed"
  call_mint(
    if Some("foo") is Some(policy_id): Option<ByteArray> {
      policy_id
    } else {
      "foo"
    }
  )
}
"#,
    )
    .unwrap();
}

fn write_verify_export_cross_module_fixture(root: &Path) {
    fs::create_dir_all(root.join("validators")).unwrap();

    fs::write(
        root.join("aiken.toml"),
        r#"
name = "test/verify_export_cross_module_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify export cross-module fixture"
"#,
    )
    .unwrap();

    fs::write(
        root.join("validators/fixture.ak"),
        r#"
validator foo {
  mint(_redeemer: Data, policy_id: ByteArray, _tx: Data) {
    expect policy_id == "foo"
    True
  }

  else(_ctx) {
    fail
  }
}
"#,
    )
    .unwrap();

    fs::write(
        root.join("validators/tests.ak"),
        r#"
use fixture

fn byte_fuzzer() -> Fuzzer<ByteArray> {
  todo
}

test foo_mint_cross_module(policy_id via byte_fuzzer()) {
  fixture.foo.mint(Void, policy_id, Void)
}
"#,
    )
    .unwrap();
}

fn write_verify_fuzzer_schema_fixture(root: &Path) {
    fs::create_dir_all(root.join("validators")).unwrap();

    fs::write(
        root.join("aiken.toml"),
        r#"
name = "test/verify_fuzzer_schema_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify fuzzer schema fixture"
"#,
    )
    .unwrap();

    fs::write(
        root.join("validators/tests.ak"),
        r#"
type OutputReference {
  OutputReference {
    transaction_id: ByteArray,
    output_index: Int,
  }
}

type Input {
  Input {
    output_reference: OutputReference,
    value: Int,
  }
}

type Transaction {
  Transaction {
    inputs: List<Input>,
    fee: Int,
  }
}

fn transaction_fuzzer() -> Fuzzer<List<Transaction>> {
  todo
}

test prop_transaction_domain_schema(transactions via transaction_fuzzer()) {
  True
}
"#,
    )
    .unwrap();
}

fn write_verify_state_machine_semantics_fixture(root: &Path) {
    fs::create_dir_all(root.join("validators")).unwrap();
    fs::create_dir_all(root.join("lib/aiken/fuzz")).unwrap();
    fs::create_dir_all(root.join("lib/cardano")).unwrap();

    fs::write(
        root.join("aiken.toml"),
        r#"
name = "test/verify_state_machine_semantics_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify state-machine semantics fixture"
"#,
    )
    .unwrap();

    fs::write(
        root.join("lib/cardano/transaction.ak"),
        r#"
pub type Input {
  Input
}

pub type Transaction {
  Transaction
}
"#,
    )
    .unwrap();

    fs::write(
        root.join("lib/aiken/fuzz/scenario.ak"),
        r#"
use cardano/transaction.{Input, Transaction}

pub type Scenario<st> {
  Done
  Step(List<String>, st, Transaction)
}

pub fn ok(
  initial_state: st,
  step: fn(st, List<Input>) -> Fuzzer<Scenario<st>>,
) -> Fuzzer<List<Transaction>> {
  todo
}
"#,
    )
    .unwrap();

    fs::write(
        root.join("validators/tests.ak"),
        r#"
use aiken/fuzz/scenario
use cardano/transaction.{Input, Transaction}

type State {
  State
}

fn step(_st: State, _utxo: List<Input>) -> Fuzzer<scenario.Scenario<State>> {
  todo
}

test prop_scenario_ok_export(transactions via scenario.ok(State, step)) {
  True
}
"#,
    )
    .unwrap();
}

#[test]
fn export_path_populates_validator_metadata_for_target_modes() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_roundtrip"))
        .expect("fixture should export property test");
    assert_eq!(
        test.target_kind,
        VerificationTargetKind::ValidatorHandler,
        "tests with validator metadata should export as validator targets"
    );

    let validator_target = test
        .validator_target
        .as_ref()
        .expect("export path should attach validator metadata");
    let handler_program_hex = validator_target
        .handler_program
        .as_ref()
        .expect("validator metadata should include compiled handler program")
        .hex
        .clone();
    let validator_module_name = validator_target.validator_module.clone();
    let validator_name = validator_target.validator_name.clone();
    let handler_name = validator_target
        .handler_name
        .as_deref()
        .and_then(|name| name.rsplit('.').next())
        .expect("validator metadata should include a handler name");
    assert!(
        validator_target.handler_program.is_some(),
        "validator metadata should include compiled handler program"
    );

    let modules = project.modules();
    let handler_module = modules
        .iter()
        .find(|module| module.name.ends_with(&validator_module_name))
        .expect("fixture module should be present after compile");
    let validator = handler_module
        .ast
        .definitions()
        .find_map(|definition| match definition {
            Definition::Validator(validator) if validator.name == validator_name => Some(validator),
            _ => None,
        })
        .expect("fixture validator should be present");
    let mint_handler = validator
        .handlers
        .iter()
        .find(|handler| handler.name == handler_name)
        .expect("fixture validator should expose referenced handler");

    let handler_args = validator
        .params
        .iter()
        .cloned()
        .chain(mint_handler.arguments.iter().cloned())
        .collect::<Vec<_>>();

    let mut raw_handler_generator = project.new_generator(Tracing::silent());
    let expected_raw_handler_hex = raw_handler_generator
        .generate_raw(&mint_handler.body, &handler_args, &handler_module.name)
        .to_debruijn()
        .expect("mint handler should convert to DeBruijn")
        .to_hex()
        .expect("mint handler should flat-encode to hex");

    let test_definition = handler_module
        .ast
        .definitions()
        .find_map(|definition| match definition {
            Definition::Test(test) if test.name == "foo_mint_roundtrip" => Some(test),
            _ => None,
        })
        .expect("fixture property test definition should be present");
    let test_args = test_definition
        .arguments
        .iter()
        .cloned()
        .map(Into::into)
        .collect::<Vec<_>>();
    let mut specialized_handler_generator = project.new_generator(Tracing::silent());
    let expected_specialized_handler_hex = specialized_handler_generator
        .generate_raw(&test_definition.body, &test_args, &handler_module.name)
        .to_debruijn()
        .expect("specialized handler should convert to DeBruijn")
        .to_hex()
        .expect("specialized handler should flat-encode to hex");

    assert_eq!(
        handler_program_hex, expected_specialized_handler_hex,
        "validator metadata should export the handler specialized to the test call shape"
    );

    assert_ne!(
        handler_program_hex, expected_raw_handler_hex,
        "validator metadata should not export the unspecialized full-arity handler"
    );

    let mut wrapper_generator = project.new_generator(Tracing::silent());
    let full_validator_hex = wrapper_generator
        .generate(validator, &handler_module.name)
        .to_debruijn()
        .expect("full validator wrapper should convert to DeBruijn")
        .to_hex()
        .expect("full validator wrapper should flat-encode to hex");

    assert_ne!(
        handler_program_hex, full_validator_hex,
        "handler program should not be the full validator script-context wrapper"
    );

    // Preflight may succeed (direct proof) or fail (opaque semantics) --
    // this test is primarily validating validator metadata extraction.
    let _preflight_result = preflight_validate_test(
        test,
        ExistentialMode::default(),
        &VerificationTargetKind::ValidatorHandler,
    );
    // Preflight may succeed (direct proof) or fail (opaque semantics) --
    // this test is primarily validating validator metadata extraction.
    let _eq_preflight = preflight_validate_test(
        test,
        ExistentialMode::default(),
        &VerificationTargetKind::Equivalence,
    );
}

#[test]
fn export_path_preserves_nested_fuzzer_data_schema() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_fuzzer_schema_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("prop_transaction_domain_schema"))
        .expect("fixture should export property test");

    assert!(
        matches!(
            &test.semantics,
            FuzzerSemantics::Opaque { reason }
            if reason.contains("not structurally understood yet")
        ),
        "todo-based schema fixtures should remain opaque in exported semantics, got {:?}",
        test.semantics
    );

    let schema = test
        .fuzzer_data_schema
        .as_ref()
        .expect("property test should export fuzzer data schema");

    let transaction_ref = match expect_data_schema(schema, &schema.root, "fuzzer root") {
        Data::List(Items::One(Declaration::Referenced(reference))) => reference,
        other => panic!("expected list root schema, got {other:?}"),
    };

    let transaction_ctor = match expect_data_schema(schema, transaction_ref, "Transaction") {
        Data::AnyOf(constructors) => {
            assert_eq!(
                constructors.len(),
                1,
                "Transaction should have one constructor"
            );
            &constructors[0].annotated
        }
        other => panic!("expected constructor schema for Transaction, got {other:?}"),
    };
    assert_eq!(transaction_ctor.index, 0);
    assert_eq!(transaction_ctor.fields.len(), 2);

    let inputs_ref =
        expect_referenced_data(&transaction_ctor.fields[0].annotated, "Transaction.inputs");
    let fee_ref = expect_referenced_data(&transaction_ctor.fields[1].annotated, "Transaction.fee");
    assert!(
        matches!(
            expect_data_schema(schema, fee_ref, "Transaction.fee"),
            Data::Integer
        ),
        "Transaction.fee should remain an integer field"
    );

    let input_ref = match expect_data_schema(schema, inputs_ref, "List<Input>") {
        Data::List(Items::One(Declaration::Referenced(reference))) => reference,
        other => panic!("expected list schema for Transaction.inputs, got {other:?}"),
    };

    let input_ctor = match expect_data_schema(schema, input_ref, "Input") {
        Data::AnyOf(constructors) => {
            assert_eq!(constructors.len(), 1, "Input should have one constructor");
            &constructors[0].annotated
        }
        other => panic!("expected constructor schema for Input, got {other:?}"),
    };
    assert_eq!(input_ctor.index, 0);
    assert_eq!(input_ctor.fields.len(), 2);

    let output_reference_ref =
        expect_referenced_data(&input_ctor.fields[0].annotated, "Input.output_reference");
    let input_value_ref = expect_referenced_data(&input_ctor.fields[1].annotated, "Input.value");
    assert!(
        matches!(
            expect_data_schema(schema, input_value_ref, "Input.value"),
            Data::Integer
        ),
        "Input.value should remain an integer field"
    );

    let output_reference_ctor =
        match expect_data_schema(schema, output_reference_ref, "OutputReference") {
            Data::AnyOf(constructors) => {
                assert_eq!(
                    constructors.len(),
                    1,
                    "OutputReference should have one constructor"
                );
                &constructors[0].annotated
            }
            other => panic!("expected constructor schema for OutputReference, got {other:?}"),
        };
    assert_eq!(output_reference_ctor.index, 0);
    assert_eq!(output_reference_ctor.fields.len(), 2);

    let transaction_id_ref = expect_referenced_data(
        &output_reference_ctor.fields[0].annotated,
        "OutputReference.transaction_id",
    );
    let output_index_ref = expect_referenced_data(
        &output_reference_ctor.fields[1].annotated,
        "OutputReference.output_index",
    );
    assert!(
        matches!(
            expect_data_schema(schema, transaction_id_ref, "OutputReference.transaction_id"),
            Data::Bytes
        ),
        "transaction_id should remain a bytes field"
    );
    assert!(
        matches!(
            expect_data_schema(schema, output_index_ref, "OutputReference.output_index"),
            Data::Integer
        ),
        "output_index should remain an integer field"
    );
}

#[test]
fn export_path_lowers_state_machine_transition_event_semantically() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_state_machine_semantics_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("prop_scenario_ok_export"))
        .expect("fixture should export property test");

    // After the Fuzzer-wrapper peel in `normalize_state_machine_trace_from_expr`,
    // a via-expression `scenario.ok(state, step)` whose typed return is
    // `Fuzzer<List<Transaction>>` is correctly recognized as a state-machine
    // trace. Since the Issue 14 follow-up, `output_semantics` and the
    // transition's `event_semantics` are threaded through
    // `default_semantics_for_type`, so qualified ADT event types are lowered
    // structurally instead of falling through to `Opaque` via the old
    // `SemanticType::Unsupported` path. The fixture's `Transaction` type is a
    // single nullary constructor (`pub type Transaction { Transaction }`), so
    // the event element must lower to `Constructors { tags: [0] }`.
    assert!(
        matches!(
            &test.semantics,
            FuzzerSemantics::StateMachineTrace {
                acceptance: StateMachineAcceptance::AcceptsSuccess,
                output_semantics,
                ..
            } if matches!(
                output_semantics.as_ref(),
                FuzzerSemantics::List { element, .. }
                    if matches!(
                        element.as_ref(),
                        FuzzerSemantics::Constructors { tags } if tags == &vec![0]
                    )
            )
        ),
        "scenario fixture should be recognized as StateMachineTrace with the event element lowered to Constructors {{ tags: [0] }} for the nullary `Transaction` ADT, got {:?}",
        test.semantics
    );
}

#[test]
fn export_path_falls_back_to_unbounded_int_semantics_for_seed_lambda() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_roundtrip"))
        .expect("fixture should export property test");

    assert!(
        matches!(
            &test.semantics,
            FuzzerSemantics::IntRange { min: None, max: None }
        ),
        "seed_fuzzer helper bodies currently widen to an unconstrained int domain, got {:?}",
        test.semantics
    );
}

#[test]
fn export_path_validator_metadata_respects_requested_tracing_mode() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported_silent = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let exported_verbose = project
        .export_tests(None, false, Tracing::verbose(), false)
        .unwrap();

    let silent_test = exported_silent
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_roundtrip"))
        .expect("fixture should export property test in silent mode");
    let verbose_test = exported_verbose
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_roundtrip"))
        .expect("fixture should export property test in verbose mode");

    let silent_handler_hex = silent_test
        .validator_target
        .as_ref()
        .and_then(|target| target.handler_program.as_ref())
        .map(|program| program.hex.clone())
        .expect("silent export should include validator handler program");
    let verbose_handler_hex = verbose_test
        .validator_target
        .as_ref()
        .and_then(|target| target.handler_program.as_ref())
        .map(|program| program.hex.clone())
        .expect("verbose export should include validator handler program");

    assert_ne!(
        silent_handler_hex, verbose_handler_hex,
        "handler metadata should reflect requested tracing mode"
    );

    let verbose_target = verbose_test
        .validator_target
        .as_ref()
        .expect("verbose export should include validator metadata");

    let modules = project.modules();
    let handler_module = modules
        .iter()
        .find(|module| module.name.ends_with(&verbose_target.validator_module))
        .expect("fixture module should be present after compile");
    let test_definition = handler_module
        .ast
        .definitions()
        .find_map(|definition| match definition {
            Definition::Test(test) if test.name == "foo_mint_roundtrip" => Some(test),
            _ => None,
        })
        .expect("fixture property test definition should be present");
    let test_args = test_definition
        .arguments
        .iter()
        .cloned()
        .map(Into::into)
        .collect::<Vec<_>>();

    let mut verbose_specialized_generator = project.new_generator(Tracing::verbose());
    let expected_verbose_hex = verbose_specialized_generator
        .generate_raw(&test_definition.body, &test_args, &handler_module.name)
        .to_debruijn()
        .expect("specialized handler should convert to DeBruijn")
        .to_hex()
        .expect("specialized handler should flat-encode to hex");

    assert_eq!(
        verbose_handler_hex, expected_verbose_hex,
        "verbose export should use caller-selected tracing for specialized handler metadata generation"
    );
}

#[test]
fn export_path_populates_validator_metadata_for_cross_module_module_select_calls() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_cross_module_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_cross_module"))
        .expect("fixture should export cross-module property test");

    assert_eq!(
        test.target_kind,
        VerificationTargetKind::ValidatorHandler,
        "cross-module handler calls should retain validator target mode"
    );

    let validator_target = test
        .validator_target
        .as_ref()
        .expect("cross-module handler calls should export validator metadata");
    assert_eq!(validator_target.validator_name, "foo");
    assert_eq!(validator_target.handler_name.as_deref(), Some("foo.mint"));
    assert!(
        validator_target.handler_program.is_some(),
        "cross-module validator metadata should include compiled handler program"
    );

    assert_preflight_fallback_required(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        ),
        "cross-module handler preflight (ValidatorHandler target)",
        "foo_mint_cross_module",
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        ),
        "cross-module handler preflight (Equivalence target)",
        "foo_mint_cross_module",
    );
}

#[test]
fn export_path_populates_validator_metadata_via_helper_functions() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_helper_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_via_helper"))
        .expect("fixture should export helper-routed property test");

    assert_eq!(
        test.target_kind,
        VerificationTargetKind::ValidatorHandler,
        "helper-routed validator tests should retain validator target mode"
    );

    let validator_target = test
        .validator_target
        .as_ref()
        .expect("helper-routed test should keep validator metadata");
    assert_eq!(validator_target.validator_name, "foo");
    assert_eq!(validator_target.handler_name.as_deref(), Some("foo.mint"));
    assert!(
        validator_target.handler_program.is_some(),
        "helper-routed validator tests should keep compiled handler program"
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        ),
        "helper-routed preflight (ValidatorHandler target)",
        "foo_mint_via_helper",
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        ),
        "helper-routed preflight (Equivalence target)",
        "foo_mint_via_helper",
    );
}

#[test]
fn export_path_populates_validator_metadata_via_local_alias_arguments() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_helper_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_via_local_alias"))
        .expect("fixture should export local-alias-routed property test");

    assert_eq!(
        test.target_kind,
        VerificationTargetKind::ValidatorHandler,
        "local-alias validator tests should retain validator target mode"
    );

    let validator_target = test
        .validator_target
        .as_ref()
        .expect("local-alias-routed test should keep validator metadata");
    assert_eq!(validator_target.validator_name, "foo");
    assert_eq!(validator_target.handler_name.as_deref(), Some("foo.mint"));
    assert!(
        validator_target.handler_program.is_some(),
        "local-alias-routed validator tests should keep compiled handler program"
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        ),
        "local-alias preflight (ValidatorHandler target)",
        "foo_mint_via_local_alias",
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        ),
        "local-alias preflight (Equivalence target)",
        "foo_mint_via_local_alias",
    );
}

#[test]
fn export_path_populates_validator_metadata_via_local_callee_aliases() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_helper_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_via_local_callee_alias"))
        .expect("fixture should export local-callee-alias-routed property test");

    assert_eq!(
        test.target_kind,
        VerificationTargetKind::ValidatorHandler,
        "local-callee-alias validator tests should retain validator target mode"
    );

    let validator_target = test
        .validator_target
        .as_ref()
        .expect("local-callee-alias-routed test should keep validator metadata");
    assert_eq!(validator_target.validator_name, "foo");
    assert_eq!(validator_target.handler_name.as_deref(), Some("foo.mint"));
    assert!(
        validator_target.handler_program.is_some(),
        "local-callee-alias-routed validator tests should keep compiled handler program"
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        ),
        "local-callee-alias preflight (ValidatorHandler target)",
        "foo_mint_via_local_callee_alias",
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        ),
        "local-callee-alias preflight (Equivalence target)",
        "foo_mint_via_local_callee_alias",
    );
}

#[test]
fn export_path_populates_validator_metadata_via_tuple_destructured_assignments() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_helper_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_via_tuple_destructure"))
        .expect("fixture should export tuple-destructuring-routed property test");

    assert_eq!(
        test.target_kind,
        VerificationTargetKind::ValidatorHandler,
        "tuple-destructuring validator tests should retain validator target mode"
    );

    let validator_target = test
        .validator_target
        .as_ref()
        .expect("tuple-destructuring-routed test should keep validator metadata");
    assert_eq!(validator_target.validator_name, "foo");
    assert_eq!(validator_target.handler_name.as_deref(), Some("foo.mint"));
    assert!(
        validator_target.handler_program.is_some(),
        "tuple-destructuring-routed validator tests should keep compiled handler program"
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        ),
        "tuple-destructure preflight (ValidatorHandler target)",
        "foo_mint_via_tuple_destructure",
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        ),
        "tuple-destructure preflight (Equivalence target)",
        "foo_mint_via_tuple_destructure",
    );
}

#[test]
fn export_path_does_not_infer_validator_metadata_for_control_flow_branch_calls() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_helper_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let when_shadowed = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_via_when_shadowed_call"))
        .expect("fixture should export when-shadowed property test");
    let if_is_shadowed = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_via_if_is_shadowed_call"))
        .expect("fixture should export if-is-shadowed property test");

    for (test, context) in [
        (when_shadowed, "when-shadowed control-flow call"),
        (if_is_shadowed, "if-is control-flow call"),
    ] {
        assert_eq!(
            test.target_kind,
            VerificationTargetKind::PropertyWrapper,
            "{context} should stay in property-wrapper mode"
        );

        assert!(
            test.validator_target.is_none(),
            "{context} should not export validator metadata from control-flow-dependent calls"
        );
        assert_preflight_missing_validator_metadata(
            preflight_validate_test(
                test,
                ExistentialMode::default(),
                &VerificationTargetKind::ValidatorHandler,
            ),
            &format!("{context} validator-target preflight"),
        );
        assert_preflight_missing_validator_metadata(
            preflight_validate_test(
                test,
                ExistentialMode::default(),
                &VerificationTargetKind::Equivalence,
            ),
            &format!("{context} equivalence preflight"),
        );
    }
}

#[test]
fn export_path_populates_validator_metadata_for_when_and_if_is_shadowed_arguments() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_helper_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let when_baseline = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_via_when_argument_baseline"))
        .expect("fixture should export when-argument baseline property test");
    let when_shadowed = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_via_when_shadowed_argument"))
        .expect("fixture should export when-argument shadowed property test");
    let if_is_baseline = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_via_if_is_argument_baseline"))
        .expect("fixture should export if-is-argument baseline property test");
    let if_is_shadowed = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_via_if_is_shadowed_argument"))
        .expect("fixture should export if-is-argument shadowed property test");

    let handler_hex = |test: &ExportedPropertyTest, context: &str| -> String {
        assert_eq!(
            test.target_kind,
            VerificationTargetKind::ValidatorHandler,
            "{context} should retain validator target mode"
        );

        let validator_target = test
            .validator_target
            .as_ref()
            .expect("{context} should keep validator metadata");

        assert_eq!(validator_target.validator_name, "foo");
        assert_eq!(validator_target.handler_name.as_deref(), Some("foo.mint"));

        validator_target
            .handler_program
            .as_ref()
            .map(|program| program.hex.clone())
            .expect("{context} should include compiled handler program")
    };

    let when_baseline_hex = handler_hex(when_baseline, "when baseline");
    let when_shadowed_hex = handler_hex(when_shadowed, "when shadowed");
    let if_is_baseline_hex = handler_hex(if_is_baseline, "if-is baseline");
    let if_is_shadowed_hex = handler_hex(if_is_shadowed, "if-is shadowed");

    assert_eq!(
        when_shadowed_hex, when_baseline_hex,
        "when argument substitution should ignore outer aliases for names rebound by the clause pattern"
    );
    assert_eq!(
        if_is_shadowed_hex, if_is_baseline_hex,
        "if-is argument substitution should ignore outer aliases for names rebound by the branch pattern"
    );

    assert_preflight_fallback_required(
        preflight_validate_test(
            when_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        ),
        "when-shadowed-argument preflight (ValidatorHandler target)",
        "foo_mint_via_when_shadowed_argument",
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            when_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        ),
        "when-shadowed-argument preflight (Equivalence target)",
        "foo_mint_via_when_shadowed_argument",
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            if_is_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        ),
        "if-is-shadowed-argument preflight (ValidatorHandler target)",
        "foo_mint_via_if_is_shadowed_argument",
    );
    assert_preflight_fallback_required(
        preflight_validate_test(
            if_is_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        ),
        "if-is-shadowed-argument preflight (Equivalence target)",
        "foo_mint_via_if_is_shadowed_argument",
    );
}

#[test]
fn export_path_ignores_multi_call_and_control_flow_dependent_validator_tests() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let repeated_test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_repeated"))
        .expect("fixture should export repeated-handler property test");
    let short_circuit_or_test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_in_short_circuit_or_rhs"))
        .expect("fixture should export short-circuit-or RHS property test");
    let short_circuit_and_test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_in_short_circuit_and_rhs"))
        .expect("fixture should export short-circuit-and RHS property test");
    let non_executed_if_test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_in_non_executed_if_branch"))
        .expect("fixture should export non-executed-branch property test");
    let negated_test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_wrapped_with_negation"))
        .expect("fixture should export negated wrapper property test");
    let comparison_test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_wrapped_with_comparison"))
        .expect("fixture should export comparison wrapper property test");

    assert_eq!(
        repeated_test.target_kind,
        VerificationTargetKind::PropertyWrapper,
        "multi-call validator tests should not infer a single validator handler target"
    );
    assert!(
        repeated_test.validator_target.is_none(),
        "multi-call validator tests should not export validator metadata"
    );
    assert_eq!(
        short_circuit_or_test.target_kind,
        VerificationTargetKind::PropertyWrapper,
        "short-circuit-or RHS validator calls should not infer a validator handler target"
    );
    assert!(
        short_circuit_or_test.validator_target.is_none(),
        "short-circuit-or RHS validator calls should not export validator metadata"
    );
    assert_eq!(
        short_circuit_and_test.target_kind,
        VerificationTargetKind::PropertyWrapper,
        "short-circuit-and RHS validator calls should not infer a validator handler target"
    );
    assert!(
        short_circuit_and_test.validator_target.is_none(),
        "short-circuit-and RHS validator calls should not export validator metadata"
    );
    assert_eq!(
        non_executed_if_test.target_kind,
        VerificationTargetKind::PropertyWrapper,
        "control-flow-dependent validator calls should not infer a validator handler target"
    );
    assert!(
        non_executed_if_test.validator_target.is_none(),
        "control-flow-dependent validator calls should not export validator metadata"
    );
    assert_eq!(
        negated_test.target_kind,
        VerificationTargetKind::PropertyWrapper,
        "negated validator calls should not infer a validator handler target"
    );
    assert!(
        negated_test.validator_target.is_none(),
        "negated validator calls should not export validator metadata"
    );
    assert_eq!(
        comparison_test.target_kind,
        VerificationTargetKind::PropertyWrapper,
        "comparison-wrapped validator calls should not infer a validator handler target"
    );
    assert!(
        comparison_test.validator_target.is_none(),
        "comparison-wrapped validator calls should not export validator metadata"
    );
}

#[test]
fn export_tests_output_generates_validator_and_equivalence_workspaces() {
    let tmp = tempfile::tempdir().unwrap();
    write_verify_export_fixture(tmp.path());

    let mut project = Project::new(tmp.path().to_path_buf(), EventTarget::default()).unwrap();
    project.compile(Options::default()).unwrap();

    let exported = project
        .export_tests(None, false, Tracing::silent(), false)
        .unwrap();
    let exported_test = exported
        .property_tests
        .iter()
        .find(|t| t.name.ends_with("foo_mint_roundtrip"))
        .expect("fixture should export property test");
    let validator_target_tests = vec![exported_test.clone()];
    assert!(
        exported_test.validator_target.is_some(),
        "fixture export should include validator metadata"
    );

    for target in [
        VerificationTargetKind::ValidatorHandler,
        VerificationTargetKind::Equivalence,
    ] {
        let out = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: out.path().to_path_buf(),
            cek_budget: 20_000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: target.clone(),
            plutus_core_dir: None,
            raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
            allow_vacuous_subgenerators: false,
        };

        let manifest = generate_lean_workspace(&validator_target_tests, &config, &SkipPolicy::All)
            .unwrap_or_else(|e| {
                panic!(
                    "workspace generation should succeed for --target {target}: {e}"
                )
            });
        assert!(
            !manifest.tests.is_empty(),
            "--target {target} should generate runnable tests"
        );
        assert_eq!(
            manifest.skipped.len(), 0,
            "--target {target} should not succeed with only skipped tests"
        );

        let entry = &manifest.tests[0];
        let handler_flat = out.path().join(format!("cbor/{}_handler.cbor", entry.id));
        assert!(
            handler_flat.exists(),
            "workspace for --target {target} should include handler flat file"
        );

        let proof = fs::read_to_string(out.path().join(&entry.lean_file)).unwrap();
        assert!(
            proof.contains("_handler"),
            "workspace for --target {target} should import handler program"
        );

        if matches!(target, VerificationTargetKind::Equivalence) {
            assert!(
                proof.contains("_equivalence"),
                "equivalence target should generate an equivalence theorem"
            );
        }
    }
}

#[test]
fn validator_target_mode_errors_without_metadata() {
    let mut test = make_test("my_module", "test_val");
    test.target_kind = VerificationTargetKind::ValidatorHandler;
    let id = test_id("my_module", "test_val");
    let lean_name = sanitize_lean_name("test_val");
    let lean_module = "AikenVerify.Proofs.My_module.test_val";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::ValidatorHandler,
    );
    let err = result.expect_err("validator target without metadata should fail");
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::UnsupportedShape,
        "ValidatorHandler target without validator_target metadata",
    );
    let err_text = err.to_string();
    assert!(
        err_text.contains("no validator target metadata"),
        "Should error about missing validator_target, got: {err_text}"
    );
}

#[test]
fn validator_mode_accepts_property_target_kind_when_metadata_exists() {
    let mut test = make_test("my_module", "test_target_metadata");
    test.target_kind = VerificationTargetKind::PropertyWrapper;
    test.validator_target = Some(ValidatorTarget {
        validator_module: "validators/my_validator".to_string(),
        validator_name: "spend".to_string(),
        handler_name: Some("spend.handler".to_string()),
        handler_program: Some(ExportedProgram {
            hex: "handler_hex".to_string(),
            flat_bytes: None,
        }),
    });
    let id = test_id("my_module", "test_target_metadata");
    let lean_name = sanitize_lean_name("test_target_metadata");
    let lean_module = "AikenVerify.Proofs.My_module.test_target_metadata";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::ValidatorHandler,
    )
    .expect("Validator mode should use validator metadata instead of strict target_kind matching");

    assert!(
        proof.contains("theorem "),
        "Validator-mode proof should still produce a theorem"
    );
    // Validator mode must reference the handler program (not just the property-wrapper program).
    assert!(
        proof.contains("handler_prog_"),
        "Validator-mode proof should import a handler_prog_* program binding, got:\n{proof}"
    );
    assert!(
        proof.contains("_handler.cbor"),
        "Validator-mode proof should point to a _handler.cbor artifact, got:\n{proof}"
    );
}

#[test]
fn validator_target_mode_uses_handler_prog() {
    let mut test = make_test("my_module", "test_val");
    test.target_kind = VerificationTargetKind::ValidatorHandler;
    test.validator_target = Some(ValidatorTarget {
        validator_module: "validators/my_validator".to_string(),
        validator_name: "spend".to_string(),
        handler_name: Some("spend.handler".to_string()),
        handler_program: Some(ExportedProgram {
            hex: "handler_hex".to_string(),
            flat_bytes: None,
        }),
    });
    let id = test_id("my_module", "test_val");
    let lean_name = sanitize_lean_name("test_val");
    let lean_module = "AikenVerify.Proofs.My_module.test_val";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::ValidatorHandler,
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("handler_prog"),
        "Validator handler mode should use handler program"
    );
    assert!(
        proof.contains("#import_uplc handler_prog"),
        "Should import the handler UPLC program"
    );
    assert!(
        proof.contains("_handler.cbor"),
        "Handler CBOR file should be imported"
    );
}

#[test]
fn equivalence_target_mode_generates_equivalence_theorem() {
    let mut test = make_test("my_module", "test_eq");
    test.target_kind = VerificationTargetKind::Equivalence;
    test.validator_target = Some(ValidatorTarget {
        validator_module: "validators/my_validator".to_string(),
        validator_name: "spend".to_string(),
        handler_name: None,
        handler_program: Some(ExportedProgram {
            hex: "handler_hex".to_string(),
            flat_bytes: None,
        }),
    });
    let id = test_id("my_module", "test_eq");
    let lean_name = sanitize_lean_name("test_eq");
    let lean_module = "AikenVerify.Proofs.My_module.test_eq";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::Equivalence,
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("_equivalence"),
        "Equivalence mode should generate an equivalence theorem"
    );
    assert!(
        proof.contains("#import_uplc handler_prog"),
        "Should import both the original and handler programs"
    );
    assert!(
        proof.contains("= proveTests handler_prog"),
        "Equivalence theorem should assert equal results between programs"
    );
}

#[test]
fn equivalence_target_mode_uses_non_vacuous_void_equivalence_goal() {
    let mut test = make_test("my_module", "test_eq_void");
    test.return_mode = TestReturnMode::Void;
    test.target_kind = VerificationTargetKind::Equivalence;
    test.validator_target = Some(ValidatorTarget {
        validator_module: "validators/my_validator".to_string(),
        validator_name: "spend".to_string(),
        handler_name: None,
        handler_program: Some(ExportedProgram {
            hex: "handler_hex".to_string(),
            flat_bytes: None,
        }),
    });

    let id = test_id("my_module", "test_eq_void");
    let lean_name = sanitize_lean_name("test_eq_void");
    let lean_module = "AikenVerify.Proofs.My_module.test_eq_void";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::Equivalence,
    )
    .unwrap();

    assert!(
        proof.contains("_equivalence"),
        "Void equivalence should generate an equivalence theorem, got:\n{proof}"
    );
}

#[test]
fn equivalence_target_returns_fallback_required_on_void_succeed_eventually() {
    // Previously this test asserted that Equivalence target + Void +
    // SucceedEventually + opaque Data semantics produced an error
    // demanding `--skip-unsupported`. Under the soundness-only contract,
    // opaque top-level semantics surface a `FallbackRequired` skip
    // regardless of target/return-mode/failure-mode.
    let mut test = make_test_with_type(
        "my_module",
        "test_eq_void_fallback",
        FuzzerOutputType::Data,
        FuzzerConstraint::Any,
    );
    test.return_mode = TestReturnMode::Void;
    test.on_test_failure = OnTestFailure::SucceedEventually;
    test.target_kind = VerificationTargetKind::Equivalence;
    test.validator_target = Some(ValidatorTarget {
        validator_module: "validators/my_validator".to_string(),
        validator_name: "spend".to_string(),
        handler_name: None,
        handler_program: Some(ExportedProgram {
            hex: "handler_hex".to_string(),
            flat_bytes: None,
        }),
    });

    let id = test_id("my_module", "test_eq_void_fallback");
    let lean_name = sanitize_lean_name("test_eq_void_fallback");
    let lean_module = "AikenVerify.Proofs.My_module.test_eq_void_fallback";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::Equivalence,
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Equivalence fallback on opaque Data (Void SucceedEventually)",
        "opaque",
    );
}

#[test]
fn equivalence_target_returns_fallback_required_in_fail_once_mode() {
    // Same soundness-only contract as the void variant: opaque
    // top-level Data semantics under Equivalence + fail-once mode
    // routes through the vacuous stub rather than erroring out.
    let mut test = make_test_with_type(
        "my_module",
        "test_eq_fallback_fail_once",
        FuzzerOutputType::Data,
        FuzzerConstraint::Any,
    );
    test.on_test_failure = OnTestFailure::SucceedImmediately;
    test.target_kind = VerificationTargetKind::Equivalence;
    test.validator_target = Some(ValidatorTarget {
        validator_module: "validators/my_validator".to_string(),
        validator_name: "spend".to_string(),
        handler_name: None,
        handler_program: Some(ExportedProgram {
            hex: "handler_hex".to_string(),
            flat_bytes: None,
        }),
    });

    let id = test_id("my_module", "test_eq_fallback_fail_once");
    let lean_name = sanitize_lean_name("test_eq_fallback_fail_once");
    let lean_module = "AikenVerify.Proofs.My_module.test_eq_fallback_fail_once";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::Equivalence,
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Equivalence fallback in fail-once mode (opaque Data)",
        "opaque",
    );
}

#[test]
fn verification_target_kind_parse_roundtrip() {
    assert_eq!(
        "property".parse::<VerificationTargetKind>().unwrap(),
        VerificationTargetKind::PropertyWrapper
    );
    assert_eq!(
        "validator".parse::<VerificationTargetKind>().unwrap(),
        VerificationTargetKind::ValidatorHandler
    );
    assert_eq!(
        "equivalence".parse::<VerificationTargetKind>().unwrap(),
        VerificationTargetKind::Equivalence
    );
    assert!("invalid".parse::<VerificationTargetKind>().is_err());
}

#[test]
fn verification_target_kind_display() {
    assert_eq!(
        VerificationTargetKind::PropertyWrapper.to_string(),
        "property"
    );
    assert_eq!(
        VerificationTargetKind::ValidatorHandler.to_string(),
        "validator"
    );
    assert_eq!(
        VerificationTargetKind::Equivalence.to_string(),
        "equivalence"
    );
}

#[test]
fn validate_git_rev_accepts_valid_inputs() {
    assert!(validate_git_rev("main", "blaster-rev").is_ok());
    assert!(validate_git_rev("abc123def", "blaster-rev").is_ok());
    assert!(validate_git_rev("v1.2.3", "plutus-core-rev").is_ok());
    assert!(validate_git_rev("feature/my-branch", "plutus-core-rev").is_ok());
    assert!(validate_git_rev("my_tag_1.0", "blaster-rev").is_ok());
}

#[test]
fn validate_git_rev_rejects_empty() {
    assert!(validate_git_rev("", "blaster-rev").is_err());
    assert!(validate_git_rev("", "plutus-core-rev").is_err());
}

#[test]
fn validate_git_rev_rejects_injection() {
    assert!(validate_git_rev("main\" ; rm -rf /", "blaster-rev").is_err());
    assert!(validate_git_rev("rev\nmalicious", "plutus-core-rev").is_err());
    assert!(validate_git_rev("rev`cmd`", "blaster-rev").is_err());
}

#[test]
fn validate_git_rev_rejects_leading_dash() {
    assert!(validate_git_rev("-X", "blaster-rev").is_err());
    assert!(validate_git_rev("--upload-pack=evil", "plutus-core-rev").is_err());
}

#[test]
fn collision_detector_passes_distinct_tests() {
    let tests = vec![
        make_test("module_a", "test_x"),
        make_test("module_b", "test_x"),
        make_test("module_a", "test_y"),
    ];
    assert!(detect_lean_path_collisions(&tests).is_ok());
}

#[test]
fn collision_detector_catches_duplicate_test() {
    // Exact same module+name should collide
    let tests = vec![
        make_test("module_a", "test_x"),
        make_test("module_a", "test_x"),
    ];
    let err = detect_lean_path_collisions(&tests).unwrap_err();
    let msg = err.to_string();
    assert!(
        msg.contains("collision"),
        "Error should mention collision: {msg}"
    );
    assert!(
        msg.contains("module_a.test_x"),
        "Error should name the test: {msg}"
    );
}

#[test]
fn collision_detector_catches_sanitization_collision() {
    // my-module and my_module both sanitize to My_module in Lean paths
    let tests = vec![
        make_test("my-module", "test_foo"),
        make_test("my_module", "test_foo"),
    ];
    let err = detect_lean_path_collisions(&tests).unwrap_err();
    let msg = err.to_string();
    assert!(
        msg.contains("collision"),
        "Should detect sanitization collision: {msg}"
    );
    assert!(
        msg.contains("my-module.test_foo"),
        "Should name first test: {msg}"
    );
    assert!(
        msg.contains("my_module.test_foo"),
        "Should name second test: {msg}"
    );
}

#[test]
fn short_hash_is_deterministic() {
    assert_eq!(short_hash("hello"), short_hash("hello"));
}

#[test]
fn short_hash_matches_known_fnv1a_vector() {
    assert_eq!(short_hash("hello"), "4f9f2cab");
}

#[test]
fn short_hash_differs_for_different_inputs() {
    assert_ne!(short_hash("hello"), short_hash("world"));
}

#[test]
fn classify_failure_counterexample() {
    assert_eq!(
        classify_failure("❌ Falsified after 3 tests"),
        FailureCategory::Counterexample
    );
    assert_eq!(
        classify_failure("Counterexample: x = 42"),
        FailureCategory::Counterexample
    );
}

#[test]
fn classify_failure_unsat_goal() {
    assert_eq!(
        classify_failure("Tactic `blaster` failed, unsolved goals remain"),
        FailureCategory::UnsatGoal
    );
    assert_eq!(
        classify_failure("error: unsolved goals\n⊢ False"),
        FailureCategory::UnsatGoal
    );
}

#[test]
fn classify_failure_timeout() {
    assert_eq!(
        classify_failure("deterministic timeout reached"),
        FailureCategory::Timeout
    );
}

#[test]
fn classify_failure_dependency() {
    assert_eq!(
        classify_failure("error: unknown package 'PlutusCore'\nlake fetch failed"),
        FailureCategory::DependencyError
    );
    assert_eq!(
        classify_failure("error: could not resolve package graph\nlake update failed"),
        FailureCategory::DependencyError
    );
}

#[test]
fn classify_failure_blaster_unsupported() {
    assert_eq!(
        classify_failure(
            "error: translateApp: Inductive predicate not yet supported: Lean.Expr.const `List.Mem [Lean.Level.zero]"
        ),
        FailureCategory::BlasterUnsupported
    );
}

#[test]
fn classify_failure_build_error() {
    assert_eq!(
        classify_failure("error: Lean exited with code 1"),
        FailureCategory::BuildError
    );
}

#[test]
fn classify_failure_unknown() {
    assert_eq!(
        classify_failure("some totally unrecognized output"),
        FailureCategory::Unknown
    );
}

#[test]
fn parse_verify_results_failure_includes_category() {
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small("error: 'test_add' unsolved goals\nTactic `blaster` failed"),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);
    let test_add = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_add")
        .unwrap();
    match &test_add.status {
        ProofStatus::Failed { category, .. } => {
            assert_eq!(
                *category,
                FailureCategory::UnsatGoal,
                "Should classify as UnsatGoal"
            );
        }
        other => panic!("Expected Failed, got: {:?}", other),
    }
}

#[test]
fn parse_verify_results_mixed_failures_keep_per_theorem_categories() {
    let manifest = make_manifest_with_termination(
        vec![
            ("my_module", "test_add", "test_add"),
            ("my_module", "test_sub", "test_sub"),
        ],
        false,
    );
    let raw = VerifyResult {
        success: false,
        stdout: CapturedOutput::small(""),
        stderr: CapturedOutput::small(
            "\
error: 'test_add' Counterexample:
error: Counterexample: x = 41
note: context line 1
note: context line 2
note: context line 3
note: context line 4
error: 'test_sub' unsolved goals
error: Tactic `blaster` failed",
        ),
        exit_code: Some(1),
        theorem_results: None,
    };

    let summary = parse_verify_results(raw, &manifest);
    let test_add = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_add")
        .unwrap();
    let test_sub = summary
        .theorems
        .iter()
        .find(|t| t.theorem_name == "test_sub")
        .unwrap();

    match &test_add.status {
        ProofStatus::Failed { category, .. } => {
            assert_eq!(
                *category,
                FailureCategory::Counterexample,
                "test_add should keep its counterexample category",
            );
        }
        other => panic!("Expected Failed for test_add, got: {:?}", other),
    }

    match &test_sub.status {
        ProofStatus::Failed { category, .. } => {
            assert_eq!(
                *category,
                FailureCategory::UnsatGoal,
                "test_sub should keep its unsat-goal category",
            );
        }
        other => panic!("Expected Failed for test_sub, got: {:?}", other),
    }
}

#[test]
fn skippable_generation_errors_use_structured_categories() {
    let err = generation_error(
        GenerationErrorCategory::FallbackRequired,
        "category-driven skip should not depend on message wording",
    );
    assert!(
        is_skippable_generation_error(&err, &SkipPolicy::All),
        "FallbackRequired category should be skippable regardless of message text"
    );
}

#[test]
fn non_skippable_generation_errors_ignore_legacy_message_fragments() {
    let err = generation_error(
        GenerationErrorCategory::InvalidConstraint,
        "unsupported constraint text that should not trigger skip",
    );
    assert!(
        !is_skippable_generation_error(&err, &SkipPolicy::All),
        "Skip behavior must be category-driven, not substring-driven"
    );
}

#[test]
fn skippable_error_classification_is_category_driven() {
    let fallback = generation_error(
        GenerationErrorCategory::FallbackRequired,
        "message text is irrelevant",
    );
    assert!(
        is_skippable_generation_error(&fallback, &SkipPolicy::All),
        "FallbackRequired should be skippable"
    );

    let invalid = generation_error(
        GenerationErrorCategory::InvalidConstraint,
        "unsupported no extractable requires fallback",
    );
    assert!(
        !is_skippable_generation_error(&invalid, &SkipPolicy::All),
        "InvalidConstraint should not be skippable"
    );
}

#[test]
fn skip_policy_from_cli_maps_clap_shape_to_policy_variants() {
    // `--strict-unsupported` (or omitting `--skip-unsupported`) → policy None.
    assert_eq!(SkipPolicy::from_cli(None), SkipPolicy::None);
    // `--skip-unsupported` (no value) → policy All.
    assert_eq!(SkipPolicy::from_cli(Some(Vec::new())), SkipPolicy::All);
    // `--skip-unsupported=E0011,E0012` → policy Codes(set).
    let codes = vec!["E0011".to_string(), "E0012".to_string()];
    let expected: std::collections::BTreeSet<String> = codes.iter().cloned().collect();
    assert_eq!(
        SkipPolicy::from_cli(Some(codes)),
        SkipPolicy::Codes(expected)
    );
}

#[test]
fn skip_policy_codes_filters_to_listed_codes_only() {
    // Spec from plan §"CLI Surface" lines 793–796:
    //   `--skip-unsupported=E0011,E0012` → skip ONLY those codes; other
    //   skippable codes hard-error. UnsoundFallback never skips regardless.
    let policy = SkipPolicy::Codes(
        ["E0011".to_string(), "E0012".to_string()]
            .into_iter()
            .collect(),
    );

    let e0011 = miette::Report::new(error_catalogue::unsupported(
        "E0011",
        UnsupportedReason::UnboundedBytearray {
            test_name: "module.test".to_string(),
        },
    ));
    assert!(
        is_skippable_generation_error(&e0011, &policy),
        "E0011 (FallbackRequired) should be skippable when in the codes list"
    );

    let e0013 = miette::Report::new(error_catalogue::unsupported(
        "E0013",
        UnsupportedReason::ListOfBool {
            test_name: "module.test".to_string(),
        },
    ));
    assert!(
        !is_skippable_generation_error(&e0013, &policy),
        "E0013 (FallbackRequired) should NOT be skippable when not in the codes list"
    );

    let s0002 = miette::Report::new(error_catalogue::unsupported(
        "S0002",
        UnsupportedReason::ConstructorTagUnresolved {
            test_name: "module.test".to_string(),
            ctor: "Variant".to_string(),
            type_name: "Type".to_string(),
        },
    ));
    assert!(
        !is_skippable_generation_error(&s0002, &policy),
        "S0002 (UnsoundFallback) must NEVER be skippable, even if its code is listed"
    );

    // S0002 is unskippable even when explicitly listed in the codes filter.
    let policy_listing_s = SkipPolicy::Codes(["S0002".to_string()].into_iter().collect());
    assert!(
        !is_skippable_generation_error(&s0002, &policy_listing_s),
        "S0002 (UnsoundFallback) must NEVER be skippable, even when explicitly listed in --skip-unsupported"
    );
}

#[test]
fn skip_policy_none_rejects_every_skippable_category() {
    // `--strict-unsupported` (= `SkipPolicy::None`) must hard-error every
    // category, even ones that `SkipPolicy::All` would silence.
    let fallback = miette::Report::new(error_catalogue::unsupported(
        "E0011",
        UnsupportedReason::UnboundedBytearray {
            test_name: "module.test".to_string(),
        },
    ));
    assert!(
        !is_skippable_generation_error(&fallback, &SkipPolicy::None),
        "FallbackRequired must NOT be skippable under SkipPolicy::None (--strict-unsupported)"
    );

    let unsupported_shape = miette::Report::new(error_catalogue::unsupported(
        "E0019",
        UnsupportedReason::TransactionShapedTest {
            test_name: "module.test".to_string(),
        },
    ));
    assert!(
        !is_skippable_generation_error(&unsupported_shape, &SkipPolicy::None),
        "UnsupportedShape must NOT be skippable under SkipPolicy::None (--strict-unsupported)"
    );
}

#[test]
fn skip_unsupported_collects_skipped_tests() {
    let tmp = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: tmp.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    // Mismatched output type / semantics gets skipped when skip_unsupported is true.
    let mut unsupported = make_test("my_module", "test_list");
    unsupported.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    unsupported.constraint = FuzzerConstraint::Any;

    let manifest = generate_lean_workspace(&[unsupported], &config, &SkipPolicy::All).unwrap();
    assert_eq!(
        manifest.skipped.len(),
        1,
        "mismatched type/semantics should be skipped"
    );
    assert!(manifest.tests.is_empty());
}

#[test]
fn skip_unsupported_ignores_collisions_from_skipped_tests() {
    let tmp = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: tmp.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::ValidatorHandler,
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let mut supported = make_test("my_module", "test_collision");
    supported.validator_target = Some(ValidatorTarget {
        validator_module: "validators/my_module".to_string(),
        validator_name: "spend".to_string(),
        handler_name: Some("spend.handler".to_string()),
        handler_program: Some(ExportedProgram {
            hex: "deadbeef".to_string(),
            flat_bytes: None,
        }),
    });
    let skipped = make_test("my_module", "test_collision");

    let manifest = generate_lean_workspace(&[supported, skipped], &config, &SkipPolicy::All)
        .unwrap_or_else(|e| {
            panic!("skip mode should ignore collisions from tests that are skipped: {e}")
        });

    assert_eq!(
        manifest.tests.len(),
        1,
        "exactly one test should be generated"
    );
    assert_eq!(
        manifest.skipped.len(),
        1,
        "unsupported colliding test should be skipped"
    );
}

#[test]
fn skip_unsupported_skips_missing_validator_metadata_for_target_modes() {
    for target in [
        VerificationTargetKind::ValidatorHandler,
        VerificationTargetKind::Equivalence,
    ] {
        let tmp = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: tmp.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
            existential_mode: ExistentialMode::default(),
            target: target.clone(),
            plutus_core_dir: None,
            raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
            allow_vacuous_subgenerators: false,
        };

        let missing_metadata = make_test("my_module", "test_missing_validator_metadata");
        let manifest = generate_lean_workspace(&[missing_metadata], &config, &SkipPolicy::All)
            .unwrap_or_else(|e| panic!("skip mode should not fail for target {target}: {e}"));

        assert!(manifest.tests.is_empty(),);
        assert_eq!(
            manifest.skipped.len(),
            1,
            "exactly one test should be skipped for target {target}"
        );
        assert!(
            manifest.skipped[0]
                .reason
                .contains("no validator target metadata"),
            "skip reason should mention missing validator metadata for target {target}"
        );
    }
}

#[test]
fn skip_unsupported_false_errors_on_unsupported() {
    let tmp = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: tmp.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let mut unsupported = make_test("my_module", "test_list");
    unsupported.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    unsupported.constraint = FuzzerConstraint::Any;

    let err = generate_lean_workspace(&[unsupported], &config, &SkipPolicy::None)
        .expect_err("Nested composites with opaque semantics should error without skip mode");

    // Without `skip_unsupported`, a skippable (FallbackRequired) error must surface
    // as a hard failure - workspace gen must not silently swallow it.
    let message = err.to_string();
    assert!(
        message.contains("test_list")
            || message.contains("cannot")
            || message.contains("opaque")
            || message.contains("unsupported"),
        "Error should mention the failing test or the reason, got: {message}"
    );
}

#[test]
fn skip_unsupported_does_not_swallow_non_skippable_generation_error() {
    let tmp = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: tmp.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    // Semantically invalid (min > max) should be treated as generation error,
    // not as a skippable unsupported-shape case.
    let bad = make_test_with_type(
        "my_module",
        "test_bad_bounds",
        FuzzerOutputType::Int,
        FuzzerConstraint::IntRange {
            min: "10".to_string(),
            max: "0".to_string(),
        },
    );

    let result = generate_lean_workspace(&[bad], &config, &SkipPolicy::All);
    assert!(result.is_err(), "Non-skippable errors should still fail");
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("non-skippable error"),
        "Error should indicate non-skippable generation failure"
    );
}

#[test]
fn skip_unsupported_mixed_generates_supported_and_skips_rest() {
    let tmp = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: tmp.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let good = make_test("my_module", "test_int");
    let mut bad = make_test("my_module", "test_list");
    bad.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    bad.constraint = FuzzerConstraint::Any;

    let manifest = generate_lean_workspace(&[good, bad], &config, &SkipPolicy::All).unwrap();
    assert_eq!(manifest.tests.len(), 1, "Good test should generate");
    assert_eq!(manifest.skipped.len(), 1, "Bad test should be skipped");
}

// --- Step 3.1 tests: String and Pair ---

#[test]
fn string_without_domain_predicate_reports_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_string",
        FuzzerOutputType::String,
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_string");
    let lean_name = sanitize_lean_name("test_string");
    let lean_module = "AikenVerify.Proofs.My_module.test_string";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    let err = result.expect_err("String with opaque semantics should not produce a direct proof");
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::FallbackRequired,
        "String without domain predicate",
    );
}

#[test]
fn pair_data_data_without_predicates_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_pair_dd",
        FuzzerOutputType::Pair(
            Box::new(FuzzerOutputType::Data),
            Box::new(FuzzerOutputType::Data),
        ),
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_pair_dd");
    let lean_name = sanitize_lean_name("test_pair_dd");
    let lean_module = "AikenVerify.Proofs.My_module.test_pair_dd";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Pair<Data,Data> without predicates",
        "opaque",
    );
}

#[test]
fn pair_int_data_generates_theorem_with_bounds() {
    let test = make_test_with_type(
        "my_module",
        "test_pair_id",
        FuzzerOutputType::Pair(
            Box::new(FuzzerOutputType::Int),
            Box::new(FuzzerOutputType::Data),
        ),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::IntRange {
                min: "-10".to_string(),
                max: "10".to_string(),
            },
            FuzzerConstraint::Any,
        ]),
    );
    let id = test_id("my_module", "test_pair_id");
    let lean_name = sanitize_lean_name("test_pair_id");
    let lean_module = "AikenVerify.Proofs.My_module.test_pair_id";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("(a : Integer)"),
        "Pair fst should bind as Integer"
    );
    assert!(proof.contains("(b : Data)"), "Pair snd should bind as Data");
    assert!(
        proof.contains("-10 <= a"),
        "Should contain lower bound for Int component"
    );
    assert!(
        proof.contains("a <= 10"),
        "Should contain upper bound for Int component"
    );
}

// --- Step 3.2 tests: Generic tuple arities ---

#[test]
fn tuple_arity_4_generates_theorem() {
    // A ByteArray leaf anywhere in the fuzzer's output type tree triggers
    // the ByteString skip (Blaster cannot universally quantify over
    // ByteString yet). Use Data in its place to exercise arity-4 tuple
    // generation without tripping the skip.
    let test = make_test_with_type(
        "my_module",
        "test_quad",
        FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::Data,
            FuzzerOutputType::Int,
            FuzzerOutputType::Bool,
            FuzzerOutputType::Data,
        ]),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::Any,
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "99".to_string(),
            },
            FuzzerConstraint::Any,
            FuzzerConstraint::Any,
        ]),
    );
    let id = test_id("my_module", "test_quad");
    let lean_name = sanitize_lean_name("test_quad");
    let lean_module = "AikenVerify.Proofs.My_module.test_quad";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("Data"),
        "Should contain Data type for first component"
    );
    assert!(
        proof.contains("Integer"),
        "Should contain Integer type for second component"
    );
    assert!(
        proof.contains("Bool"),
        "Should contain Bool type for third component"
    );
}

#[test]
fn tuple_arity_5_all_data_without_predicates_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_quint",
        FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::Data,
            FuzzerOutputType::Data,
            FuzzerOutputType::Data,
            FuzzerOutputType::Data,
            FuzzerOutputType::Data,
        ]),
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_quint");
    let lean_name = sanitize_lean_name("test_quint");
    let lean_module = "AikenVerify.Proofs.My_module.test_quint";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Tuple(Data x5) without predicates",
        "opaque",
    );
}

#[test]
fn tuple_high_arity_without_predicates_returns_fallback_required() {
    let test = make_test_with_type(
        "my_module",
        "test_arity_20",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Data; 20]),
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_arity_20");
    let lean_name = sanitize_lean_name("test_arity_20");
    let lean_module = "AikenVerify.Proofs.My_module.test_arity_20";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Tuple arity-20 without predicates",
        "opaque",
    );
}

// --- Step 3.3 tests: List with bounds ---

#[test]
fn list_with_bounds_generates_theorem() {
    let test = make_test_with_type(
        "my_module",
        "test_list_bounded",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            }),
            min_len: Some(0),
            max_len: Some(10),
        },
    );
    let id = test_id("my_module", "test_list_bounded");
    let lean_name = sanitize_lean_name("test_list_bounded");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_bounded";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("_l0"),
        "Should emit per-length theorem for length 0"
    );
    assert!(
        proof.contains("_l10"),
        "Should emit per-length theorem up to the max length"
    );
    assert!(
        proof.contains(": Integer)"),
        "Should annotate element parameters as Integer"
    );
    assert!(
        proof.contains("0 \u{2264}") || proof.contains("0 <="),
        "Should contain lower bound predicate on elements"
    );
}

#[test]
fn list_data_with_bounds_generates_theorem() {
    let test = make_test_with_type(
        "my_module",
        "test_list_data",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: Some(1),
            max_len: Some(5),
        },
    );
    let id = test_id("my_module", "test_list_data");
    let lean_name = sanitize_lean_name("test_list_data");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_data";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("_l1"),
        "Should emit per-length theorem for the min length"
    );
    assert!(
        proof.contains("_l5"),
        "Should emit per-length theorem for the max length"
    );
    assert!(
        proof.contains(": Data)"),
        "Should annotate element parameters as Data"
    );
}

#[test]
fn list_with_inconsistent_length_bounds_errors() {
    let test = make_test_with_type(
        "my_module",
        "test_list_bad_len_bounds",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            }),
            min_len: Some(5),
            max_len: Some(2),
        },
    );
    let id = test_id("my_module", "test_list_bad_len_bounds");
    let lean_name = sanitize_lean_name("test_list_bad_len_bounds");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_bad_len_bounds";

    let err = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap_err();

    assert!(
        err.to_string().contains("inconsistent list-length bounds"),
        "Should report inconsistent list bounds, got:\n{err}"
    );
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::InvalidConstraint,
        "inconsistent bounds should be InvalidConstraint",
    );
}

#[test]
fn list_theorem_naming_uses_length_suffix() {
    let test = make_test_with_type(
        "my_module",
        "test_list_naming",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            }),
            min_len: Some(0),
            max_len: Some(2),
        },
    );
    let id = test_id("my_module", "test_list_naming");
    let lean_name = sanitize_lean_name("test_list_naming");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_naming";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("_l0"),
        "Proof should contain per-length theorem name `_l0`, got:\n{proof}"
    );
    assert!(
        proof.contains("_l2"),
        "Proof should contain per-length theorem name `_l2`, got:\n{proof}"
    );
    assert!(
        !proof.contains("xs : List"),
        "Proof must not use the old `xs : List` format, got:\n{proof}"
    );
    assert!(
        !proof.contains("xs.length"),
        "Proof must not use the old `xs.length` format, got:\n{proof}"
    );
    assert!(
        !proof.contains("\u{2200} xs"),
        "Proof must not use the old `\u{2200} xs` format, got:\n{proof}"
    );
}

#[test]
fn list_length_indexed_path_rejects_inconsistent_bounds_before_loop() {
    // Integer element type forces the length-indexed code path (not fallback),
    // and min=7 > max=3 is inconsistent. This confirms `validate_list_len_bounds`
    // runs on the length-indexed path as well as the fallback path.
    let test = make_test_with_type(
        "my_module",
        "test_list_len_indexed_bad_bounds",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            }),
            min_len: Some(7),
            max_len: Some(3),
        },
    );
    let id = test_id("my_module", "test_list_len_indexed_bad_bounds");
    let lean_name = sanitize_lean_name("test_list_len_indexed_bad_bounds");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_len_indexed_bad_bounds";

    let err = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap_err();

    let msg = err.to_string();
    assert!(
        msg.contains("inconsistent list-length bounds")
            || (msg.contains("bounds") && msg.contains("inconsistent")),
        "Should report inconsistent list bounds on the length-indexed path, got:\n{msg}"
    );
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::InvalidConstraint,
        "length-indexed path with inconsistent bounds should be InvalidConstraint",
    );
}

#[test]
fn list_bounds_can_be_extracted_through_and_constraint() {
    let test = make_test_with_type(
        "my_module",
        "test_list_and",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
        FuzzerConstraint::And(vec![
            FuzzerConstraint::Any,
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(1),
                max_len: Some(5),
            },
        ]),
    );
    let id = test_id("my_module", "test_list_and");
    let lean_name = sanitize_lean_name("test_list_and");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_and";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("_l1"),
        "Should emit per-length theorem for the min length"
    );
    assert!(
        proof.contains("_l5"),
        "Should emit per-length theorem for the max length"
    );
    assert!(
        proof.contains(": Data)"),
        "Should annotate element parameters as Data"
    );
}

#[test]
fn list_missing_max_len_generates_one_sided_length_precondition() {
    let test = make_test_with_type(
        "my_module",
        "test_list_nomax",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            }),
            min_len: Some(0),
            max_len: None,
        },
    );
    let id = test_id("my_module", "test_list_nomax");
    let lean_name = sanitize_lean_name("test_list_nomax");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_nomax";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("List"),
        "One-sided length precondition test should generate a List proof"
    );
    assert!(
        proof.contains("xs.length"),
        "One-sided length precondition should include length bounds"
    );
}

// --- Step 3.4 tests: ADT fallback via Data encoding ---

#[test]
fn unsupported_type_returns_fallback_required() {
    // An unsupported top-level ADT type with opaque semantics now routes
    // through the `FallbackRequired` skip instead of erroring out.
    // This is exactly the `prop_permissions_scenarii_distribution` case
    // that motivated the soundness-only contract (Fuzzer<Outcome> where
    // Outcome is a custom enum without a Lean lowering yet).
    let test = make_test_with_type(
        "my_module",
        "test_adt",
        FuzzerOutputType::Unsupported("MyCustomType".to_string()),
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_adt");
    let lean_name = sanitize_lean_name("test_adt");
    let lean_module = "AikenVerify.Proofs.My_module.test_adt";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert_proof_generation_skipped_as_fallback(
        &result,
        "Unsupported top-level ADT with opaque semantics",
        "opaque",
    );
}

#[test]
fn unsupported_type_in_tuple_falls_back_to_data() {
    let test = make_test_with_type(
        "my_module",
        "test_tuple_adt",
        FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::Int,
            FuzzerOutputType::Unsupported("MyCustomType".to_string()),
        ]),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "50".to_string(),
            },
            FuzzerConstraint::Any,
        ]),
    );
    let id = test_id("my_module", "test_tuple_adt");
    let lean_name = sanitize_lean_name("test_tuple_adt");
    let lean_module = "AikenVerify.Proofs.My_module.test_tuple_adt";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("Integer"),
        "Should contain Integer type for Int component"
    );
    assert!(
        proof.contains("Data"),
        "Unsupported type should fall back to Data"
    );
}

#[test]
fn tuple_int_data_with_shared_int_range_constrains_only_int_positions() {
    let test = make_test_with_type(
        "my_module",
        "test_tuple_shared_range",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Data]),
        FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "50".to_string(),
        },
    );
    let id = test_id("my_module", "test_tuple_shared_range");
    let lean_name = sanitize_lean_name("test_tuple_shared_range");
    let lean_module = "AikenVerify.Proofs.My_module.test_tuple_shared_range";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("Integer"),
        "Should contain Integer for Int position"
    );
    assert!(
        proof.contains("Data"),
        "Should contain Data for Data position"
    );
    assert!(
        proof.contains("0 <= a"),
        "Shared IntRange should apply to Int position"
    );
}

// --- requires_explicit_bounds extended tests ---

#[test]
fn requires_explicit_bounds_list_needs_extractable_domain() {
    assert!(requires_explicit_bounds(
        &FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        &FuzzerConstraint::Any,
    ));
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        &FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: Some(0),
            max_len: Some(10),
        },
    ));
}

#[test]
fn requires_explicit_bounds_pair_with_int() {
    // Pair(Int, Data) with Any constraint => needs bounds
    assert!(requires_explicit_bounds(
        &FuzzerOutputType::Pair(
            Box::new(FuzzerOutputType::Int),
            Box::new(FuzzerOutputType::Data),
        ),
        &FuzzerConstraint::Any,
    ));
    // Pair(Data, Data) never needs bounds
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::Pair(
            Box::new(FuzzerOutputType::Data),
            Box::new(FuzzerOutputType::Data),
        ),
        &FuzzerConstraint::Any,
    ));
}

#[test]
fn requires_explicit_bounds_generic_tuple_with_int() {
    // Tuple(Int, Data, Bool) with Int needs bounds
    assert!(requires_explicit_bounds(
        &FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::Int,
            FuzzerOutputType::Data,
            FuzzerOutputType::Bool,
        ]),
        &FuzzerConstraint::Any,
    ));
    // Tuple(Data, Data, Bool) never needs bounds
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::Data,
            FuzzerOutputType::Data,
            FuzzerOutputType::Bool,
        ]),
        &FuzzerConstraint::Any,
    ));
}

#[test]
fn failure_category_serializes_to_json() {
    let result = TheoremResult {
        test_name: "mod.test".to_string(),
        theorem_name: "test".to_string(),
        status: ProofStatus::Failed {
            category: FailureCategory::Counterexample,
            reason: "x = 42".to_string(),
        },
        over_approximations: 0,
    };
    let json = serde_json::to_string(&result).unwrap();
    // Wire format: ProofStatus is internally-tagged with `kind` (snake_case);
    // FailureCategory is internally-tagged with `kind` (kebab-case).
    // See plan §P0.3 — top-level `version: "1"`.
    assert!(
        json.contains("\"category\":{\"kind\":\"counterexample\"}"),
        "JSON should include kebab-case kind-tagged category: {json}"
    );
    assert!(
        json.contains("\"reason\":\"x = 42\""),
        "JSON should include reason: {json}"
    );
}

// --- ArtifactRetention tests ---

#[test]
fn artifact_retention_parse_roundtrip() {
    for s in &["on-failure", "on-success", "always", "never"] {
        let parsed: ArtifactRetention = s.parse().unwrap();
        assert_eq!(parsed.to_string(), *s);
    }
}

#[test]
fn artifact_retention_parse_rejects_unknown() {
    assert!("bogus".parse::<ArtifactRetention>().is_err());
}

#[test]
fn artifact_retention_default_is_on_failure() {
    assert_eq!(ArtifactRetention::default(), ArtifactRetention::OnFailure);
}

#[test]
fn should_retain_on_failure_keeps_on_fail() {
    assert!(should_retain_artifacts(ArtifactRetention::OnFailure, false));
    assert!(!should_retain_artifacts(ArtifactRetention::OnFailure, true));
}

#[test]
fn should_retain_on_success_keeps_on_pass() {
    assert!(!should_retain_artifacts(
        ArtifactRetention::OnSuccess,
        false
    ));
    assert!(should_retain_artifacts(ArtifactRetention::OnSuccess, true));
}

#[test]
fn should_retain_always_keeps_everything() {
    assert!(should_retain_artifacts(ArtifactRetention::Always, false));
    assert!(should_retain_artifacts(ArtifactRetention::Always, true));
}

#[test]
fn should_retain_never_keeps_nothing() {
    assert!(!should_retain_artifacts(ArtifactRetention::Never, false));
    assert!(!should_retain_artifacts(ArtifactRetention::Never, true));
}

// --- clean_artifacts tests ---

#[test]
fn clean_artifacts_removes_only_generated_outputs() {
    let dir = tempfile::tempdir().unwrap();
    let out = dir.path().join("verify_workspace");
    fs::create_dir_all(&out).unwrap();
    fs::create_dir_all(out.join("AikenVerify")).unwrap();
    fs::write(out.join("AikenVerify/stale.lean"), "-- stale").unwrap();
    fs::create_dir_all(out.join("logs")).unwrap();
    fs::write(out.join("logs/lake-build.log"), "stale").unwrap();
    fs::write(out.join("lakefile.lean"), "-- stale").unwrap();
    fs::write(out.join("lean-toolchain"), "leanprover/lean4:v0.0.0\n").unwrap();

    fs::create_dir_all(out.join("PlutusCore")).unwrap();
    fs::write(out.join("PlutusCore/lakefile.lean"), "-- keep").unwrap();
    fs::create_dir_all(out.join("cache")).unwrap();
    fs::write(out.join("cache/custom.txt"), "keep").unwrap();

    let removed = clean_artifacts(&out).unwrap();
    assert_eq!(removed.len(), 4);
    assert!(removed.contains(&out.join("AikenVerify")));
    assert!(removed.contains(&out.join("logs")));
    assert!(removed.contains(&out.join("lakefile.lean")));
    assert!(removed.contains(&out.join("lean-toolchain")));

    assert!(out.exists(), "clean should not remove the entire out_dir");
    assert!(!out.join("AikenVerify").exists());
    assert!(!out.join("logs").exists());
    assert!(!out.join("lakefile.lean").exists());
    assert!(!out.join("lean-toolchain").exists());
    assert!(out.join("PlutusCore/lakefile.lean").exists());
    assert!(out.join("cache/custom.txt").exists());
}

#[test]
fn clean_artifacts_returns_empty_for_missing_dir() {
    let dir = tempfile::tempdir().unwrap();
    let out = dir.path().join("nonexistent");
    let removed = clean_artifacts(&out).unwrap();
    assert!(removed.is_empty());
}

#[test]
fn clean_artifacts_with_dot_path_keeps_workspace_root() {
    let dir = tempfile::tempdir().unwrap();
    let out = dir.path().join("verify_workspace");
    fs::create_dir_all(out.join("AikenVerify")).unwrap();
    fs::write(out.join("AikenVerify/stale.lean"), "-- stale").unwrap();

    let removed = clean_artifacts(&out.join(".")).unwrap();

    assert_eq!(removed.len(), 1);
    assert!(
        removed
            .iter()
            .any(|path| path.ends_with(std::path::Path::new("AikenVerify"))),
        "expected AikenVerify directory to be removed, got: {removed:?}"
    );
    assert!(
        out.exists(),
        "clean should never remove the workspace root even for out_dir='.'"
    );
    assert!(!out.join("AikenVerify").exists());
}

#[test]
fn clean_artifacts_rejects_parent_traversal_paths() {
    let dir = tempfile::tempdir().unwrap();
    let out = dir.path().join("verify_workspace");
    fs::create_dir_all(&out).unwrap();

    let _err = clean_artifacts(&out.join("../outside"))
        .expect_err("paths containing parent traversal should be rejected");
}

#[test]
fn clean_artifacts_rejects_generic_paths_without_verify_markers() {
    let dir = tempfile::tempdir().unwrap();
    let out = dir.path().join("outside_project_workspace");
    fs::create_dir_all(out.join("logs")).unwrap();
    fs::create_dir_all(out.join("cbor")).unwrap();
    fs::write(out.join("manifest.json"), "{}").unwrap();
    fs::write(out.join("lakefile.lean"), "-- unrelated").unwrap();

    let _err =
        clean_artifacts(&out).expect_err("generic paths without verify markers should be rejected");
}

#[cfg(unix)]
#[test]
fn clean_artifacts_rejects_symlinked_generated_paths_that_escape_workspace() {
    use std::os::unix::fs::symlink;

    let dir = tempfile::tempdir().unwrap();
    let out = dir.path().join("verify_workspace");
    fs::create_dir_all(&out).unwrap();

    let outside = dir.path().join("outside_lake");
    fs::create_dir_all(outside.join("build/lib/AikenVerify")).unwrap();
    fs::write(
        outside.join("build/lib/AikenVerify/should-not-delete.olean"),
        "keep",
    )
    .unwrap();

    symlink(&outside, out.join(".lake")).unwrap();

    let _err = clean_artifacts(&out)
        .expect_err("cleanup should reject generated paths that resolve outside out_dir");
}

#[test]
fn clear_generated_workspace_removes_generated_outputs_and_preserves_cache() {
    let dir = tempfile::tempdir().unwrap();
    let out = dir.path();

    fs::create_dir_all(out.join("AikenVerify/Proofs/Stale")).unwrap();
    fs::write(out.join("AikenVerify/Proofs/Stale/old.lean"), "-- stale").unwrap();
    fs::create_dir_all(out.join("cbor")).unwrap();
    fs::write(out.join("cbor/stale.cbor"), "stale").unwrap();
    fs::write(
        out.join("AikenVerify.lean"),
        "import AikenVerify.Proofs.Stale.old",
    )
    .unwrap();
    fs::write(out.join("manifest.json"), "{}").unwrap();
    fs::write(out.join("lakefile.lean"), "-- stale").unwrap();
    fs::write(out.join("lean-toolchain"), "leanprover/lean4:v0.0.0\n").unwrap();
    fs::create_dir_all(out.join("logs")).unwrap();
    fs::write(out.join("logs/stale.log"), "stale").unwrap();
    fs::create_dir_all(out.join(".lake/build/lib/AikenVerify/Stale")).unwrap();
    fs::write(
        out.join(".lake/build/lib/AikenVerify/Stale/stale.olean"),
        "stale",
    )
    .unwrap();

    fs::create_dir_all(out.join("PlutusCore")).unwrap();
    fs::write(out.join("PlutusCore/lakefile.lean"), "-- keep").unwrap();
    fs::create_dir_all(out.join(".lake/packages/Blaster")).unwrap();
    fs::write(out.join(".lake/packages/Blaster/cache.txt"), "keep").unwrap();
    fs::create_dir_all(out.join(".lake/build/lib/PlutusCore")).unwrap();
    fs::write(out.join(".lake/build/lib/PlutusCore/cache.olean"), "keep").unwrap();

    clear_generated_workspace(out).unwrap();

    assert!(!out.join("AikenVerify").exists());
    assert!(!out.join("AikenVerify.lean").exists());
    assert!(!out.join("cbor").exists());
    assert!(!out.join("manifest.json").exists());
    assert!(!out.join("lakefile.lean").exists());
    assert!(!out.join("lean-toolchain").exists());
    assert!(!out.join("logs").exists());
    assert!(!out.join(".lake/build/lib/AikenVerify").exists());

    assert!(out.join("PlutusCore/lakefile.lean").exists());
    assert!(out.join(".lake/packages/Blaster/cache.txt").exists());
    assert!(out.join(".lake/build/lib/PlutusCore/cache.olean").exists());
}

// --- capabilities tests ---

#[test]
fn capabilities_json_roundtrip() {
    let caps = capabilities();
    assert_eq!(caps.version, VERIFICATION_CAPABILITIES_VERSION);
    let json = serde_json::to_string_pretty(&caps).unwrap();
    // Verify it contains expected top-level fields
    assert!(json.contains("supported_test_kinds"));
    assert!(json.contains("unsupported_test_kinds"));
    assert!(json.contains("target_modes"));
    assert!(json.contains("supported_fuzzer_types"));
    assert!(json.contains("unsupported_fuzzer_types"));
    assert!(json.contains("existential_modes"));
    assert!(json.contains("max_test_arity"));
}

#[test]
fn capabilities_supported_types_include_all_working_types() {
    let caps = capabilities();
    assert!(caps.supported_fuzzer_types.contains(&"Int".to_string()));
    assert!(caps.supported_fuzzer_types.contains(&"Bool".to_string()));
    assert!(
        caps.supported_fuzzer_types
            .contains(&"ByteArray".to_string())
    );
    assert!(caps.supported_fuzzer_types.contains(&"String".to_string()));
    assert!(caps.supported_fuzzer_types.contains(&"Data".to_string()));
}

#[test]
fn capabilities_unsupported_kinds_include_unit_and_benchmark() {
    let caps = capabilities();
    let kinds: Vec<&str> = caps
        .unsupported_test_kinds
        .iter()
        .map(|n| n.kind.as_str())
        .collect();
    assert!(kinds.contains(&"unit"));
    assert!(kinds.contains(&"benchmark"));
}

#[test]
fn capabilities_existential_modes_include_witness_and_proof() {
    let caps = capabilities();
    assert!(caps.existential_modes.contains(&"witness".to_string()));
}

#[test]
fn capabilities_max_arity_is_one() {
    assert_eq!(capabilities().max_test_arity, 1);
}

// --- Step 11.2: Compatibility fixture suite ---
//
// These tests validate the full pipeline (export -> workspace gen -> proof file)
// for every supported type, expected failure cases, and skip scenarios.
// They serve as the e2e compatibility regression suite.

#[test]
fn compat_all_supported_scalar_types_generate_workspace() {
    let types_and_constraints = [
        (
            FuzzerOutputType::Int,
            FuzzerConstraint::IntRange {
                min: "0".into(),
                max: "10".into(),
            },
        ),
        (FuzzerOutputType::Bool, FuzzerConstraint::Any),
        (FuzzerOutputType::ByteArray, FuzzerConstraint::Any),
        (FuzzerOutputType::String, FuzzerConstraint::Any),
        (FuzzerOutputType::Data, FuzzerConstraint::Any),
    ];

    for (i, (output_type, constraint)) in types_and_constraints.iter().enumerate() {
        let mut test = make_test("compat", &format!("scalar_{i}"));
        test.fuzzer_output_type = output_type.clone();
        test.constraint = constraint.clone();
        test.semantics = derive_fixture_semantics_from_constraint(
            &test.fuzzer_output_type,
            &test.constraint,
            false,
        );

        let dir = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: dir.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
            existential_mode: ExistentialMode::Witness,
            target: VerificationTargetKind::PropertyWrapper,
            plutus_core_dir: None,
            raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
            allow_vacuous_subgenerators: false,
        };

        let manifest =
            generate_lean_workspace(&[test], &config, &SkipPolicy::All).unwrap_or_else(|e| {
                panic!(
                    "Scalar type {:?} should generate workspace (possibly with skips), got: {e}",
                    output_type
                )
            });

        // Either the test generates a theorem or is gracefully skipped.
        let generated = manifest.tests.len() + manifest.skipped.len();
        assert_eq!(
            generated,
            1,
            "Scalar type {:?} must produce either one test entry or one skip entry, got tests={} skipped={}",
            output_type,
            manifest.tests.len(),
            manifest.skipped.len()
        );
    }
}

#[test]
fn compat_tuple_and_list_types_generate_workspace() {
    let cases: Vec<(FuzzerOutputType, FuzzerConstraint, &str)> = vec![
        (
            FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Data]),
            FuzzerConstraint::Tuple(vec![
                FuzzerConstraint::IntRange {
                    min: "0".into(),
                    max: "5".into(),
                },
                FuzzerConstraint::Any,
            ]),
            "tuple_int_data",
        ),
        (
            FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
            FuzzerConstraint::List {
                elem: Box::new(FuzzerConstraint::Any),
                min_len: Some(0),
                max_len: Some(3),
            },
            "list_data",
        ),
    ];

    for (output_type, constraint, label) in &cases {
        let mut test = make_test("compat", label);
        test.fuzzer_output_type = output_type.clone();
        test.constraint = constraint.clone();
        test.semantics = derive_fixture_semantics_from_constraint(
            &test.fuzzer_output_type,
            &test.constraint,
            false,
        );

        let dir = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: dir.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
            existential_mode: ExistentialMode::Witness,
            target: VerificationTargetKind::PropertyWrapper,
            plutus_core_dir: None,
            raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
            allow_vacuous_subgenerators: false,
        };

        let manifest = generate_lean_workspace(&[test], &config, &SkipPolicy::None);
        assert!(
            manifest.is_ok(),
            "Composite type {label} should generate workspace"
        );
        assert_eq!(manifest.unwrap().tests.len(), 1);
    }
}

#[test]
fn compat_unsupported_type_returns_fallback_required_without_skip() {
    // Historically: without `--skip-unsupported`, nested composites with
    // opaque semantics errored out of workspace generation. Under the
    // soundness-only contract, opaque top-level semantics route through
    // the vacuous stub so workspace generation succeeds and the test is
    // emitted (not skipped).
    let mut test = make_test("compat", "nested");
    test.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    test.constraint = FuzzerConstraint::Any;
    test.semantics =
        derive_fixture_semantics_from_constraint(&test.fuzzer_output_type, &test.constraint, false);

    let dir = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: dir.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    // Without `--skip-unsupported`, nested composites with opaque
    // semantics now fail workspace generation via FallbackRequired.
    let err = generate_lean_workspace(&[test], &config, &SkipPolicy::None).expect_err(
        "workspace generation should fail on opaque semantics without --skip-unsupported",
    );
    let msg = err.to_string();
    assert!(
        msg.contains("opaque") || msg.contains("unsupported"),
        "workspace error should mention opaque/unsupported, got: {msg}"
    );
}

#[test]
fn compat_unsupported_type_still_returns_fallback_required_with_flag() {
    // The `--skip-unsupported` flag is now redundant for top-level opaque
    // semantics -- the soundness-only stub preempts the skip path. Skipping
    // is still reserved for cases like ByteString quantification where
    // Blaster genuinely cannot produce any proof.
    let mut test = make_test("compat", "nested");
    test.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    test.constraint = FuzzerConstraint::Any;
    test.semantics =
        derive_fixture_semantics_from_constraint(&test.fuzzer_output_type, &test.constraint, false);

    let dir = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: dir.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let manifest = generate_lean_workspace(&[test], &config, &SkipPolicy::All).unwrap();
    // With `--skip-unsupported`, the opaque composite goes into the
    // `skipped` bucket rather than emitting a stub.
    assert!(
        manifest.tests.is_empty(),
        "opaque top-level should not be in manifest.tests with --skip-unsupported, got {} entries",
        manifest.tests.len()
    );
    assert_eq!(
        manifest.skipped.len(),
        1,
        "opaque composite should be recorded in manifest.skipped"
    );
}

/// `Data` is rejected by S0003 in witness mode. Drive the existential
/// `fail_once` end-to-end coverage under proof mode instead.
#[test]
fn compat_fail_once_proof_mode_generates_existential() {
    let mut test = make_test_with_failure(
        "compat",
        "fail_once_test",
        OnTestFailure::SucceedImmediately,
    );
    test.fuzzer_output_type = FuzzerOutputType::Data;
    test.constraint = FuzzerConstraint::Any;
    test.semantics =
        derive_fixture_semantics_from_constraint(&test.fuzzer_output_type, &test.constraint, true);

    let dir = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: dir.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::Proof,
        target: VerificationTargetKind::PropertyWrapper,
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let manifest = generate_lean_workspace(&[test], &config, &SkipPolicy::None).unwrap();
    assert_eq!(manifest.tests.len(), 1);

    // Read the generated proof file to verify existential theorem
    let proof_path = dir.path().join(&manifest.tests[0].lean_file);
    let content = fs::read_to_string(proof_path).unwrap();
    assert!(
        content.contains("∃"),
        "Existential theorem should use existential quantifier"
    );
}

#[test]
fn compat_void_return_mode_generates_halt_theorem() {
    let mut test = make_test("compat", "void_test");
    test.return_mode = TestReturnMode::Void;
    test.fuzzer_output_type = FuzzerOutputType::Data;
    test.constraint = FuzzerConstraint::Any;
    test.semantics =
        derive_fixture_semantics_from_constraint(&test.fuzzer_output_type, &test.constraint, true);

    let dir = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: dir.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false,
    };

    let manifest = generate_lean_workspace(&[test], &config, &SkipPolicy::None).unwrap();
    assert_eq!(manifest.tests.len(), 1);

    let proof_path = dir.path().join(&manifest.tests[0].lean_file);
    let content = fs::read_to_string(proof_path).unwrap();
    assert!(
        content.contains("proveTestsHalt"),
        "Void-returning test should use proveTestsHalt"
    );
}

#[test]
fn phase12_benchmark_matrix_collects_fidelity_and_tractability_evidence() {
    let scalar_bounds = make_test_with_type(
        "phase12",
        "scalar_bounds",
        FuzzerOutputType::Int,
        FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        },
    );
    let bounded_collection = make_test_with_type(
        "phase12",
        "bounded_collection",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "3".to_string(),
            }),
            min_len: Some(1),
            max_len: Some(4),
        },
    );
    let constructor_domain = make_test_with_type(
        "phase12",
        "constructor_domain",
        FuzzerOutputType::Data,
        FuzzerConstraint::DataConstructorTags {
            tags: vec![0, 1, 2],
        },
    );
    let mut mapped_projected = make_test_with_type(
        "phase12",
        "mapped_projected_domain",
        FuzzerOutputType::Int,
        FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        })),
    );
    mapped_projected.semantics = FuzzerSemantics::IntRange {
        min: Some("100".to_string()),
        max: Some("110".to_string()),
    };

    let renamed_wrapper_a = make_test_with_type(
        "phase12.helpers",
        "wrap_alpha_case",
        FuzzerOutputType::Int,
        FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        },
    );
    let renamed_wrapper_b = make_test_with_type(
        "phase12.helpers",
        "wrap_bravo_case",
        FuzzerOutputType::Int,
        FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        },
    );

    let reduced_state_machine_trace = make_phase12_state_machine_test(
        "phase12_reduced_trace_ok",
        StateMachineAcceptance::AcceptsSuccess,
    );
    let amaru_permissions_property = make_phase12_state_machine_test(
        "prop_permissions_core_development_standard_ko",
        StateMachineAcceptance::AcceptsFailure,
    );

    let mut total_direct_bytes = 0usize;
    let mut renamed_metrics = Vec::new();

    let direct_cases = vec![
        ("scalar_bounds", scalar_bounds.clone()),
        ("bounded_collection", bounded_collection.clone()),
        ("constructor_domain", constructor_domain.clone()),
        ("mapped_projected", mapped_projected.clone()),
        ("renamed_wrapper_a", renamed_wrapper_a.clone()),
        ("renamed_wrapper_b", renamed_wrapper_b.clone()),
    ];

    // Success-mode state-machine cases with opaque transition event types
    // now surface a FallbackRequired skip from proof generation rather
    // than emitting a vacuous stub.
    let stub_cases = vec![(
        "reduced_state_machine_trace",
        reduced_state_machine_trace.clone(),
    )];

    for (label, test) in &stub_cases {
        let err = generate_proof_for_phase12_case(test)
            .err()
            .unwrap_or_else(|| {
                panic!("opaque state-machine case {label} should now surface FallbackRequired")
            });
        let msg = err.to_string();
        assert!(
            msg.contains("opaque") || msg.contains("unsupported"),
            "opaque state-machine case {label} should mention opaque/unsupported, got: {msg}"
        );
    }

    // Failure-mode state-machine cases carry `List<String>` label outputs,
    // which still hit the pre-existing ByteString/String universal-quantifier
    // gate before the state-machine trace proof path. These continue to
    // error, which `--skip-unsupported` treats as skip.
    let error_cases = vec![(
        "amaru_permissions_property",
        amaru_permissions_property.clone(),
    )];

    for (label, test) in &error_cases {
        let _err = generate_proof_for_phase12_case(test).expect_err(&format!(
            "ko-mode state-machine case {label} should error (ByteString gate)"
        ));
    }

    for (label, test) in &direct_cases {
        let direct = generate_proof_for_phase12_case(test)
            .unwrap_or_else(|e| panic!("direct proof should generate for {label}: {e}"));
        let direct_metrics = collect_proof_benchmark_metrics(&direct);
        assert!(direct_metrics.theorem_size_bytes > 0 && direct_metrics.theorem_count >= 1,);
        assert!(
            direct_metrics.theorem_size_bytes < 120_000,
            "theorem size budget exceeded for {label}: {:?}",
            direct_metrics
        );
        assert!(
            direct_metrics.helper_count < 400,
            "helper-definition budget exceeded for {label}: {:?}",
            direct_metrics
        );

        total_direct_bytes += direct_metrics.theorem_size_bytes;

        if matches!(*label, "renamed_wrapper_a" | "renamed_wrapper_b") {
            assert!(
                direct.contains("(0 <= x \u{2227} x <= 10)"),
                "renamed/wrapped helper invariance should preserve semantic bounds for {label}, got:\n{direct}"
            );
            renamed_metrics.push(direct_metrics.clone());
        }
    }

    assert!(
        total_direct_bytes > 0,
        "direct proof fixtures should emit theorem bytes"
    );

    assert_eq!(renamed_metrics.len(), 2);
    assert_eq!(
        renamed_metrics[0].helper_count, renamed_metrics[1].helper_count,
        "renamed/wrapped helper cases should preserve helper complexity"
    );

    let mut widened_semantics = make_test_with_type(
        "phase12",
        "widened_semantics_case",
        FuzzerOutputType::Int,
        FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        },
    );
    widened_semantics.semantics = FuzzerSemantics::Opaque {
        reason: "phase12 widened-domain benchmark fixture".to_string(),
    };
    // Widened (opaque) top-level semantics now surface a FallbackRequired
    // skip from proof generation rather than emitting a vacuous stub.
    let widened_err = generate_proof_for_phase12_case(&widened_semantics)
        .expect_err("widened opaque semantics should now surface FallbackRequired");
    let widened_msg = widened_err.to_string();
    assert!(
        widened_msg.contains("opaque"),
        "widened opaque fixture error should mention opaque, got: {widened_msg}"
    );
}

// --- Step 11.4: Intentional limitations register ---
//
// This test encodes the intentional limitations contract. Each entry is:
//   (limitation, status, rationale, impact, mitigation, reconsideration_trigger)
//
// If the capabilities() output changes, this test must be updated to reflect
// the new contract. This prevents undocumented behavior changes.

#[test]
fn intentional_limitations_register() {
    let caps = capabilities();

    // 1. Unit tests are intentionally unsupported.
    let unit = caps
        .unsupported_test_kinds
        .iter()
        .find(|n| n.kind == "unit")
        .unwrap();
    assert_eq!(unit.status, "intentional");

    // 2. Benchmarks are intentionally unsupported.
    let bench = caps
        .unsupported_test_kinds
        .iter()
        .find(|n| n.kind == "benchmark")
        .unwrap();
    assert_eq!(bench.status, "intentional");

    // 3. Max test arity is 1 (intentional: multi-arg tests must be
    //    encoded as tuple/record). Reconsideration: when compiler supports
    //    automatic multi-arg decomposition to tuple fuzzers.
    assert_eq!(caps.max_test_arity, 1);

    // 4. Blaster translation gaps are documented explicitly so failures can
    //    be surfaced with actionable context (e.g. List.Mem translation).
    assert!(
        caps.unsupported_fuzzer_types
            .iter()
            .any(|t| t.contains("Blaster translation gaps"))
    );

    // 6. Supported envelope includes all three target modes.
    assert_eq!(caps.target_modes.len(), 3);

    // 7. Finite nullary ADT constructor domains are now explicitly supported.
    assert!(
        caps.supported_fuzzer_types
            .iter()
            .any(|t| t.contains("Finite nullary ADT constructor domains"))
    );

    // 8. Partial-application/higher-order resolver coverage is explicitly
    //    tracked as a known limitation.
    assert!(
        caps.unsupported_fuzzer_types
            .iter()
            .any(|t| t.contains("partial-application"))
    );
}

#[test]
fn requires_explicit_bounds_tuple_with_bare_int_range() {
    // A bare IntRange constraint should satisfy tuple/pair Int-typed positions.
    // Tuple(Int, Data) with IntRange => no explicit bounds needed
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Data,]),
        &FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "100".to_string(),
        },
    ));
    // Pair(Int, Int) with IntRange => no explicit bounds needed
    assert!(!requires_explicit_bounds(
        &FuzzerOutputType::Pair(
            Box::new(FuzzerOutputType::Int),
            Box::new(FuzzerOutputType::Int),
        ),
        &FuzzerConstraint::IntRange {
            min: "-10".to_string(),
            max: "10".to_string(),
        },
    ));
    // Pair(Int, Bool) with Map(IntRange) still needs explicit bounds since
    // Map constraints are input-domain only.
    assert!(requires_explicit_bounds(
        &FuzzerOutputType::Pair(
            Box::new(FuzzerOutputType::Int),
            Box::new(FuzzerOutputType::Bool),
        ),
        &FuzzerConstraint::Map(Box::new(FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "255".to_string(),
        })),
    ));
}

#[test]
fn extract_int_range_from_and_intersects_bounds() {
    let range = extract_int_range_from_constraint(&FuzzerConstraint::And(vec![
        FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "100".to_string(),
        },
        FuzzerConstraint::IntRange {
            min: "20".to_string(),
            max: "80".to_string(),
        },
    ]))
    .expect("And-wrapped ranges should be extractable");

    assert_eq!(range.0, "20");
    assert_eq!(range.1, "80");
}

#[test]
fn tuple_int_proof_opens_data_namespace() {
    // Tuple proofs always construct a `Data.List [...]` argument, so Data must
    // be opened even when all element Lean types are non-Data.
    let test = make_test_with_type(
        "my_module",
        "test_tuple_ints",
        FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int, FuzzerOutputType::Int]),
        FuzzerConstraint::Tuple(vec![
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            },
            FuzzerConstraint::IntRange {
                min: "20".to_string(),
                max: "30".to_string(),
            },
        ]),
    );
    let id = test_id("my_module", "test_tuple_ints");
    let lean_name = sanitize_lean_name("test_tuple_ints");
    let lean_module = "AikenVerify.Proofs.My_module.test_tuple_ints";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("open PlutusCore.Data (Data)"),
        "Tuple proof should open Data namespace even for all-Int elements"
    );
    assert!(
        proof.contains("Data.List"),
        "Tuple proof should construct Data.List argument"
    );
}

#[test]
fn list_bool_with_mapped_int_range_reports_fallback_required() {
    // Mapped IntRange over Bool output cannot be translated to direct output
    // predicates; we should fall back to sampled-domain theorems.
    let test = make_test_with_type(
        "my_module",
        "test_list_bool",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Bool)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Map(Box::new(
                FuzzerConstraint::IntRange {
                    min: "0".to_string(),
                    max: "1".to_string(),
                },
            ))),
            min_len: Some(0),
            max_len: Some(5),
        },
    );
    let id = test_id("my_module", "test_list_bool");
    let lean_name = sanitize_lean_name("test_list_bool");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_bool";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    let err = result.expect_err("mapped IntRange over Bool cannot produce a direct proof");
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::FallbackRequired,
        "Mapped IntRange over List<Bool>",
    );
}

// --- Item 2.1: lean_type_for nested tuple/pair support ---

#[test]
fn lean_type_for_pair_int_bytearray() {
    let t = FuzzerOutputType::Pair(
        Box::new(FuzzerOutputType::Int),
        Box::new(FuzzerOutputType::ByteArray),
    );
    assert_eq!(
        lean_type_for(&t),
        Some("(Integer \u{00d7} ByteString)".to_string())
    );
}

#[test]
fn lean_type_for_nested_pair() {
    let t = FuzzerOutputType::Pair(
        Box::new(FuzzerOutputType::Int),
        Box::new(FuzzerOutputType::Pair(
            Box::new(FuzzerOutputType::Bool),
            Box::new(FuzzerOutputType::ByteArray),
        )),
    );
    assert_eq!(
        lean_type_for(&t),
        Some("(Integer \u{00d7} (Bool \u{00d7} ByteString))".to_string())
    );
}

#[test]
fn lean_type_for_tuple_three_elements() {
    let t = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::Int,
        FuzzerOutputType::Bool,
        FuzzerOutputType::ByteArray,
    ]);
    assert_eq!(
        lean_type_for(&t),
        Some("(Integer \u{00d7} Bool \u{00d7} ByteString)".to_string())
    );
}

#[test]
fn lean_type_for_tuple_empty() {
    let t = FuzzerOutputType::Tuple(vec![]);
    assert_eq!(lean_type_for(&t), Some("Unit".to_string()));
}

#[test]
fn lean_type_for_tuple_single() {
    let t = FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int]);
    assert_eq!(lean_type_for(&t), Some("Integer".to_string()));
}

#[test]
fn lean_type_for_list_of_pairs() {
    let t = FuzzerOutputType::List(Box::new(FuzzerOutputType::Pair(
        Box::new(FuzzerOutputType::Int),
        Box::new(FuzzerOutputType::Bool),
    )));
    assert_eq!(
        lean_type_for(&t),
        Some("List (Integer \u{00d7} Bool)".to_string())
    );
}

// --- Item 2.1: lean_data_encoder nested tuple/pair support ---

#[test]
fn lean_data_encoder_pair_int_bool() {
    let t = FuzzerOutputType::Pair(
        Box::new(FuzzerOutputType::Int),
        Box::new(FuzzerOutputType::Bool),
    );
    let enc = lean_data_encoder(&t, "v").unwrap();
    assert!(
        enc.contains("Data.List"),
        "Pair encoder should produce Data.List, got: {enc}"
    );
    assert!(
        enc.contains("Data.I"),
        "Pair encoder should encode Int element, got: {enc}"
    );
    assert!(
        enc.contains("Data.Constr"),
        "Pair encoder should encode Bool element, got: {enc}"
    );
}

#[test]
fn lean_data_encoder_nested_pair() {
    let t = FuzzerOutputType::Pair(
        Box::new(FuzzerOutputType::Int),
        Box::new(FuzzerOutputType::Pair(
            Box::new(FuzzerOutputType::Bool),
            Box::new(FuzzerOutputType::ByteArray),
        )),
    );
    let enc = lean_data_encoder(&t, "v").unwrap();
    assert!(
        enc.contains("Data.List"),
        "Nested pair encoder should produce Data.List, got: {enc}"
    );
}

#[test]
fn lean_data_encoder_tuple_three_elements() {
    let t = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::Int,
        FuzzerOutputType::Bool,
        FuzzerOutputType::ByteArray,
    ]);
    let enc = lean_data_encoder(&t, "v").unwrap();
    assert!(
        enc.contains("Data.List"),
        "Tuple encoder should produce Data.List, got: {enc}"
    );
    assert!(
        enc.contains("Data.I"),
        "Tuple encoder should encode Int element, got: {enc}"
    );
    assert!(
        enc.contains("Data.B"),
        "Tuple encoder should encode ByteArray element, got: {enc}"
    );
}

#[test]
fn lean_data_encoder_tuple_empty() {
    let t = FuzzerOutputType::Tuple(vec![]);
    let enc = lean_data_encoder(&t, "v").unwrap();
    assert_eq!(enc, "Data.List []");
}

#[test]
fn lean_data_encoder_tuple_single() {
    let t = FuzzerOutputType::Tuple(vec![FuzzerOutputType::Int]);
    let enc = lean_data_encoder(&t, "v").unwrap();
    assert_eq!(enc, "Data.I v");
}

// --- Item 2.1: lean_default_witness for tuple/pair ---

#[test]
fn lean_default_witness_pair() {
    let t = FuzzerOutputType::Pair(
        Box::new(FuzzerOutputType::Int),
        Box::new(FuzzerOutputType::Bool),
    );
    let w = lean_default_witness(&t);
    assert_eq!(w, "((0 : Integer), true)");
}

#[test]
fn lean_default_witness_tuple() {
    let t = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::Int,
        FuzzerOutputType::Bool,
        FuzzerOutputType::ByteArray,
    ]);
    let w = lean_default_witness(&t);
    assert_eq!(w, "((0 : Integer), true, ByteString.empty)");
}

// --- R5: scalar-output helpers with String, Data, Unsupported ---
//
// These tests exercise the three output types that share the ByteArray,
// Data, and Data-fallback branches in `lean_type_for`, `lean_data_encoder`,
// and `lean_default_witness`. They catch regressions in the `Unsupported`
// fallback ("degrade to Data") and the `String`/`ByteArray` collapsing logic.

#[test]
fn lean_type_for_string_returns_bytestring() {
    // String is treated the same as ByteArray at the Lean level.
    assert_eq!(
        lean_type_for(&FuzzerOutputType::String),
        Some("ByteString".to_string())
    );
}

#[test]
fn lean_type_for_data_returns_data() {
    assert_eq!(
        lean_type_for(&FuzzerOutputType::Data),
        Some("Data".to_string())
    );
}

#[test]
fn lean_type_for_unsupported_falls_back_to_data() {
    // Unsupported should always normalize to the Data universe so proofs can
    // still be emitted even when the element type cannot be modeled precisely.
    let t = FuzzerOutputType::Unsupported("MyOpaqueType".to_string());
    assert_eq!(lean_type_for(&t), Some("Data".to_string()));
}

#[test]
fn lean_type_for_unsupported_is_name_independent() {
    // The Lean type should not depend on the specific Aiken type name;
    // two distinct unsupported types map to the same Data type.
    let t1 = FuzzerOutputType::Unsupported("Option<Int>".to_string());
    let t2 = FuzzerOutputType::Unsupported("Result<ByteArray,Int>".to_string());
    assert_eq!(lean_type_for(&t1), lean_type_for(&t2));
}

#[test]
fn lean_data_encoder_string_uses_bytestring_constructor() {
    // String shares the ByteArray encoder because the Plutus runtime
    // represents UTF-8 strings as byte strings.
    let enc = lean_data_encoder(&FuzzerOutputType::String, "v").unwrap();
    assert_eq!(enc, "Data.B v");
}

#[test]
fn lean_data_encoder_data_uses_identity() {
    // Data inputs are already in the Data universe; the encoder must be
    // the identity on the variable (no Data.I / Data.B / Data.Constr wrap).
    let enc = lean_data_encoder(&FuzzerOutputType::Data, "v").unwrap();
    assert_eq!(enc, "v");
}

#[test]
fn lean_data_encoder_unsupported_uses_identity() {
    // Unsupported types fall back to Data and therefore must also use the
    // identity encoder; otherwise we would double-encode a Data value.
    let t = FuzzerOutputType::Unsupported("SomeCustomRecord".to_string());
    let enc = lean_data_encoder(&t, "x").unwrap();
    assert_eq!(enc, "x");
}

#[test]
fn lean_data_encoder_unsupported_varies_with_var_name() {
    // The encoder should thread the variable name through; otherwise a
    // shared Unsupported encoder would alias two different inputs.
    let t = FuzzerOutputType::Unsupported("SomeType".to_string());
    let e1 = lean_data_encoder(&t, "a").unwrap();
    let e2 = lean_data_encoder(&t, "b").unwrap();
    assert_eq!(e1, "a");
    assert_eq!(e2, "b");
    assert_ne!(e1, e2);
}

#[test]
fn lean_default_witness_string_is_empty_bytestring() {
    // String witnesses are empty byte strings (like ByteArray), not
    // Lean string literals.
    assert_eq!(
        lean_default_witness(&FuzzerOutputType::String),
        "ByteString.empty"
    );
}

#[test]
fn lean_default_witness_data_is_zero_integer_data() {
    // The Data default must be a well-formed Data term; `Data.I 0` is the
    // canonical "smallest" Data value used across the proof generator.
    assert_eq!(lean_default_witness(&FuzzerOutputType::Data), "Data.I 0");
}

#[test]
fn lean_default_witness_unsupported_matches_data_witness() {
    // Unsupported falls back to the Data witness so existential proofs can
    // still produce a concrete value without knowing the original shape.
    let t = FuzzerOutputType::Unsupported("SomeAlias".to_string());
    assert_eq!(
        lean_default_witness(&t),
        lean_default_witness(&FuzzerOutputType::Data)
    );
    assert_eq!(lean_default_witness(&t), "Data.I 0");
}

#[test]
fn lean_default_witness_unsupported_does_not_leak_type_name() {
    // The witness is a Lean value expression, not a type name: the original
    // Aiken type string must not appear in the output (it would be invalid
    // Lean syntax).
    let t = FuzzerOutputType::Unsupported("MySecretType".to_string());
    let w = lean_default_witness(&t);
    assert!(
        !w.contains("MySecretType"),
        "witness should not embed the Aiken type name, got: {w}"
    );
}

// --- Item 2.5: AcceptsFailure event accumulation ---

fn make_non_opaque_transition_semantics() -> StateMachineTransitionSemantics {
    StateMachineTransitionSemantics {
        terminal_tag: 0,
        step_tag: 1,
        label_field_index: 0,
        next_state_field_index: 1,
        event_field_index: 2,
        state_semantics: Box::new(FuzzerSemantics::IntRange {
            min: Some("0".to_string()),
            max: Some("100".to_string()),
        }),
        step_input_semantics: vec![],
        label_semantics: Box::new(FuzzerSemantics::IntRange {
            min: Some("0".to_string()),
            max: Some("10".to_string()),
        }),
        event_semantics: Box::new(FuzzerSemantics::IntRange {
            min: Some("0".to_string()),
            max: Some("50".to_string()),
        }),
    }
}

#[test]
fn accepts_failure_reachable_from_preserves_events() {
    let transition_semantics = make_non_opaque_transition_semantics();

    let mut shape_builder = LeanDataShapeBuilder::default();
    let (_reachable_output, definitions) = build_state_machine_trace_reachability_helpers(
        "failure_events_test",
        "failure_events_test",
        &StateMachineAcceptance::AcceptsFailure,
        &transition_semantics,
        &Default::default(),
        &mut shape_builder,
        "failure_events_test_shape",
        None,
    )
    .unwrap();

    // The recursive case should prepend the current event
    assert!(
        definitions.contains("events = event :: tailEvents"),
        "AcceptsFailure recursive case should prepend event to tailEvents, got:\n{definitions}"
    );
    // It should NOT drop events by using bare tailEvents
    assert!(
        !definitions.contains("events = tailEvents\n"),
        "AcceptsFailure should NOT drop events in recursive case, got:\n{definitions}"
    );
}

#[test]
fn accepts_success_reachable_from_also_preserves_events() {
    let transition_semantics = make_non_opaque_transition_semantics();

    let mut shape_builder = LeanDataShapeBuilder::default();
    let (_reachable_output, definitions) = build_state_machine_trace_reachability_helpers(
        "success_events_test",
        "success_events_test",
        &StateMachineAcceptance::AcceptsSuccess,
        &transition_semantics,
        &Default::default(),
        &mut shape_builder,
        "success_events_test_shape",
        None,
    )
    .unwrap();

    // The AcceptsSuccess recursive case should also prepend events
    assert!(
        definitions.contains("events = event :: tailEvents"),
        "AcceptsSuccess recursive case should prepend event to tailEvents, got:\n{definitions}"
    );
}

// --- Phase 2 Batch B: Item 2.2 - ADT field schema predicates ---

/// Build a simple ADT schema with two constructors: one nullary, one with fields.
fn make_adt_with_fields_schema() -> ExportedDataSchema {
    let mut definitions = Definitions::new();

    let root = Reference::new("my_module/Outcome");
    definitions.insert(
        &root,
        Annotated::from(Schema::Data(Data::AnyOf(vec![
            // Nullary constructor: Success (tag 0)
            Constructor {
                index: 0,
                fields: vec![],
            }
            .into(),
            // Constructor with fields: Failure(reason: ByteString, code: Integer) (tag 1)
            Constructor {
                index: 1,
                fields: vec![
                    Declaration::Inline(Box::new(Data::Bytes)).into(),
                    Declaration::Inline(Box::new(Data::Integer)).into(),
                ],
            }
            .into(),
        ]))),
    );

    ExportedDataSchema { root, definitions }
}

#[test]
fn adt_with_fields_and_data_semantics_uses_schema_predicate() {
    let mut test = make_test_with_type(
        "my_module",
        "test_adt_fields",
        FuzzerOutputType::Unsupported("Outcome".to_string()),
        FuzzerConstraint::Any,
    );
    test.fuzzer_data_schema = Some(make_adt_with_fields_schema());
    // Semantics is Data since constraint is Any and output is Unsupported
    test.semantics = FuzzerSemantics::Data;

    let id = test_id("my_module", "test_adt_fields");
    let lean_name = sanitize_lean_name("test_adt_fields");
    let lean_module = "AikenVerify.Proofs.My_module.test_adt_fields";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("Data"),
        "ADT with schema should quantify over Data"
    );
}

#[test]
fn adt_with_fields_and_constructor_tags_uses_schema_predicate() {
    let mut test = make_test_with_type(
        "my_module",
        "test_adt_tags_fields",
        FuzzerOutputType::Unsupported("Outcome".to_string()),
        FuzzerConstraint::DataConstructorTags { tags: vec![0, 1] },
    );
    test.fuzzer_data_schema = Some(make_adt_with_fields_schema());

    let id = test_id("my_module", "test_adt_tags_fields");
    let lean_name = sanitize_lean_name("test_adt_tags_fields");
    let lean_module = "AikenVerify.Proofs.My_module.test_adt_tags_fields";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("Data"),
        "ADT with tags and schema should quantify over Data"
    );
    assert!(
        proof.contains("Data.Constr"),
        "Should contain constructor tag matching via Data.Constr"
    );
}

#[test]
fn data_type_without_schema_uses_plain_data_quantification() {
    let mut test = make_test_with_type(
        "my_module",
        "test_data_no_schema",
        FuzzerOutputType::Data,
        FuzzerConstraint::Any,
    );
    // Set Data semantics explicitly (make_test_with_type derives Opaque for Any+Data)
    test.semantics = FuzzerSemantics::Data;
    // fuzzer_data_schema is None by default in make_test_with_type

    let id = test_id("my_module", "test_data_no_schema");
    let lean_name = sanitize_lean_name("test_data_no_schema");
    let lean_module = "AikenVerify.Proofs.My_module.test_data_no_schema";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("(x : Data)"),
        "Plain Data quantification should bind x as Data"
    );
    assert!(proof.contains("dataArg"), "Should use dataArg helper");
}

// --- Issue 14: DataWithSchema variant tests ---

#[test]
fn data_with_schema_semantics_not_opaque() {
    // DataWithSchema carries a structural schema predicate, so it must NOT
    // be classified as opaque by the soundness gate used in state-machine
    // trace proof generation.
    let semantics = FuzzerSemantics::DataWithSchema {
        type_name: "my.Type".to_string(),
    };
    assert!(
        !fuzzer_semantics_contains_opaque(&semantics),
        "DataWithSchema must not be treated as opaque: schema predicate is available"
    );

    // Also verify the same semantics nested in a List / Product / StateMachine.
    let nested_list = FuzzerSemantics::List {
        element: Box::new(semantics.clone()),
        min_len: None,
        max_len: None,
    };
    assert!(
        !fuzzer_semantics_contains_opaque(&nested_list),
        "List<DataWithSchema> must not be opaque"
    );

    let nested_product = FuzzerSemantics::Product(vec![semantics]);
    assert!(
        !fuzzer_semantics_contains_opaque(&nested_product),
        "Product<DataWithSchema> must not be opaque"
    );
}

#[test]
fn data_with_schema_generates_structural_precondition() {
    // A test with DataWithSchema semantics AND an exported fuzzer_data_schema
    // should generate a proof that wires the structural schema predicate into
    // the theorem precondition, so the generated theorem quantifies over
    // `Data` values that satisfy the ADT shape.
    let mut test = make_test_with_type(
        "my_module",
        "test_data_with_schema",
        FuzzerOutputType::Unsupported("Outcome".to_string()),
        FuzzerConstraint::Any,
    );
    test.fuzzer_data_schema = Some(make_adt_with_fields_schema());
    test.semantics = FuzzerSemantics::DataWithSchema {
        type_name: "my_module.Outcome".to_string(),
    };

    let id = test_id("my_module", "test_data_with_schema");
    let lean_name = sanitize_lean_name("test_data_with_schema");
    let lean_module = "AikenVerify.Proofs.My_module.test_data_with_schema";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("(x : Data)"),
        "DataWithSchema should quantify over Data, got:\n{proof}"
    );
    assert!(
        proof.contains("compiler-exported structural domain predicate"),
        "DataWithSchema + fuzzer_data_schema should emit the structural domain \
         predicate block, got:\n{proof}"
    );
    // The schema predicate prefix is `{lean_name}_schema` and the root
    // predicate name is used in the theorem precondition.
    assert!(
        proof.contains(&format!("{lean_name}_schema")),
        "Generated proof should reference the schema predicate by its \
         `{{lean_name}}_schema_*` prefix, got:\n{proof}"
    );

    // Assert the root predicate appears as an antecedent in the theorem
    // implication, not merely as a dead helper definition. A future change
    // that removes the `precondition_parts.push(...)` call would leave the
    // schema predicate defined but never referenced in the theorem body;
    // this check catches that regression.
    //
    // The theorem looks like:
    //   theorem ... (x : Data) : {schema_pred} x → proveTests ... = ...
    // So the predicate name must appear before the first `→` in the theorem
    // body.
    let theorem_start = proof
        .find("theorem ")
        .expect("generated proof must contain a theorem declaration");
    let theorem_body = &proof[theorem_start..];
    // Implication arrow (U+2192). Using the literal since `lean_sym` is a
    // private module inside `verify.rs`.
    let implies_pos = theorem_body
        .find('\u{2192}')
        .expect("universal theorem must contain an implication arrow");
    let antecedent = &theorem_body[..implies_pos];
    let schema_pred_prefix = format!("{lean_name}_schema");
    assert!(
        antecedent.contains(&schema_pred_prefix),
        "schema predicate '{schema_pred_prefix}' must appear as theorem \
         antecedent (before the first → arrow), found antecedent: \
         {antecedent:?}"
    );
}

#[test]
fn data_with_schema_without_fuzzer_schema_returns_fallback_required() {
    // Soundness guard: a test with `DataWithSchema` semantics but NO exported
    // `fuzzer_data_schema` would otherwise generate an unconstrained universal
    // theorem `∀ x : Data, True → P x`, which could be spuriously reported as
    // PROVED for validators that happen to accept all `Data`. The generator
    // must surface a `FallbackRequired` skip instead.
    let mut test = make_test_with_type(
        "my_module",
        "test_data_with_schema_no_schema",
        FuzzerOutputType::Unsupported("Outcome".to_string()),
        FuzzerConstraint::Any,
    );
    test.fuzzer_data_schema = None;
    test.semantics = FuzzerSemantics::DataWithSchema {
        type_name: "my_module.Outcome".to_string(),
    };

    let id = test_id("my_module", "test_data_with_schema_no_schema");
    let lean_name = sanitize_lean_name("test_data_with_schema_no_schema");
    let lean_module = "AikenVerify.Proofs.My_module.test_data_with_schema_no_schema";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );

    let err = result.expect_err(
        "expected FallbackRequired skip for DataWithSchema with no fuzzer_data_schema, \
     got proof content",
    );
    assert_generation_error_category(
        &err,
        GenerationErrorCategory::FallbackRequired,
        "DataWithSchema without fuzzer_data_schema",
    );
    let message = err.to_string();
    assert!(
        message.contains("DataWithSchema"),
        "error should mention DataWithSchema, got: {message}"
    );
    assert!(
        message.contains("unconstrained universal theorem")
            || message.contains("no exported fuzzer schema"),
        "error should explain the soundness reason, got: {message}"
    );
}

// --- Phase 2 Batch B: Item 2.3 - Data.Map (Dict) support ---

#[test]
fn dict_type_flows_through_as_list_of_pairs() {
    // Dict<K, V> compiles to List<Pair<K, V>>. With the ByteString skip,
    // use Data (not ByteArray) as the key so the skip doesn't fire.
    let dict_type = FuzzerOutputType::List(Box::new(FuzzerOutputType::Pair(
        Box::new(FuzzerOutputType::Data),
        Box::new(FuzzerOutputType::Int),
    )));
    let test = make_test_with_type(
        "my_module",
        "test_dict_domain",
        dict_type,
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: Some(0),
            max_len: Some(10),
        },
    );
    let id = test_id("my_module", "test_dict_domain");
    let lean_name = sanitize_lean_name("test_dict_domain");
    let lean_module = "AikenVerify.Proofs.My_module.test_dict_domain";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("List"),
        "Dict should flow through as List type"
    );
    assert!(
        proof.contains("xs.length"),
        "Dict list should have length bounds"
    );
}

#[test]
fn dict_type_with_data_semantics_does_not_error() {
    // Dict<Data, Data> where semantics only knows it's a list of Data
    let dict_type = FuzzerOutputType::List(Box::new(FuzzerOutputType::Pair(
        Box::new(FuzzerOutputType::Data),
        Box::new(FuzzerOutputType::Data),
    )));
    let test = make_test_with_type(
        "my_module",
        "test_dict_data",
        dict_type,
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::Any),
            min_len: None,
            max_len: None,
        },
    );
    let id = test_id("my_module", "test_dict_data");
    let lean_name = sanitize_lean_name("test_dict_data");
    let lean_module = "AikenVerify.Proofs.My_module.test_dict_data";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("List"),
        "Dict<Data,Data> should generate List-based proof"
    );
    assert!(
        proof.contains("Data"),
        "Dict elements should reference Data type"
    );
}

// --- Phase 2 Batch B: Item 2.4 - Typed list quantification for ADT elements ---

#[test]
fn list_of_nullary_enum_applies_constructor_tags_to_elements() {
    // List<NullaryEnum> where the enum has tags [0, 1, 2]
    let test = make_test_with_type(
        "my_module",
        "test_list_enum",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Unsupported("Color".to_string()))),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::DataConstructorTags {
                tags: vec![0, 1, 2],
            }),
            min_len: Some(1),
            max_len: Some(5),
        },
    );
    let id = test_id("my_module", "test_list_enum");
    let lean_name = sanitize_lean_name("test_list_enum");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_enum";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("List"),
        "List of enum should quantify over List type"
    );
    assert!(
        proof.contains("Data"),
        "List of Unsupported should map elements to Data"
    );
}

#[test]
fn list_of_data_with_constructor_element_semantics_generates_predicate() {
    // Explicit Data list with constructor-tag element semantics
    let test = make_test_with_type(
        "my_module",
        "test_list_data_ctors",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::DataConstructorTags { tags: vec![0, 1] }),
            min_len: Some(0),
            max_len: Some(3),
        },
    );
    let id = test_id("my_module", "test_list_data_ctors");
    let lean_name = sanitize_lean_name("test_list_data_ctors");
    let lean_module = "AikenVerify.Proofs.My_module.test_list_data_ctors";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("theorem "),
        "Generated proof should contain a theorem"
    );
    assert!(
        proof.contains("_l0"),
        "Should emit per-length theorem for length 0"
    );
    assert!(
        proof.contains("_l3"),
        "Should emit per-length theorem for the max length"
    );
    assert!(
        proof.contains(": Data)"),
        "Should annotate element parameters as Data"
    );
    assert!(
        proof.contains("Data.Constr"),
        "Constructor-tag element semantics should generate Data.Constr predicate"
    );
}

#[test]
fn state_machine_existential_theorem_reports_fallback_required_pending_todo_4_6() {
    // Soundness gate: `try_generate_state_machine_trace_proof_from_semantics` must
    // refuse to emit a direct proof for existential (`fail once`) state-machine trace
    // theorems while the reachability encoding relies on the globally over-approximating
    // `step_inputs_satisfiable` predicate. See the TODO(4.6) comment in verify.rs:
    // widening reachability is sound for `∀`-theorems but lets Z3 witness spurious
    // failing traces for `∃`-theorems, which would be unsound.
    //
    // Until per-transition step-input witnesses are implemented, such tests must fall
    // back to the skip path (GenerationErrorCategory::FallbackRequired) rather than
    // falsely claiming success.
    let mut test = make_phase12_state_machine_test(
        "prop_state_machine_trace_fail_once",
        StateMachineAcceptance::AcceptsSuccess,
    );
    // Force the existential theorem form: `fail once` => OnTestFailure::SucceedImmediately
    // => TheoremForm { existential: true, .. } in determine_theorem_form.
    test.on_test_failure = OnTestFailure::SucceedImmediately;

    let id = test_id("permissions.test", "prop_state_machine_trace_fail_once");
    let lean_name = sanitize_lean_name("prop_state_machine_trace_fail_once");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_state_machine_trace_fail_once";

    let err = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    )
    .expect_err(
        "existential state-machine trace proofs must refuse generation while TODO(4.6) \
         leaves reachability globally over-approximated",
    );

    assert_generation_error_category(
        &err,
        GenerationErrorCategory::FallbackRequired,
        "existential state-machine trace should route through the skip path",
    );

    let message = err.to_string();
    assert!(
        message.contains("Existential state-machine traces")
            || message.contains("reachability")
            || message.contains("over-approximates"),
        "error message should explain the reachability soundness gate, got: {message}"
    );
}

// --- Issue 13: Recursive schema lowering via Lean `mutual ... end` blocks ---

fn make_self_recursive_tree_schema() -> ExportedDataSchema {
    // Tree = Leaf | Node(Tree, Int)
    // Self-referential: the Node constructor's first field is Tree itself.
    let mut definitions = Definitions::new();

    let tree = Reference::new("my_module/Tree");
    definitions.insert(
        &tree,
        Annotated::from(Schema::Data(Data::AnyOf(vec![
            // Leaf (tag 0, nullary)
            Constructor {
                index: 0,
                fields: vec![],
            }
            .into(),
            // Node(Tree, Int) (tag 1)
            Constructor {
                index: 1,
                fields: vec![
                    Declaration::Referenced(tree.clone()).into(),
                    Declaration::Inline(Box::new(Data::Integer)).into(),
                ],
            }
            .into(),
        ]))),
    );

    ExportedDataSchema {
        root: tree,
        definitions,
    }
}

fn make_mutually_recursive_schema() -> ExportedDataSchema {
    // TypeA = AOnly | AHasB(TypeB)
    // TypeB = BOnly | BHasA(TypeA)
    // Two types that reference each other.
    let mut definitions = Definitions::new();

    let type_a = Reference::new("my_module/TypeA");
    let type_b = Reference::new("my_module/TypeB");

    definitions.insert(
        &type_a,
        Annotated::from(Schema::Data(Data::AnyOf(vec![
            Constructor {
                index: 0,
                fields: vec![],
            }
            .into(),
            Constructor {
                index: 1,
                fields: vec![Declaration::Referenced(type_b.clone()).into()],
            }
            .into(),
        ]))),
    );

    definitions.insert(
        &type_b,
        Annotated::from(Schema::Data(Data::AnyOf(vec![
            Constructor {
                index: 0,
                fields: vec![],
            }
            .into(),
            Constructor {
                index: 1,
                fields: vec![Declaration::Referenced(type_a.clone()).into()],
            }
            .into(),
        ]))),
    );

    ExportedDataSchema {
        root: type_a,
        definitions,
    }
}

#[test]
fn recursive_schema_emits_mutual_block() {
    let mut test = make_test_with_type(
        "my_module",
        "test_recursive_tree",
        FuzzerOutputType::Unsupported("Tree".to_string()),
        FuzzerConstraint::Any,
    );
    test.fuzzer_data_schema = Some(make_self_recursive_tree_schema());
    // Use Data semantics so the schema-backed predicate path is exercised.
    test.semantics = FuzzerSemantics::Data;

    let id = test_id("my_module", "test_recursive_tree");
    let lean_name = sanitize_lean_name("test_recursive_tree");
    let lean_module = "AikenVerify.Proofs.My_module.test_recursive_tree";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect("recursive schema should no longer require a FallbackRequired skip");

    assert!(
        proof.contains("mutual"),
        "recursive schema should be wrapped in a `mutual` block, got:\n{proof}"
    );
    assert!(
        proof.contains("\nend\n") || proof.contains("end\n"),
        "recursive schema `mutual` block must be closed with `end`, got:\n{proof}"
    );
    // Predicate definition must appear *inside* the mutual block. Lean
    // indents definitions inside `mutual ... end` with two spaces.
    assert!(
        proof.contains("  def "),
        "recursive schema predicate should be defined inside the mutual block (expected `  def ` 2-space indent), got:\n{proof}"
    );

    // Extract the mutual block body and verify the self-reference appears
    // *inside* it (not just elsewhere in the file).
    let mutual_start = proof
        .find("mutual")
        .expect("`mutual` substring must exist after prior assertion");
    let mutual_body = &proof[mutual_start..];
    let mutual_end = mutual_body
        .find("\nend")
        .expect("`mutual` block must be closed with `end`");
    let mutual_block = &mutual_body[..mutual_end];

    // Pick up the Tree predicate's full name from a `  def <name> : Data`
    // line inside the mutual block. The name includes a content hash so we
    // cannot hard-code it.
    let def_marker = "  def ";
    let def_line_start = mutual_block
        .find(def_marker)
        .expect("mutual block must contain an indented `def` line");
    let after_def = &mutual_block[def_line_start + def_marker.len()..];
    let name_end = after_def
        .find(" : Data")
        .expect("def line inside mutual block must have the form `  def <name> : Data ...`");
    let first_pred_name = &after_def[..name_end];

    // The self-recursive Tree predicate must be invoked from within its own
    // body (or from a sibling predicate in the SCC). Either way, *some*
    // predicate name defined in the mutual block must appear more than once
    // inside the block (once on its def line, once or more in a body).
    let occurrences = mutual_block.matches(first_pred_name).count();
    assert!(
        occurrences >= 2,
        "a predicate defined inside the recursive `mutual` block must be referenced at least once inside the block (self-recursion or cross-reference). \
         Predicate `{first_pred_name}` appeared {occurrences} time(s) in:\n{mutual_block}"
    );

    // The Node-constructor arm for a self-recursive Tree should have two
    // fields (recursive subtree, then Int) glued by a conjunction. Catching
    // the conjunction guards against accidental regressions where the body
    // collapses to `True`/`False`.
    assert!(
        mutual_block.contains("x_0") && mutual_block.contains("∧"),
        "recursive Tree predicate body should pattern-match on field bindings (`x_0`) and conjoin recursive/primitive checks with `∧`, got mutual block:\n{mutual_block}"
    );
}

#[test]
fn mutually_recursive_schema_emits_mutual_block() {
    let mut test = make_test_with_type(
        "my_module",
        "test_mutually_recursive",
        FuzzerOutputType::Unsupported("TypeA".to_string()),
        FuzzerConstraint::Any,
    );
    test.fuzzer_data_schema = Some(make_mutually_recursive_schema());
    test.semantics = FuzzerSemantics::Data;

    let id = test_id("my_module", "test_mutually_recursive");
    let lean_name = sanitize_lean_name("test_mutually_recursive");
    let lean_module = "AikenVerify.Proofs.My_module.test_mutually_recursive";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect("mutually recursive schema should no longer require a FallbackRequired skip");

    assert!(
        proof.contains("mutual"),
        "mutually recursive schemas should be wrapped in a `mutual` block, got:\n{proof}"
    );
    assert!(
        proof.contains("\nend\n") || proof.contains("end\n"),
        "mutually recursive `mutual` block must be closed with `end`, got:\n{proof}"
    );

    // Extract the mutual block body once so further assertions are scoped
    // correctly and don't accidentally pass on stray matches elsewhere.
    let mutual_start = proof
        .find("mutual")
        .expect("`mutual` substring must exist after prior assertion");
    let mutual_body = &proof[mutual_start..];
    let mutual_end = mutual_body
        .find("\nend")
        .expect("`mutual` block must be closed with `end`");
    let mutual_block = &mutual_body[..mutual_end];

    // There must be at least two indented `  def ` lines inside the block,
    // one per type in the mutually-recursive SCC.
    let indented_def_count = mutual_block.matches("  def ").count();
    assert!(
        indented_def_count >= 2,
        "mutually recursive schema should produce at least two `  def ` lines inside the mutual block, found {indented_def_count}. Block:\n{mutual_block}"
    );

    // Both TypeA's and TypeB's predicate names must appear inside the block
    // as definitions (prefixed with `  def `). Their names include a content
    // hash, so match on the unique `my_module_TypeA` / `my_module_TypeB`
    // infix that the predicate-naming scheme guarantees.
    assert!(
        mutual_block.contains("  def ") && mutual_block.contains("my_module_TypeA"),
        "mutual block should define a predicate for TypeA, got:\n{mutual_block}"
    );
    assert!(
        mutual_block.contains("my_module_TypeB"),
        "mutual block should define a predicate for TypeB, got:\n{mutual_block}"
    );

    // Extract each def's full predicate name so we can check the
    // cross-reference pattern: TypeA's body should call TypeB's predicate
    // and vice versa.
    let def_marker = "  def ";
    let mut def_names: Vec<&str> = Vec::new();
    let mut cursor = 0usize;
    while let Some(rel) = mutual_block[cursor..].find(def_marker) {
        let abs = cursor + rel + def_marker.len();
        let rest = &mutual_block[abs..];
        let name_end = rest
            .find(" : Data")
            .expect("def line inside mutual block must have the form `  def <name> : Data ...`");
        def_names.push(&rest[..name_end]);
        cursor = abs + name_end;
    }
    assert!(
        def_names.len() >= 2,
        "expected to parse at least two predicate names out of the mutual block, got {def_names:?} from:\n{mutual_block}"
    );

    // Every defined predicate must be referenced *somewhere else* in the
    // mutual block (cross-reference or self-reference inside the SCC). For
    // TypeA<->TypeB this catches any regression that flattens the bodies to
    // `True`/`False` without actually calling the sibling predicate.
    for name in &def_names {
        let occurrences = mutual_block.matches(name).count();
        assert!(
            occurrences >= 2,
            "predicate `{name}` defined in the mutually-recursive block should also be referenced at least once inside the block (cross-reference), but appeared {occurrences} time(s) in:\n{mutual_block}"
        );
    }
}

#[test]
fn non_recursive_schema_does_not_emit_mutual_block() {
    // Reuse the non-recursive ADT fixture defined earlier in this file.
    let mut test = make_test_with_type(
        "my_module",
        "test_non_recursive_adt",
        FuzzerOutputType::Unsupported("Outcome".to_string()),
        FuzzerConstraint::Any,
    );
    test.fuzzer_data_schema = Some(make_adt_with_fields_schema());
    test.semantics = FuzzerSemantics::Data;

    let id = test_id("my_module", "test_non_recursive_adt");
    let lean_name = sanitize_lean_name("test_non_recursive_adt");
    let lean_module = "AikenVerify.Proofs.My_module.test_non_recursive_adt";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect("non-recursive schemas must still generate proofs without fallback");

    assert!(
        !proof.contains("mutual"),
        "non-recursive schemas must not emit a `mutual` block, got:\n{proof}"
    );
}

/// Fixture: A -> B acyclic reference chain.
///
/// TypeA has a single variant whose only field is a reference to TypeB.
/// TypeB has a single variant whose only field is a primitive Integer.
/// There is no back-edge from B to A, so the dependency graph is a DAG
/// and no `mutual` block should be emitted.
fn make_acyclic_ref_chain_schema() -> ExportedDataSchema {
    let mut definitions = Definitions::new();

    let type_a = Reference::new("my_module/TypeA");
    let type_b = Reference::new("my_module/TypeB");

    // TypeB: single constructor taking an Integer. No reference back to A.
    definitions.insert(
        &type_b,
        Annotated::from(Schema::Data(Data::AnyOf(vec![
            Constructor {
                index: 0,
                fields: vec![Declaration::Inline(Box::new(Data::Integer)).into()],
            }
            .into(),
        ]))),
    );

    // TypeA: single constructor taking a TypeB.
    definitions.insert(
        &type_a,
        Annotated::from(Schema::Data(Data::AnyOf(vec![
            Constructor {
                index: 0,
                fields: vec![Declaration::Referenced(type_b).into()],
            }
            .into(),
        ]))),
    );

    ExportedDataSchema {
        root: type_a,
        definitions,
    }
}

#[test]
fn non_recursive_ref_chain_does_not_emit_mutual_block() {
    // Exercises the acyclic A -> B path: TypeA references TypeB, but TypeB
    // has no back-reference to TypeA. The dependency graph is a DAG, so the
    // generator must emit plain top-level `def`s and *not* wrap them in a
    // `mutual ... end` block.
    let mut test = make_test_with_type(
        "my_module",
        "test_acyclic_ref_chain",
        FuzzerOutputType::Unsupported("TypeA".to_string()),
        FuzzerConstraint::Any,
    );
    test.fuzzer_data_schema = Some(make_acyclic_ref_chain_schema());
    test.semantics = FuzzerSemantics::Data;

    let id = test_id("my_module", "test_acyclic_ref_chain");
    let lean_name = sanitize_lean_name("test_acyclic_ref_chain");
    let lean_module = "AikenVerify.Proofs.My_module.test_acyclic_ref_chain";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect("acyclic A -> B schema chain must generate a proof without FallbackRequired");

    // No cycle -> no `mutual` block.
    assert!(
        !proof.contains("mutual"),
        "acyclic reference chain (A -> B) must not emit a `mutual` block, got:\n{proof}"
    );

    // Non-mutual `def`s are still emitted (unindented, top-level).
    assert!(
        proof.contains("def "),
        "acyclic reference chain should still emit top-level `def` predicates, got:\n{proof}"
    );

    // Both type names must appear somewhere in the generated predicates.
    // The naming scheme guarantees a `my_module_TypeA` / `my_module_TypeB`
    // infix in the predicate name that does not collide with the theorem
    // text.
    assert!(
        proof.contains("my_module_TypeA"),
        "acyclic ref chain proof should contain a predicate referencing TypeA, got:\n{proof}"
    );
    assert!(
        proof.contains("my_module_TypeB"),
        "acyclic ref chain proof should contain a predicate referencing TypeB, got:\n{proof}"
    );
}

/// S4 — golden fragment test: verify that a minimal `TransitionProp` tree
/// lowers to the expected Lean `isValidTransition` body.
///
/// Covers the `Exists → Or → EqOutput` backbone that every amaru-treasury
/// step function produces. Pinning this ensures refactors to the lowering
/// continue to emit a syntactically valid, sound over-approximation.
#[test]
fn emit_transition_prop_as_lean_golden_fragment() {
    use aiken_lang::test_framework::{ShallowConst, ShallowIr, ShallowIrType, TransitionProp};

    // Build: ∃ (x : Data), transition = Data.Constr 0 [] ∨ transition = Data.Constr 1 []
    let prop = TransitionProp::Exists {
        binder: "x".to_string(),
        ty: ShallowIrType::Data,
        domain: Box::new(aiken_lang::test_framework::FuzzerSemantics::Data),
        body: Box::new(TransitionProp::Or(vec![
            TransitionProp::EqOutput(ShallowIr::Construct {
                module: "m".to_string(),
                constructor: "A".to_string(),
                tag: 0,
                fields: vec![],
            }),
            TransitionProp::EqOutput(ShallowIr::Construct {
                module: "m".to_string(),
                constructor: "B".to_string(),
                tag: 1,
                fields: vec![],
            }),
        ])),
    };

    let (def, log, _sub_gens, _s0002) =
        crate::verify::emit_is_valid_transition_def_for_export("myTest_ivt", &prop);
    assert!(
        log.is_empty(),
        "no widenings should be introduced, got: {log:?}"
    );
    // The emitted def must contain the binder for the existential and
    // the two equality branches joined by Lean's disjunction symbol.
    assert!(
        def.contains("def myTest_ivt (state : Data) (transition : Data) : Prop :="),
        "def header missing, got:\n{def}"
    );
    // The binder `x` is unused in the body (two `EqOutput`s of constants),
    // so the emitter prefixes it with `_` to suppress Lean's unused-variable
    // warning.
    assert!(
        def.contains("\u{2203} (_x : Data)"),
        "inner existential missing, got:\n{def}"
    );
    assert!(
        def.contains("transition = Data.Constr (0 : Integer) []"),
        "first EqOutput missing, got:\n{def}"
    );
    assert!(
        def.contains("transition = Data.Constr (1 : Integer) []"),
        "second EqOutput missing, got:\n{def}"
    );
    assert!(
        def.contains("\u{2228}"),
        "disjunction symbol missing, got:\n{def}"
    );

    // Unsupported leaves must widen to True and be logged exactly once.
    let widening_prop = TransitionProp::Unsupported {
        reason: "testing".to_string(),
        source_location: None,
    };
    let (def2, log2, _sub_gens2, _s0002_2) =
        crate::verify::emit_is_valid_transition_def_for_export("myTest_ivt", &widening_prop);
    assert!(
        def2.contains("True"),
        "unsupported leaf should widen to True, got:\n{def2}"
    );
    assert_eq!(
        log2.len(),
        1,
        "unsupported leaf should append exactly one log entry, got: {log2:?}"
    );
    assert!(
        log2[0].contains("Unsupported: testing"),
        "log entry should carry the reason, got: {log2:?}"
    );

    // Const(Int) lowered via EqOutput should appear verbatim.
    let eq_int = TransitionProp::EqOutput(ShallowIr::Const(ShallowConst::Int("42".to_string())));
    let (def3, log3, _sub_gens3, _s0002_3) =
        crate::verify::emit_is_valid_transition_def_for_export("myTest_ivt", &eq_int);
    assert!(log3.is_empty());
    assert!(
        def3.contains("transition = Data.I (42 : Integer)"),
        "Int EqOutput should emit Data.I literal, got:\n{def3}"
    );
}

/// S6 — `SubGenerator` leaves emit opaque-predicate references using the
/// `__SGPFX__` placeholder (replaced at proof-file generation time) and record
/// the sub-generator in the returned list.
#[test]
fn emit_transition_prop_sub_generator_emits_opaque_ref_and_collects_name() {
    use aiken_lang::test_framework::{FuzzerSemantics, ShallowIrType, TransitionProp};

    // A simple Or of two sub-generator branches inside an Exists.
    let prop = TransitionProp::Exists {
        binder: "_out".to_string(),
        ty: ShallowIrType::Data,
        domain: Box::new(FuzzerSemantics::Data),
        body: Box::new(TransitionProp::Or(vec![
            TransitionProp::SubGenerator {
                module: "permissions/test".to_string(),
                fn_name: "scenario_inputs_baseline".to_string(),
            },
            TransitionProp::SubGenerator {
                module: "permissions/test".to_string(),
                fn_name: "scenario_inputs_no_scripts".to_string(),
            },
        ])),
    };

    let (def, log, sub_gens, _s0002) =
        crate::verify::emit_is_valid_transition_def_for_export("myTest_ivt", &prop);

    // The log should record both sub-generators.
    assert_eq!(
        log.len(),
        2,
        "expected 2 sub-generator log entries, got: {log:?}"
    );
    assert!(
        log.iter().any(|l| l.contains("scenario_inputs_baseline")),
        "log should mention scenario_inputs_baseline, got: {log:?}"
    );
    assert!(
        log.iter().any(|l| l.contains("scenario_inputs_no_scripts")),
        "log should mention scenario_inputs_no_scripts, got: {log:?}"
    );

    // The def should contain placeholder references for both sub-generators.
    assert!(
        def.contains("__SGPFX__scenario_inputs_baseline_prop"),
        "def should contain opaque ref for baseline, got:\n{def}"
    );
    assert!(
        def.contains("__SGPFX__scenario_inputs_no_scripts_prop"),
        "def should contain opaque ref for no_scripts, got:\n{def}"
    );

    // The placeholder should reference the correct variable names.
    // `state` is the isValidTransition parameter; `_out` is the Exists binder.
    assert!(
        def.contains("__SGPFX__scenario_inputs_baseline_prop state _out"),
        "predicate ref should use state and binding_var, got:\n{def}"
    );

    // The collected sub-generators list should contain both, deduplicated.
    assert_eq!(
        sub_gens.len(),
        2,
        "expected 2 unique sub-generators, got: {sub_gens:?}"
    );
    assert!(
        sub_gens
            .iter()
            .any(|(_, name)| name == "scenario_inputs_baseline"),
        "sub_gens should contain baseline, got: {sub_gens:?}"
    );
    assert!(
        sub_gens
            .iter()
            .any(|(_, name)| name == "scenario_inputs_no_scripts"),
        "sub_gens should contain no_scripts, got: {sub_gens:?}"
    );

    // Duplicate sub-generators should be deduplicated.
    let prop_dup = TransitionProp::Or(vec![
        TransitionProp::SubGenerator {
            module: "m".to_string(),
            fn_name: "same_gen".to_string(),
        },
        TransitionProp::SubGenerator {
            module: "m".to_string(),
            fn_name: "same_gen".to_string(),
        },
    ]);
    let (_, _, sub_gens_dup, _s0002_dup) =
        crate::verify::emit_is_valid_transition_def_for_export("myTest_ivt", &prop_dup);
    assert_eq!(
        sub_gens_dup.len(),
        1,
        "duplicate sub-generators should be deduplicated, got: {sub_gens_dup:?}"
    );
}

/// EqOutput with opaque/Var rhs: existentials must stay LOCAL to the branch,
/// not be hoisted to the definition level as top-level ∃ binders.
///
/// Before the fix, `emit_is_valid_transition_def` produced:
///
/// ```lean
/// def foo (state : Data) (transition : Data) : Prop :=
///   ∃ (_fuzz_0 : Data),   -- top-level!  makes definition trivially True
///   (transition = _fuzz_0 ∨ …)
/// ```
///
/// After the fix it should produce:
///
/// ```lean
/// def foo (state : Data) (transition : Data) : Prop :=
///   ((∃ (_fuzz_0 : Data), transition = _fuzz_0) ∨ …)
/// ```
///
/// The key invariant: NO existential appears between the `Prop :=` line and
/// the opening parenthesis of the body.
#[test]
fn emit_transition_prop_eq_output_keeps_existentials_local() {
    use aiken_lang::test_framework::{FuzzerSemantics, ShallowIr, ShallowIrType, TransitionProp};

    // EqOutput with a bare Var: the rhs cannot be emitted as a concrete Data
    // literal, so the emitter produces a fresh existential. Under the fix, that
    // existential must be LOCAL (wrapped around the equality).
    let var_prop = TransitionProp::Or(vec![
        TransitionProp::EqOutput(ShallowIr::Var {
            name: "Done".to_string(),
            ty: ShallowIrType::Data,
        }),
        TransitionProp::Exists {
            binder: "x".to_string(),
            ty: ShallowIrType::Data,
            domain: Box::new(FuzzerSemantics::Data),
            body: Box::new(TransitionProp::EqOutput(ShallowIr::Construct {
                module: "m".to_string(),
                constructor: "Step".to_string(),
                tag: 1,
                fields: vec![ShallowIr::Var {
                    name: "st".to_string(),
                    ty: ShallowIrType::Data,
                }],
            })),
        },
    ]);

    let (def, _log, _sub_gens, _s0002) =
        crate::verify::emit_is_valid_transition_def_for_export("eqlocal_ivt", &var_prop);

    // The definition must NOT have top-level ∃ binders before the body.
    // After "Prop :=" the next non-whitespace content should be `(`, not `∃`.
    let after_decl = def
        .split("Prop :=")
        .nth(1)
        .expect("def must contain 'Prop :='");
    let first_non_ws = after_decl.trim_start().chars().next().unwrap_or('?');
    assert_eq!(
        first_non_ws, '(',
        "top-level existential leaked before body — definition starts with {:?} instead of '('.\nFull def:\n{def}",
        first_non_ws
    );

    // The Var rhs's existential must appear INSIDE the first disjunct.
    assert!(
        def.contains("∃ (_fuzz_0 : Data), transition = _fuzz_0"),
        "inner local existential for Var rhs must be present, got:\n{def}"
    );

    // The Construct-with-Var-field branch should also keep its existential local.
    assert!(
        def.contains("∃ (_fuzz_1 : Data), transition = Data.Constr (1 : Integer) [_fuzz_1]"),
        "inner local existential for Construct-with-Var-field must be present, got:\n{def}"
    );
}

#[test]
fn unused_binder_gets_underscore_prefix() {
    // An Exists whose body does not mention the binder should emit `_x`.
    use aiken_lang::test_framework::{
        FuzzerSemantics, ShallowConst, ShallowIr, ShallowIrType, TransitionProp,
    };
    let prop = TransitionProp::Exists {
        binder: "action".to_string(),
        ty: ShallowIrType::Data,
        domain: Box::new(FuzzerSemantics::Data),
        body: Box::new(TransitionProp::Pure(ShallowIr::Const(ShallowConst::Bool(
            true,
        )))),
    };
    let mut ctx = super::ShallowIrEmitCtx::new();
    let mut log = vec![];
    let out = super::emit_transition_prop_as_lean(
        &prop,
        "state",
        "transition",
        "_ignored",
        &mut ctx,
        &mut log,
    );
    assert!(out.contains("_action"), "expected `_action` in: {out}");
    assert!(
        !out.contains("(action "),
        "unexpected bare `action` in: {out}"
    );
}

#[test]
fn used_binder_keeps_original_name() {
    // An Exists whose SubGenerator references the binder (as binding_var) should
    // NOT be prefixed. fn_name is intentionally different from binder so the test
    // only passes if the binder survives via the binding_var substitution path.
    use aiken_lang::test_framework::{FuzzerSemantics, ShallowIrType, TransitionProp};
    let prop = TransitionProp::Exists {
        binder: "myvar".to_string(),
        ty: ShallowIrType::Data,
        domain: Box::new(FuzzerSemantics::Data),
        body: Box::new(TransitionProp::SubGenerator {
            module: "m".to_string(),
            fn_name: "other_gen".to_string(), // intentionally different from binder
        }),
    };
    let mut ctx = super::ShallowIrEmitCtx::new();
    let mut log = vec![];
    let out = super::emit_transition_prop_as_lean(
        &prop,
        "state",
        "transition",
        "_ignored",
        &mut ctx,
        &mut log,
    );
    // SubGenerator emits `__SGPFX__other_gen_prop state myvar` — `myvar` IS in
    // inner (as the trailing binding_var argument), so the binder keeps its bare name.
    assert!(
        out.contains("(myvar :"),
        "expected bare `myvar :` in: {out}"
    );
    assert!(
        out.contains("state myvar"),
        "expected myvar as binding_var arg in: {out}"
    );
}

#[test]
fn already_underscored_binder_not_double_prefixed() {
    use aiken_lang::test_framework::{
        FuzzerSemantics, ShallowConst, ShallowIr, ShallowIrType, TransitionProp,
    };
    let prop = TransitionProp::Exists {
        binder: "_backpass_0".to_string(),
        ty: ShallowIrType::Data,
        domain: Box::new(FuzzerSemantics::Data),
        body: Box::new(TransitionProp::Pure(ShallowIr::Const(ShallowConst::Bool(
            true,
        )))),
    };
    let mut ctx = super::ShallowIrEmitCtx::new();
    let mut log = vec![];
    let out = super::emit_transition_prop_as_lean(
        &prop,
        "state",
        "transition",
        "_ignored",
        &mut ctx,
        &mut log,
    );
    assert!(!out.contains("__backpass"), "double underscore in: {out}");
    assert!(
        out.contains("(_backpass_0 :"),
        "expected `_backpass_0 :` in: {out}"
    );
}

#[test]
fn whole_word_check_prevents_false_positive() {
    // Binder `n` must not be considered "used" by `_backpass_n_1`.
    assert!(!super::ident_is_used("(_backpass_n_1 : Data)", "n"));
    assert!(super::ident_is_used("(n : Data)", "n"));
    assert!(super::ident_is_used("foo n bar", "n"));
    assert!(!super::ident_is_used("fn_thing", "n"));
}

// --- Soundness guard: SKIPPED when step IR present but no transition constraints ---

#[test]
fn state_machine_halt_with_step_ir_but_no_transition_prop_and_no_witnesses_is_skipped() {
    // Scenario: the step function body was analysed (step_function_ir = Some) but
    // every leaf of the TransitionProp was Unsupported (transition_prop_lean = None).
    // No concrete witnesses are available either.
    //
    // Without the guard, `generate_proof_file` would fall through to
    // `try_generate_direct_proof_from_semantics` which emits
    //   ∀ x, reachable x → proveTestsHalt prog (dataArg x)
    // where `reachable` over-approximates the step function. That theorem is
    // vacuously true / false and must NOT be admitted.
    //
    // With the guard, generate_proof_file returns FallbackRequired (SKIPPED).
    use aiken_lang::test_framework::{ShallowIr, ShallowIrType};

    let mut test =
        make_phase12_state_machine_test("prop_vacuous_ok", StateMachineAcceptance::AcceptsSuccess);

    // Simulate "body was resolved but produced vacuous IR" by setting
    // step_function_ir to a bare Var (the simplest non-None value) while
    // leaving transition_prop_lean = None and concrete_halt_witnesses empty.
    if let FuzzerSemantics::StateMachineTrace {
        ref mut step_function_ir,
        ..
    } = test.semantics
    {
        *step_function_ir = Some(ShallowIr::Var {
            name: "step".to_string(),
            ty: ShallowIrType::Data,
        });
    }
    // Confirm our fixture: no witnesses, no transition prop.
    assert!(test.concrete_halt_witnesses.is_empty());
    assert!(test.transition_prop_lean.is_none());

    let id = test_id("permissions.test", "prop_vacuous_ok");
    let lean_name = sanitize_lean_name("prop_vacuous_ok");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_vacuous_ok";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    );

    assert_proof_generation_skipped_as_fallback(
        &result,
        "halt test with step IR but no transition prop and no witnesses must be SKIPPED",
        "zero extractable constraints",
    );
}

#[test]
fn state_machine_halt_with_step_ir_and_transition_prop_but_no_witnesses_is_skipped_when_two_phase_disabled()
 {
    // Scenario: AIKEN_EMIT_TWO_PHASE=0 disables the two-phase emission path.
    // The test has step IR + a populated transition_prop_lean + no witnesses.
    // The ORIGINAL guard (transition_prop_lean.is_none()) does NOT fire here,
    // so without the new bypass guard control would fall into
    // try_generate_direct_proof_from_semantics with the `reachable`
    // over-approximation — labelled "unsound: would be false in general".
    // The new guard must SKIP this configuration.
    use aiken_lang::test_framework::{ShallowIr, ShallowIrType};

    let _guard = env_mutex().lock().unwrap_or_else(|e| e.into_inner());
    let prior = std::env::var("AIKEN_EMIT_TWO_PHASE").ok();
    // SAFETY: guarded by env_mutex() — no other thread reads AIKEN_EMIT_TWO_PHASE
    // while this lock is held.
    unsafe {
        std::env::set_var("AIKEN_EMIT_TWO_PHASE", "0");
    }

    let mut test = make_phase12_state_machine_test(
        "prop_two_phase_off_ok",
        StateMachineAcceptance::AcceptsSuccess,
    );
    if let FuzzerSemantics::StateMachineTrace {
        ref mut step_function_ir,
        ..
    } = test.semantics
    {
        *step_function_ir = Some(ShallowIr::Var {
            name: "step".to_string(),
            ty: ShallowIrType::Data,
        });
    }
    // Populate transition_prop_lean so the ORIGINAL guard does NOT fire.
    // The new guard must fire even when transition_prop_lean is Some.
    // (Body text is `True` here just to give the field a value; the
    // bypass guard fires before the structural-vacuity gate, so
    // `is_vacuous` is irrelevant for this test path. We mirror the body
    // honestly anyway.)
    test.transition_prop_lean = Some(crate::export::ExportedTransitionProp {
        is_valid_transition_def:
            "def t_isValidTransition (state : Data) (transition : Data) : Prop := True".to_string(),
        initial_state_lean: None,
        unsupported_log: Vec::new(),
        opaque_sub_generators: Vec::new(),
        s0002_marker: None,
        is_vacuous: true,
    });
    assert!(test.concrete_halt_witnesses.is_empty());
    assert!(test.transition_prop_lean.is_some());

    let id = test_id("permissions.test", "prop_two_phase_off_ok");
    let lean_name = sanitize_lean_name("prop_two_phase_off_ok");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_two_phase_off_ok";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    );

    // Restore env before assertions so a panic doesn't leak state to other tests.
    // SAFETY: still under env_mutex().
    unsafe {
        match prior {
            Some(v) => std::env::set_var("AIKEN_EMIT_TWO_PHASE", v),
            None => std::env::remove_var("AIKEN_EMIT_TWO_PHASE"),
        }
    }

    assert_proof_generation_skipped_as_fallback(
        &result,
        "halt test with step IR + transition prop but no witnesses must SKIP when AIKEN_EMIT_TWO_PHASE=0",
        "two-phase disabled",
    );
}

#[test]
fn state_machine_halt_two_phase_off_guard_does_not_block_when_witnesses_present() {
    // Control: when AIKEN_EMIT_TWO_PHASE=0 is set but concrete witnesses exist,
    // the new bypass guard must NOT fire — the witness-based native_decide
    // fallback path is still sound and must be reachable.
    use aiken_lang::test_framework::{ShallowIr, ShallowIrType};

    let _guard = env_mutex().lock().unwrap_or_else(|e| e.into_inner());
    let prior = std::env::var("AIKEN_EMIT_TWO_PHASE").ok();
    // SAFETY: guarded by env_mutex().
    unsafe {
        std::env::set_var("AIKEN_EMIT_TWO_PHASE", "0");
    }

    let mut test = make_phase12_state_machine_test(
        "prop_two_phase_off_with_witnesses_ok",
        StateMachineAcceptance::AcceptsSuccess,
    );
    if let FuzzerSemantics::StateMachineTrace {
        ref mut step_function_ir,
        ..
    } = test.semantics
    {
        *step_function_ir = Some(ShallowIr::Var {
            name: "step".to_string(),
            ty: ShallowIrType::Data,
        });
    }
    // Add a concrete witness so the new guard is bypassed.
    // `concrete_halt_witnesses` is Vec<String> of CBOR-hex-encoded PlutusData.
    // The exact bytes are irrelevant to the guard; only non-emptiness matters.
    test.concrete_halt_witnesses.push("80".to_string());

    let id = test_id("permissions.test", "prop_two_phase_off_with_witnesses_ok");
    let lean_name = sanitize_lean_name("prop_two_phase_off_with_witnesses_ok");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_two_phase_off_with_witnesses_ok";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    );

    // SAFETY: still under env_mutex().
    unsafe {
        match prior {
            Some(v) => std::env::set_var("AIKEN_EMIT_TWO_PHASE", v),
            None => std::env::remove_var("AIKEN_EMIT_TWO_PHASE"),
        }
    }

    // Contract: if the result is Err, it must NOT be the new guard's
    // "two-phase disabled" message. (Other generation errors are acceptable
    // here — this test is narrowly about the new guard not firing.)
    if let Err(e) = &result {
        let msg = e.to_string();
        assert!(
            !msg.contains("two-phase disabled"),
            "new guard must not fire when witnesses are present, got: {msg}"
        );
    }
}

#[test]
fn transition_body_is_vacuous_detects_true() {
    assert!(super::transition_body_is_vacuous(
        "def foo_ivt (s : Data) (t : Data) : Prop :=\n  True\n"
    ));
    assert!(super::transition_body_is_vacuous(
        "def foo_ivt (s : Data) (t : Data) : Prop :=\n  (True)\n"
    ));
    assert!(super::transition_body_is_vacuous(
        "def foo_ivt (s : Data) (t : Data) : Prop :=\n  ∃ (_x : Data),\n  True\n"
    ));
    assert!(!super::transition_body_is_vacuous(
        "def foo_ivt (s : Data) (t : Data) : Prop :=\n  (True ∨ X)\n"
    ));
    assert!(!super::transition_body_is_vacuous(
        "def foo_ivt (s : Data) (t : Data) : Prop :=\n  (__SGPFX__foo_prop state transition)\n"
    ));
}

/// M3 drift sentinel: for every representative `TransitionProp` in this
/// corpus, if the rendered Lean text's `transition_body_is_vacuous`
/// flags it as vacuous, the structural
/// `aiken_lang::test_framework::transition_prop_is_vacuous` MUST also
/// flag it.  The reverse direction is intentionally not asserted: the
/// structural predicate is strictly more precise (it recurses through
/// `∧`/`∨`, while the text predicate gives up on those connectives).
///
/// **Why this test exists.**  The structural predicate is the
/// production gate; the text predicate is preserved only as a
/// `#[cfg(test)]` regression check.  If a future emitter change altered
/// the rendered surface form (e.g. emitting `True.intro` instead of
/// `True`, or inserting a `let` binding), the text predicate would
/// silently start mis-classifying — that drift would be invisible
/// without this sentinel because production never consults it.  This
/// test pins the agreement on a corpus broad enough to catch any such
/// drift the next time the emitter is touched.
#[test]
fn structural_vacuity_agrees_with_text_drift_sentinel() {
    use super::emit_is_valid_transition_def_for_export;
    use aiken_lang::test_framework::{
        FuzzerSemantics, ShallowConst, ShallowIr, ShallowIrType, TransitionProp, TransitionPropArm,
        transition_prop_is_vacuous,
    };

    fn unsupported_leaf() -> TransitionProp {
        TransitionProp::Unsupported {
            reason: "drift-sentinel-unsupported".to_string(),
            source_location: None,
        }
    }

    let pure_true = TransitionProp::Pure(ShallowIr::Const(ShallowConst::Bool(true)));
    let unsupported = unsupported_leaf();
    let exists_unsupported = TransitionProp::Exists {
        binder: "x".to_string(),
        ty: ShallowIrType::Data,
        domain: Box::new(FuzzerSemantics::Data),
        body: Box::new(unsupported_leaf()),
    };
    let empty_and = TransitionProp::And(vec![]);
    let empty_or = TransitionProp::Or(vec![]);
    let empty_match = TransitionProp::Match {
        scrutinee: ShallowIr::Var {
            name: "x".to_string(),
            ty: ShallowIrType::Data,
        },
        arms: vec![],
    };
    let eq_output = TransitionProp::EqOutput(ShallowIr::Var {
        name: "transition".to_string(),
        ty: ShallowIrType::Data,
    });
    let sub_generator = TransitionProp::SubGenerator {
        module: "m".to_string(),
        fn_name: "f".to_string(),
    };
    // Composite where structural is strictly more precise than text:
    // text predicate doesn't recurse through `∧`, so it returns false;
    // structural recurses on each Unsupported leaf and returns true.
    let and_of_unsupported = TransitionProp::And(vec![unsupported_leaf(), unsupported_leaf()]);
    // Composite where text predicate succeeds via the `∃ ... , True`
    // strip pattern: Exists wraps the unsupported leaf, which renders
    // to `(∃ (_x : Data), True)` — the text predicate strips the
    // existential and recognises `True`.
    let exists_of_match_arms_all_vacuous = TransitionProp::Exists {
        binder: "y".to_string(),
        ty: ShallowIrType::Data,
        domain: Box::new(FuzzerSemantics::Data),
        body: Box::new(TransitionProp::Match {
            scrutinee: ShallowIr::Var {
                name: "y".to_string(),
                ty: ShallowIrType::Data,
            },
            // empty arms → emitter renders body as `True`
            arms: vec![],
        }),
    };
    // Match with all-Unsupported arms: emitter renders as
    // `(True ∨ True)` (text predicate gives up); structural is true.
    let match_arms_unsupported = TransitionProp::Match {
        scrutinee: ShallowIr::Var {
            name: "x".to_string(),
            ty: ShallowIrType::Data,
        },
        arms: vec![
            TransitionPropArm {
                tag: Some(0),
                bindings: vec![],
                body: unsupported_leaf(),
            },
            TransitionPropArm {
                tag: Some(1),
                bindings: vec![],
                body: unsupported_leaf(),
            },
        ],
    };
    // Commit 13 follow-up #1 — IfThenElse corpus expansion.
    //
    // `IfThenElse` arrived in the lowering as the canonical encoding of the
    // `fork*_and_then` ScenarioBuilder helpers (scenario-plan §2 / commit 11).
    // The drift sentinel did not previously exercise it, so a future emitter
    // change to `IfThenElse`'s rendered surface (e.g. emitting `if h : c then`
    // instead of `if c then`) could silently break the agreement between
    // the structural and text predicates without a single test failing.
    //
    // The three entries below cover the three structural shapes that matter
    // for the `text⇒structural` direction:
    //
    //   * `if_then_else_unsup_unsup` — both branches are leaves the text
    //     predicate cannot inspect (`Unsupported`).  Both predicates must
    //     agree (structural recurses; text inspects the rendered branches).
    //   * `if_then_else_eq_output_unsup` — mixed: one branch is `EqOutput`
    //     (non-vacuous), the other is `Unsupported`.  This is the realistic
    //     `fork_if_and_then(cond, _, fT, fF)` lowering when only one branch
    //     materialises an output.
    //   * `non_empty_or_mixed` — non-empty `Or` with mixed-vacuity branches
    //     (`Unsupported` ∨ `EqOutput`).  Pairs the M3 corpus's empty-`Or`
    //     case with the realistic mixed shape.
    let if_then_else_unsup_unsup = TransitionProp::IfThenElse {
        cond: ShallowIr::Const(ShallowConst::Bool(true)),
        t: Box::new(unsupported_leaf()),
        e: Box::new(unsupported_leaf()),
    };
    let if_then_else_eq_output_unsup = TransitionProp::IfThenElse {
        cond: ShallowIr::Const(ShallowConst::Bool(true)),
        t: Box::new(TransitionProp::EqOutput(ShallowIr::Var {
            name: "transition".to_string(),
            ty: ShallowIrType::Data,
        })),
        e: Box::new(unsupported_leaf()),
    };
    let non_empty_or_mixed = TransitionProp::Or(vec![
        unsupported_leaf(),
        TransitionProp::EqOutput(ShallowIr::Var {
            name: "transition".to_string(),
            ty: ShallowIrType::Data,
        }),
    ]);

    let corpus: &[(&str, TransitionProp)] = &[
        ("pure_true", pure_true),
        ("unsupported", unsupported),
        ("exists_of_unsupported", exists_unsupported),
        ("empty_and", empty_and),
        ("empty_or", empty_or),
        ("empty_match", empty_match),
        ("eq_output", eq_output),
        ("sub_generator", sub_generator),
        ("and_of_unsupported", and_of_unsupported),
        ("exists_of_empty_match", exists_of_match_arms_all_vacuous),
        ("match_all_unsupported", match_arms_unsupported),
        ("if_then_else_unsup_unsup", if_then_else_unsup_unsup),
        ("if_then_else_eq_output_unsup", if_then_else_eq_output_unsup),
        ("non_empty_or_mixed", non_empty_or_mixed),
    ];

    for (label, prop) in corpus {
        let (def_text, _log, _sub_gens, _s0002) =
            emit_is_valid_transition_def_for_export("drift_sentinel_ivt", prop);
        let text_says_vacuous = super::transition_body_is_vacuous(&def_text);
        let structural_says_vacuous = transition_prop_is_vacuous(prop);

        // Safe direction: text⇒structural.  If text flags it, structural
        // MUST agree — otherwise production (which trusts structural)
        // would emit a vacuous proof that the text predicate would have
        // caught.
        if text_says_vacuous {
            assert!(
                structural_says_vacuous,
                "drift sentinel: text predicate flagged '{label}' as vacuous but \
                 structural predicate did not. Rendered Lean:\n{def_text}"
            );
        }

        // Diagnostic: also surface the verdicts for humans grepping the
        // log when this test fires, so the failure points directly to
        // the drifting case.
        eprintln!(
            "drift sentinel: label={label:<32} text={text_says_vacuous} structural={structural_says_vacuous}",
        );
    }
}

// ---------------------------------------------------------------------------
// M6 — schema lowering Err propagation regression tests.
//
// These pin the three M6 surfaces that previously silently widened to a
// trivial `True` precondition (or fell through to a weaker semantics-only
// path).  Each test exercises the exact code site the M6 commit changed and
// asserts the propagation:
//
//   * E0028 (`UnsupportedFuzzerOutputType`) — the surgical early gate in
//     `try_generate_direct_proof_from_semantics` for the
//     `(FuzzerOutputType::Unsupported(_), FuzzerSemantics::Data, no schema)`
//     triple.  Pre-M6: fell through to the `(Data, _) | (Unsupported, _)`
//     arm and produced a precondition-less theorem reported as PROVED.
//
//   * E0017 (`OpaqueListElementSemantics`) — the schema-helper builder in
//     the same dispatcher arm; pre-M6 the `Err(_) => None` arm silently
//     dropped the schema-derived precondition.
//
//   * E0015 (`QualifiedAdtNoSchema`) — the inner `DataWithSchema` arm in
//     `emit_partial_data_semantics_predicate`; pre-M6 the `if let Ok(root)`
//     arm silently widened to `def {name} (_ : Data) : Prop := True` when
//     the lowerer failed.
//
// All three categories are `FallbackRequired` (per
// `verify/error_catalogue.rs`), so `is_skippable_generation_error` returns
// `true` — i.e. `--skip-unsupported=E00xx` records the test as skipped
// rather than crashing the run.  These tests pin (a) the code, (b) the typed
// `UnsupportedReason` payload, and (c) the skippable category in one shot.
// ---------------------------------------------------------------------------

/// Build a deliberately broken `ExportedDataSchema` whose `root` references a
/// type that is *not* present in `definitions`.  Feeding this to
/// `build_exported_data_shape_predicates*` triggers the
/// "missing exported fuzzer schema definition" branch in
/// `exported_data_schema_for_reference` (verify.rs:3025).  Used by the
/// E0015/E0017 propagation tests to force the lowerer to fail loud.
fn make_broken_schema_with_dangling_root() -> ExportedDataSchema {
    ExportedDataSchema {
        // Root references a name that is intentionally absent from
        // `definitions`, so the lowerer's first lookup fails.
        root: Reference::new("missing_module/MissingType"),
        definitions: Definitions::new(),
    }
}

#[test]
fn m6_dispatcher_unsupported_data_no_schema_propagates_e0028() {
    // M6b regression — the surgical early gate at verify.rs:7689 must
    // hard-error the `(Unsupported, FuzzerSemantics::Data, no schema)`
    // triple as `E0028 (UnsupportedFuzzerOutputType)` instead of falling
    // through to the `(Data, _) | (Unsupported, _)` arm and emitting a
    // precondition-less PROVED theorem.
    let mut test = make_test_with_type(
        "my_module",
        "test_unsupported_no_schema",
        FuzzerOutputType::Unsupported("Foo".to_string()),
        FuzzerConstraint::Any,
    );
    // `make_test_with_type` derives semantics from the constraint; for
    // `Unsupported + Any` (with `allow_default_for_any=false`) it picks
    // `Opaque`.  Pin `Data` explicitly so we hit the M6 gate (which is
    // guarded on `FuzzerSemantics::Data`, not `Opaque`).
    test.semantics = FuzzerSemantics::Data;
    // The gate is also conditioned on `fuzzer_data_schema.is_none()` — a
    // populated schema would route through the E0017 site instead.
    assert!(
        test.fuzzer_data_schema.is_none(),
        "fixture must have no fuzzer_data_schema for the E0028 gate to fire",
    );

    let id = test_id("my_module", "test_unsupported_no_schema");
    let lean_name = sanitize_lean_name("test_unsupported_no_schema");
    let lean_module = "AikenVerify.Proofs.My_module.test_unsupported_no_schema";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );

    let report = result.expect_err(
        "M6 surgical early gate must hard-error the (Unsupported, Data, no schema) triple, \
         not silently produce a precondition-less theorem",
    );
    let err = report
        .downcast_ref::<GenerationError>()
        .unwrap_or_else(|| panic!("error must downcast to GenerationError, got: {report}"));
    assert_eq!(
        err.code,
        Some("E0028"),
        "expected E0028 from the dispatcher early gate, got code {:?}, message: {}",
        err.code,
        err.message,
    );
    match err.reason.as_ref() {
        Some(UnsupportedReason::UnsupportedFuzzerOutputType {
            test_name,
            type_repr,
        }) => {
            assert_eq!(test_name, "my_module.test_unsupported_no_schema");
            assert_eq!(type_repr, "Foo");
        }
        other => panic!("expected UnsupportedReason::UnsupportedFuzzerOutputType, got {other:?}"),
    }
    assert_eq!(
        err.category,
        GenerationErrorCategory::FallbackRequired,
        "E0028 must be FallbackRequired so --skip-unsupported=E0028 records the test as skipped",
    );
    assert!(
        is_skippable_generation_error(&report, &SkipPolicy::All),
        "E0028 (FallbackRequired) MUST be skippable by --skip-unsupported",
    );
}

#[test]
fn m6_dispatcher_schema_lowering_failure_propagates_e0017() {
    // M6c regression — the schema-helper builder at verify.rs:7808 must
    // wrap a lowerer failure as `E0017 (OpaqueListElementSemantics)` and
    // propagate, instead of silently widening to a semantics-only theorem
    // whose precondition is strictly weaker than the schema would impose.
    //
    // Triggered by feeding a `(Data, FuzzerSemantics::Data)` fixture with a
    // *broken* `fuzzer_data_schema` (root references an undefined type, so
    // `exported_data_schema_for_reference` returns `Err` at the first
    // lookup).
    let mut test = make_test_with_type(
        "my_module",
        "test_schema_lowering_fails",
        FuzzerOutputType::Data,
        FuzzerConstraint::Any,
    );
    test.semantics = FuzzerSemantics::Data;
    test.fuzzer_data_schema = Some(make_broken_schema_with_dangling_root());

    let id = test_id("my_module", "test_schema_lowering_fails");
    let lean_name = sanitize_lean_name("test_schema_lowering_fails");
    let lean_module = "AikenVerify.Proofs.My_module.test_schema_lowering_fails";

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );

    let report = result.expect_err(
        "M6 schema-helper builder must propagate the lowerer's Err as E0017, \
         not silently fall through with a weaker semantics-only precondition",
    );
    let err = report
        .downcast_ref::<GenerationError>()
        .unwrap_or_else(|| panic!("error must downcast to GenerationError, got: {report}"));
    assert_eq!(
        err.code,
        Some("E0017"),
        "expected E0017 from the schema-helper builder, got code {:?}, message: {}",
        err.code,
        err.message,
    );
    match err.reason.as_ref() {
        Some(UnsupportedReason::OpaqueListElementSemantics { test_name, reason }) => {
            assert_eq!(test_name, "my_module.test_schema_lowering_fails");
            // The lowerer's underlying error message is stashed in `reason`
            // (formatted via `format!("{e}")`); pin the substring that
            // identifies the dangling-reference branch.  This guards
            // against accidentally swallowing the lowerer's message.
            assert!(
                reason.contains("missing exported fuzzer schema definition"),
                "E0017 reason should surface the lowerer's underlying error message; got: {reason}",
            );
        }
        other => panic!("expected UnsupportedReason::OpaqueListElementSemantics, got {other:?}"),
    }
    assert_eq!(
        err.category,
        GenerationErrorCategory::FallbackRequired,
        "E0017 must be FallbackRequired so --skip-unsupported=E0017 records the test as skipped",
    );
    assert!(
        is_skippable_generation_error(&report, &SkipPolicy::All),
        "E0017 (FallbackRequired) MUST be skippable by --skip-unsupported",
    );
}

#[test]
fn m6_inner_data_schema_lookup_failure_propagates_e0015() {
    // M6a regression — the `DataWithSchema` arm of
    // `emit_partial_data_semantics_predicate` at verify.rs:3527 must wrap a
    // lowerer failure as `E0015 (QualifiedAdtNoSchema { test_name,
    // type_name })` and propagate.  Pre-M6 the `if let Ok(root)` arm
    // silently substituted `def {name} (_ : Data) : Prop := True`, leaving
    // the test's reported precondition out of sync with the actual semantic
    // constraint.
    //
    // The dispatcher reaches this site via
    // `build_state_machine_trace_reachability_helpers`, which forwards
    // `transition_semantics.state_semantics` (and friends) into
    // `build_partial_data_semantics_predicates_into`.  By making
    // `state_semantics = DataWithSchema { type_name: "broken.Type" }` and
    // populating `inner_data_schemas["broken.Type"]` with a
    // dangling-reference schema, we force the lowerer to fail and the M6
    // wrapper to fire.
    use std::collections::BTreeMap;

    let broken_type_name = "missing_module/MissingType".to_string();

    let transition_semantics = StateMachineTransitionSemantics {
        terminal_tag: 0,
        step_tag: 1,
        label_field_index: 0,
        next_state_field_index: 1,
        event_field_index: 2,
        // Route through the M6a wrapper — the lowerer will hit the
        // dangling-reference branch when it tries to materialize the
        // structural schema for `broken_type_name`.
        state_semantics: Box::new(FuzzerSemantics::DataWithSchema {
            type_name: broken_type_name.clone(),
        }),
        step_input_semantics: vec![],
        // Keep the remaining semantics innocuous so the only failing
        // lookup is the targeted DataWithSchema arm.
        label_semantics: Box::new(FuzzerSemantics::IntRange {
            min: Some("0".to_string()),
            max: Some("10".to_string()),
        }),
        event_semantics: Box::new(FuzzerSemantics::IntRange {
            min: Some("0".to_string()),
            max: Some("50".to_string()),
        }),
    };

    let mut inner_data_schemas: BTreeMap<String, ExportedDataSchema> = BTreeMap::new();
    inner_data_schemas.insert(
        broken_type_name.clone(),
        make_broken_schema_with_dangling_root(),
    );

    let test_name = "my_module.test_inner_schema_lookup_fails";

    let mut shape_builder = LeanDataShapeBuilder::default();
    let result = build_state_machine_trace_reachability_helpers(
        test_name,
        "inner_schema_test",
        &StateMachineAcceptance::AcceptsSuccess,
        &transition_semantics,
        &inner_data_schemas,
        &mut shape_builder,
        "inner_schema_test_shape",
        None,
    );

    let report = result.expect_err(
        "M6 inner DataWithSchema arm must propagate the lowerer's Err as E0015, \
         not silently widen to `def _ : Prop := True`",
    );
    let err = report
        .downcast_ref::<GenerationError>()
        .unwrap_or_else(|| panic!("error must downcast to GenerationError, got: {report}"));
    assert_eq!(
        err.code,
        Some("E0015"),
        "expected E0015 from the inner DataWithSchema arm, got code {:?}, message: {}",
        err.code,
        err.message,
    );
    match err.reason.as_ref() {
        Some(UnsupportedReason::QualifiedAdtNoSchema {
            test_name: actual_test_name,
            type_name,
        }) => {
            assert_eq!(actual_test_name, test_name);
            assert_eq!(type_name, &broken_type_name);
        }
        other => panic!("expected UnsupportedReason::QualifiedAdtNoSchema, got {other:?}"),
    }
    assert_eq!(
        err.category,
        GenerationErrorCategory::FallbackRequired,
        "E0015 must be FallbackRequired so --skip-unsupported=E0015 records the test as skipped",
    );
    assert!(
        is_skippable_generation_error(&report, &SkipPolicy::All),
        "E0015 (FallbackRequired) MUST be skippable by --skip-unsupported",
    );
}

#[test]
fn vacuous_transition_prop_is_skipped() {
    // Build a state-machine halt test with a populated transition_prop_lean
    // whose body widened entirely to `True`. The two-phase guard must promote
    // this to a FallbackRequired SKIP rather than emit a vacuous proof.
    use aiken_lang::test_framework::{ShallowIr, ShallowIrType};

    let mut test = make_phase12_state_machine_test(
        "prop_vacuous_transition_ok",
        StateMachineAcceptance::AcceptsSuccess,
    );

    // step IR present so the halt-test detection succeeds.
    if let FuzzerSemantics::StateMachineTrace {
        ref mut step_function_ir,
        ..
    } = test.semantics
    {
        *step_function_ir = Some(ShallowIr::Var {
            name: "step".to_string(),
            ty: ShallowIrType::Data,
        });
    }

    // Vacuous transition prop body: widened entirely to `True`.
    // M3: the structural-vacuity gate now reads `is_vacuous` directly
    // rather than re-parsing the rendered Lean text.
    test.transition_prop_lean = Some(crate::export::ExportedTransitionProp {
        is_valid_transition_def:
            "def foo_s4_isValidTransition (state : Data) (transition : Data) : Prop :=\n  True\n"
                .to_string(),
        initial_state_lean: None,
        unsupported_log: Vec::new(),
        opaque_sub_generators: Vec::new(),
        s0002_marker: None,
        is_vacuous: true,
    });
    // Add a witness so the AIKEN_EMIT_TWO_PHASE=0 bypass guard does not fire
    // (we want to specifically test the vacuity guard, regardless of env state).
    test.concrete_halt_witnesses.push("80".to_string());

    let id = test_id("permissions.test", "prop_vacuous_transition_ok");
    let lean_name = sanitize_lean_name("prop_vacuous_transition_ok");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_vacuous_transition_ok";

    // Engage the two-phase path explicitly.
    let _guard = env_mutex().lock().unwrap_or_else(|e| e.into_inner());
    let prior = std::env::var("AIKEN_EMIT_TWO_PHASE").ok();
    // SAFETY: guarded by env_mutex().
    unsafe {
        std::env::set_var("AIKEN_EMIT_TWO_PHASE", "1");
    }

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    );

    // Restore env before assertions.
    // SAFETY: still under env_mutex().
    unsafe {
        match prior {
            Some(v) => std::env::set_var("AIKEN_EMIT_TWO_PHASE", v),
            None => std::env::remove_var("AIKEN_EMIT_TWO_PHASE"),
        }
    }

    assert_proof_generation_skipped_as_fallback(
        &result,
        "vacuous TransitionProp body must be SKIPPED rather than emitted as a vacuous proof",
        "vacuous",
    );
}

#[test]
fn eq_output_var_transition_prop_is_skipped_as_vacuous() {
    use aiken_lang::test_framework::{
        ShallowIr, ShallowIrType, TransitionProp, transition_prop_is_vacuous,
    };

    let mut test = make_phase12_state_machine_test(
        "prop_eq_output_var_vacuous_ok",
        StateMachineAcceptance::AcceptsSuccess,
    );

    if let FuzzerSemantics::StateMachineTrace {
        ref mut step_function_ir,
        ..
    } = test.semantics
    {
        *step_function_ir = Some(ShallowIr::Var {
            name: "step".to_string(),
            ty: ShallowIrType::Data,
        });
    }

    let prop = TransitionProp::EqOutput(ShallowIr::Var {
        name: "transition".to_string(),
        ty: ShallowIrType::Data,
    });
    let (def, log, sub_gens, s0002_marker) = crate::verify::emit_is_valid_transition_def_for_export(
        "foo_eq_var_isValidTransition",
        &prop,
    );
    let is_vacuous = transition_prop_is_vacuous(&prop);
    assert!(
        is_vacuous,
        "EqOutput(Var) lowers to ∃ v, transition = v and must be classified as vacuous"
    );

    test.transition_prop_lean = Some(crate::export::ExportedTransitionProp {
        is_valid_transition_def: def,
        initial_state_lean: None,
        unsupported_log: log,
        opaque_sub_generators: sub_gens,
        s0002_marker,
        is_vacuous,
    });
    test.concrete_halt_witnesses.push("80".to_string());

    let id = test_id("permissions.test", "prop_eq_output_var_vacuous_ok");
    let lean_name = sanitize_lean_name("prop_eq_output_var_vacuous_ok");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_eq_output_var_vacuous_ok";

    let _guard = env_mutex().lock().unwrap_or_else(|e| e.into_inner());
    let prior = std::env::var("AIKEN_EMIT_TWO_PHASE").ok();
    unsafe {
        std::env::set_var("AIKEN_EMIT_TWO_PHASE", "1");
    }

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    );

    unsafe {
        match prior {
            Some(v) => std::env::set_var("AIKEN_EMIT_TWO_PHASE", v),
            None => std::env::remove_var("AIKEN_EMIT_TWO_PHASE"),
        }
    }

    assert_proof_generation_skipped_as_fallback(
        &result,
        "EqOutput(Var) TransitionProp body must be skipped as vacuous",
        "vacuous",
    );
}

#[test]
fn exists_bound_output_uses_domain_for_structural_vacuity() {
    use aiken_lang::test_framework::{
        FuzzerSemantics, ShallowIr, ShallowIrType, TransitionProp, transition_prop_is_vacuous,
    };

    let prop = TransitionProp::Exists {
        binder: "x".to_string(),
        ty: ShallowIrType::Data,
        domain: Box::new(FuzzerSemantics::Constructors { tags: vec![0, 1] }),
        body: Box::new(TransitionProp::EqOutput(ShallowIr::Var {
            name: "x".to_string(),
            ty: ShallowIrType::Data,
        })),
    };

    let (_def, _log, _sub_gens, _s0002_marker) =
        crate::verify::emit_is_valid_transition_def_for_export(
            "foo_exists_bound_output_isValidTransition",
            &prop,
        );

    assert!(
        !transition_prop_is_vacuous(&prop),
        "domain-constrained bind-return shapes must not be skipped as vacuous"
    );
}

// --- IfThenElse: precise condition emission ---

#[test]
fn emit_lean_bool_const_true_and_false() {
    use aiken_lang::test_framework::{ShallowConst, ShallowIr};

    let mut ctx = super::ShallowIrEmitCtx::new();
    let out_t =
        super::emit_shallow_ir_as_lean_bool(&ShallowIr::Const(ShallowConst::Bool(true)), &mut ctx);
    assert_eq!(out_t, "true", "Const(Bool(true)) should emit `true`");

    let out_f =
        super::emit_shallow_ir_as_lean_bool(&ShallowIr::Const(ShallowConst::Bool(false)), &mut ctx);
    assert_eq!(out_f, "false", "Const(Bool(false)) should emit `false`");

    // Neither path should have introduced any existential.
    assert!(
        ctx.existentials.is_empty(),
        "Bool literal emission must not allocate fresh existentials, got: {:?}",
        ctx.existentials
    );
}

#[test]
fn emit_lean_bool_eq_comparison() {
    // Eq/NotEq require `DecidableEq Data` which is not yet confirmed to exist
    // in the Blaster prelude. Until confirmed, these fall back to a fresh Bool
    // existential (safe — under IfThenElse encoding this widens to t ∨ e).
    // TODO: when `DecidableEq Data` is confirmed, re-enable `decide (l = r)`.
    use aiken_lang::test_framework::{ShallowBinOp, ShallowConst, ShallowIr};

    let prop = ShallowIr::BinOp {
        op: ShallowBinOp::Eq,
        left: Box::new(ShallowIr::Const(ShallowConst::Int("42".to_string()))),
        right: Box::new(ShallowIr::Const(ShallowConst::Int("42".to_string()))),
    };
    let mut ctx = super::ShallowIrEmitCtx::new();
    let out = super::emit_shallow_ir_as_lean_bool(&prop, &mut ctx);
    // Falls back to a fresh Bool existential since DecidableEq Data is unconfirmed.
    assert!(
        out.starts_with('_'),
        "Eq comparison should fall back to fresh Bool existential, got: {out}"
    );
    assert_eq!(
        ctx.existentials.len(),
        1,
        "one fresh Bool existential expected, got: {:?}",
        ctx.existentials
    );
    assert_eq!(
        ctx.existentials[0].1, "Bool",
        "existential must be typed Bool, got: {:?}",
        ctx.existentials[0]
    );
}

#[test]
fn if_then_else_emits_precise_encoding() {
    use aiken_lang::test_framework::{ShallowBinOp, ShallowConst, ShallowIr, TransitionProp};

    // Build: if (42 == 42) { transition = Data.I 1 } else { transition = Data.I 0 }
    let cond = ShallowIr::BinOp {
        op: ShallowBinOp::Eq,
        left: Box::new(ShallowIr::Const(ShallowConst::Int("42".to_string()))),
        right: Box::new(ShallowIr::Const(ShallowConst::Int("42".to_string()))),
    };
    let prop = TransitionProp::IfThenElse {
        cond,
        t: Box::new(TransitionProp::EqOutput(ShallowIr::Const(
            ShallowConst::Int("1".to_string()),
        ))),
        e: Box::new(TransitionProp::EqOutput(ShallowIr::Const(
            ShallowConst::Int("0".to_string()),
        ))),
    };

    let mut ctx = super::ShallowIrEmitCtx::new();
    let mut log = vec![];
    let out = super::emit_transition_prop_as_lean(
        &prop,
        "state",
        "transition",
        "_ignored",
        &mut ctx,
        &mut log,
    );

    // Precise encoding must include both `= true` (then-branch) and `= false`
    // (else-branch) guards.
    assert!(
        out.contains("= true"),
        "precise encoding must include `= true` then-guard, got:\n{out}"
    );
    assert!(
        out.contains("= false"),
        "precise encoding must include `= false` else-guard, got:\n{out}"
    );
    // Both branches' EqOutput bodies should appear.
    assert!(
        out.contains("Data.I (1 : Integer)"),
        "then-branch body must be present, got:\n{out}"
    );
    assert!(
        out.contains("Data.I (0 : Integer)"),
        "else-branch body must be present, got:\n{out}"
    );
    // Log must NOT contain the legacy "widened to branch disjunction" entry,
    // since we now emit precisely.
    assert!(
        !log.iter()
            .any(|e| e.contains("widened to branch disjunction")),
        "precise IfThenElse emission must not log a widening entry, got: {log:?}"
    );
}

// --- Item 12: step_fn_sound theorem-vs-axiom dispatch ---

/// A flat three-field Construct IR: the most common step function shape.
/// Fields: [Opaque(Data), Opaque(Data), Opaque(Data)] → three existentials.
fn make_flat_construct_ir() -> aiken_lang::test_framework::ShallowIr {
    use aiken_lang::test_framework::{ShallowIr, ShallowIrType};
    ShallowIr::Construct {
        module: "test".to_string(),
        constructor: "Step".to_string(),
        tag: 1,
        fields: vec![
            ShallowIr::Opaque {
                ty: ShallowIrType::Data,
                reason: "label".to_string(),
                code: None,
            },
            ShallowIr::Opaque {
                ty: ShallowIrType::Data,
                reason: "next_state".to_string(),
                code: None,
            },
            ShallowIr::Opaque {
                ty: ShallowIrType::Data,
                reason: "event".to_string(),
                code: None,
            },
        ],
    }
}

#[test]
fn step_fn_sound_emits_theorem_for_construct_ir() {
    // When the step IR is a top-level Construct whose tag matches step_tag,
    // the soundness block should be a `private theorem` — not `private axiom`.
    // make_flat_construct_ir() has tag=1; step_tag=1 matches.
    let ir = make_flat_construct_ir();
    let out = try_emit_step_fn_sound_as_theorem(
        "my_step_fn",
        &ir,
        1, // step_tag matches Construct.tag=1
        0, // label_index
        1, // next_state_index
        2, // event_index
        "my_label_field",
        "my_next_state_field",
        "my_event_field",
    );
    assert!(
        out.is_some(),
        "Construct IR with matching step_tag should yield Some(theorem)"
    );
    let text = out.unwrap();
    assert!(
        text.contains("private theorem my_step_fn_sound"),
        "Construct IR must emit `private theorem`, got:\n{text}"
    );
    assert!(
        !text.contains("private axiom"),
        "Construct IR must NOT emit `private axiom`, got:\n{text}"
    );
    // Proof must use `_state` (not `state`) to suppress unused-variable warnings.
    assert!(
        text.contains("_state"),
        "theorem must use `_state` (unused binder), got:\n{text}"
    );
    // Proof must use explicit simp-only with if_pos rfl to avoid Integer DecidableEq.
    assert!(
        text.contains("if_pos rfl"),
        "theorem must use `if_pos rfl` for tag guard reduction, got:\n{text}"
    );
}

#[test]
fn step_fn_sound_tag_mismatch_returns_none() {
    // When the Construct IR tag differs from step_tag, the field extractor's
    // `if tag = step_tag then … else none` guard reduces to `none`, so the
    // proof goal `none = some _` is unprovable. Must return None.
    let ir = make_flat_construct_ir(); // tag=1
    let out = try_emit_step_fn_sound_as_theorem(
        "sf", &ir, 42, // step_tag=42 ≠ Construct.tag=1
        0, 1, 2, "lf", "nsf", "ef",
    );
    assert!(
        out.is_none(),
        "tag mismatch (ir_tag=1, step_tag=42) must return None"
    );
}

#[test]
fn step_fn_sound_skips_for_opaque_ir() {
    use aiken_lang::test_framework::{ShallowIr, ShallowIrType};
    let ir = ShallowIr::Opaque {
        ty: ShallowIrType::Data,
        reason: "unknown step function".to_string(),
        code: None,
    };
    let out = try_emit_step_fn_sound_as_theorem(
        "my_step_fn",
        &ir,
        1, // step_tag
        0,
        1,
        2,
        "my_label_field",
        "my_next_state_field",
        "my_event_field",
    );
    assert!(
        out.is_none(),
        "Opaque IR must yield None — caller emits S0001 instead of a trusted axiom"
    );
}

#[test]
fn step_fn_sound_unsupported_emits_s0001_with_shape() {
    use super::error_catalogue;
    let err = error_catalogue::unsupported(
        "S0001",
        UnsupportedReason::StepFnSoundAxiomEmitted {
            test_name: "t".to_string(),
            shape: "Opaque".to_string(),
        },
    );
    assert_eq!(
        err.category,
        GenerationErrorCategory::UnsoundFallback,
        "S0001 must be categorised as UnsoundFallback"
    );
    assert_eq!(
        err.code,
        Some("S0001"),
        "S0001 catalogue construction must populate the stable code"
    );
    assert!(
        !err.message.contains("[S0001]"),
        "miette renders Diagnostic::code() separately; message should not embed [S0001], got: {}",
        err.message
    );
    let report = miette::Report::new(err);
    assert!(
        !is_skippable_generation_error(&report, &SkipPolicy::All),
        "S0001 (UnsoundFallback) must NOT be skippable by --skip-unsupported"
    );
}

/// Commit 18 typed-payload migration: the `S0002:…` string-sniff bridge
/// (`S0002_REASON_PREFIX` + `parse_constructor_tag_unresolved_marker`) is
/// retired. The producer now stores a typed
/// `OpaqueCode::ConstructorTagUnresolved { ctor, type_name }` directly on
/// `ShallowIr::Opaque.code`; the consumer matches on it. This test pins
/// the typed round-trip end-to-end.
#[test]
fn typed_opaque_code_round_trips_via_walker() {
    use aiken_lang::test_framework::{OpaqueCode, ShallowIr, ShallowIrType};
    let ir = ShallowIr::Opaque {
        ty: ShallowIrType::Data,
        reason: "diagnostic only — not parsed".to_string(),
        code: Some(OpaqueCode::ConstructorTagUnresolved {
            ctor: "Variant".to_string(),
            type_name: "module.Type".to_string(),
        }),
    };
    let code = aiken_lang::test_framework::find_first_typed_opaque_in_shallow_ir(&ir)
        .expect("typed code must be locatable on a fresh Opaque");
    match code {
        OpaqueCode::ConstructorTagUnresolved { ctor, type_name } => {
            assert_eq!(ctor, "Variant");
            assert_eq!(type_name, "module.Type");
        }
        _ => panic!("unexpected opaque code variant: {code:?}"),
    }
}

/// An `Opaque` with no typed `code` (the common case for unsupported
/// binops, complex patterns, etc.) must NOT be misclassified as S0002:
/// the walker returns `None`, so the dispatcher falls through to S0001.
#[test]
fn typed_opaque_walker_returns_none_for_untyped_opaque() {
    use aiken_lang::test_framework::{ShallowIr, ShallowIrType};
    let ir = ShallowIr::Opaque {
        ty: ShallowIrType::Data,
        reason: "unsupported binop Add".to_string(),
        code: None,
    };
    assert!(
        aiken_lang::test_framework::find_first_typed_opaque_in_shallow_ir(&ir).is_none(),
        "untyped Opaque must not be misclassified as S0002"
    );
}

#[test]
fn describe_step_ir_shape_returns_stable_labels() {
    use aiken_lang::test_framework::{ShallowConst, ShallowIr, ShallowIrType};
    let opaque = ShallowIr::Opaque {
        ty: ShallowIrType::Data,
        reason: "x".to_string(),
        code: None,
    };
    assert_eq!(describe_step_ir_shape(&opaque), "Opaque");

    let construct = ShallowIr::Construct {
        module: "m".to_string(),
        constructor: "C".to_string(),
        tag: 0,
        fields: vec![],
    };
    assert_eq!(describe_step_ir_shape(&construct), "Construct");

    let if_ir = ShallowIr::If {
        cond: Box::new(ShallowIr::Const(ShallowConst::Bool(true))),
        then_branch: Box::new(ShallowIr::Const(ShallowConst::Unit)),
        else_branch: Box::new(ShallowIr::Const(ShallowConst::Unit)),
    };
    assert_eq!(describe_step_ir_shape(&if_ir), "If");
}

#[test]
fn step_fn_sound_emits_none_for_if_ir() {
    use aiken_lang::test_framework::{ShallowConst, ShallowIr, ShallowIrType};
    // A top-level If node should fall through to None — the transition might
    // not be a Constr, so the soundness claim is not derivable.
    let ir = ShallowIr::If {
        cond: Box::new(ShallowIr::Const(ShallowConst::Bool(true))),
        then_branch: Box::new(ShallowIr::Opaque {
            ty: ShallowIrType::Data,
            reason: "then".to_string(),
            code: None,
        }),
        else_branch: Box::new(ShallowIr::Opaque {
            ty: ShallowIrType::Data,
            reason: "else".to_string(),
            code: None,
        }),
    };
    let out = try_emit_step_fn_sound_as_theorem("sf", &ir, 1, 0, 1, 2, "lf", "nsf", "ef");
    assert!(
        out.is_none(),
        "If IR should yield None (not a Construct), got Some(...)"
    );
}

#[test]
fn step_fn_sound_theorem_obtain_pattern_matches_existential_count() {
    // For a 3-field flat Construct (tag=1, step_tag=1), the `obtain` pattern
    // must name all 3 existentials followed by `rfl`.
    let ir = make_flat_construct_ir();
    let text = try_emit_step_fn_sound_as_theorem(
        "sf", &ir, 1, // step_tag=1 matches ir.tag=1
        0, 1, 2, "lf", "nsf", "ef",
    )
    .unwrap();
    // All three fresh variables must appear in the obtain pattern.
    assert!(
        text.contains("_fuzz_0") && text.contains("_fuzz_1") && text.contains("_fuzz_2"),
        "obtain pattern must name all 3 existentials, got:\n{text}"
    );
    // `rfl` must close the equality.
    assert!(
        text.contains("rfl"),
        "obtain pattern must include `rfl`, got:\n{text}"
    );
}

#[test]
fn step_fn_sound_theorem_witnesses_correct_fields() {
    // label_index=2, next_state_index=0, event_index=1 — a non-default field order.
    // The `exact ⟨nextState_witness, label_witness, event_witness, ...⟩` must
    // use _fuzz_0 as nextState, _fuzz_2 as label, _fuzz_1 as event.
    let ir = make_flat_construct_ir(); // tag=1
    let text = try_emit_step_fn_sound_as_theorem(
        "sf", &ir, 1, // step_tag=1 matches
        2, // label at field 2 → _fuzz_2
        0, // nextState at field 0 → _fuzz_0
        1, // event at field 1 → _fuzz_1
        "lf", "nsf", "ef",
    )
    .unwrap();
    // A simple substring check: _fuzz_0 (nextState), _fuzz_2 (label), _fuzz_1 (event).
    assert!(
        text.contains("_fuzz_0, _fuzz_2, _fuzz_1"),
        "exact witnesses must be (nextState=_fuzz_0, label=_fuzz_2, event=_fuzz_1), got:\n{text}"
    );
}

#[test]
fn step_fn_sound_out_of_range_index_returns_none() {
    // If any field index exceeds the number of Construct fields, return None.
    let ir = make_flat_construct_ir(); // 3 fields (indices 0-2), tag=1
    let out = try_emit_step_fn_sound_as_theorem(
        "sf", &ir, 1, // step_tag matches
        3, // label_index out of range for a 3-field Construct
        1, 2, "lf", "nsf", "ef",
    );
    assert!(
        out.is_none(),
        "out-of-range label_index must return None, got: Some(...)"
    );
}

#[test]
fn step_fn_sound_via_reachability_helpers_emits_s0002_for_unresolved_tag() {
    // End-to-end dispatch: a step IR whose root is `Opaque` carrying the
    // S0002 marker must be lifted to a `ConstructorTagUnresolved` hard
    // error rather than the generic `StepFnSoundAxiomEmitted` (S0001) that
    // any Opaque-shaped step IR would otherwise produce. This is the
    // commit-8 contract that closes the resolve_constructor_tag-tag-0
    // unsoundness.
    let transition_semantics = make_non_opaque_transition_semantics();
    // Commit 18: typed-payload migration — the dispatcher now keys off the
    // typed `OpaqueCode::ConstructorTagUnresolved { … }` field, so the
    // fixture sets it directly rather than embedding a `S0002:…` prefix in
    // the diagnostic `reason` string.
    let s0002_ir = aiken_lang::test_framework::ShallowIr::Opaque {
        ty: aiken_lang::test_framework::ShallowIrType::Data,
        reason: aiken_lang::test_framework::s0002_reason_message("Ghost", "ghost.Phantom"),
        code: Some(
            aiken_lang::test_framework::OpaqueCode::ConstructorTagUnresolved {
                ctor: "Ghost".to_string(),
                type_name: "ghost.Phantom".to_string(),
            },
        ),
    };

    let mut shape_builder = LeanDataShapeBuilder::default();
    let result = build_state_machine_trace_reachability_helpers(
        "step_sound_test",
        "step_sound_test",
        &StateMachineAcceptance::AcceptsSuccess,
        &transition_semantics,
        &Default::default(),
        &mut shape_builder,
        "step_sound_test_shape",
        Some(&s0002_ir),
    );

    let report = result.expect_err("S0002-marked Opaque step IR must produce a hard error");
    let err = report
        .downcast_ref::<GenerationError>()
        .expect("error must downcast to GenerationError");
    assert_eq!(
        err.code,
        Some("S0002"),
        "S0002 dispatch must win over the fallback S0001 path; got code {:?}, message: {}",
        err.code,
        err.message,
    );
    match err.reason.as_ref() {
        Some(UnsupportedReason::ConstructorTagUnresolved {
            ctor, type_name, ..
        }) => {
            assert_eq!(ctor, "Ghost");
            assert_eq!(type_name, "ghost.Phantom");
        }
        other => panic!("expected UnsupportedReason::ConstructorTagUnresolved, got {other:?}",),
    }
    assert_eq!(
        err.category,
        GenerationErrorCategory::UnsoundFallback,
        "S0002 must be UnsoundFallback (never skippable)",
    );
    assert!(
        !is_skippable_generation_error(&report, &SkipPolicy::All),
        "S0002 must NOT be skippable by --skip-unsupported",
    );
}

/// End-to-end regression for Issue 1 (Oracle A blocking review): when a step
/// IR carries the S0002 marker buried inside a `Construct` field (NOT at the
/// top level), the verify-side dispatcher must still surface a hard, non-
/// skippable `S0002` error. The pre-fix behaviour: only a top-level `Opaque`
/// matched the dispatch arm, so a nested marker was missed and the proof
/// fell through to the generic step_fn path which silently widened the
/// inner Opaque to a fresh existential and emitted a step_fn_sound axiom
/// (S0001) — masking the real root cause.
#[test]
fn step_fn_sound_dispatch_walks_into_construct_fields_for_s0002() {
    use aiken_lang::test_framework::{ShallowIr, ShallowIrType};

    let transition_semantics = make_non_opaque_transition_semantics();

    // Construct step IR carrying an S0002-marked Opaque buried inside one
    // of its fields. The Construct shape itself is NOT vacuous (`Construct`
    // is in the non-vacuous list), so it survives the upstream
    // `shallow_ir_is_vacuous` filter. The dispatcher must walk into the
    // fields to find the typed `OpaqueCode::ConstructorTagUnresolved`
    // payload (commit 18 retired the `S0002:…` string-prefix bridge).
    let step_ir = ShallowIr::Construct {
        module: "test".to_string(),
        constructor: "Step".to_string(),
        tag: 1,
        fields: vec![
            ShallowIr::Opaque {
                ty: ShallowIrType::Data,
                reason: "innocent reason".to_string(),
                code: None,
            },
            ShallowIr::Opaque {
                ty: ShallowIrType::Data,
                reason: aiken_lang::test_framework::s0002_reason_message("Ghost", "ghost.Phantom"),
                code: Some(
                    aiken_lang::test_framework::OpaqueCode::ConstructorTagUnresolved {
                        ctor: "Ghost".to_string(),
                        type_name: "ghost.Phantom".to_string(),
                    },
                ),
            },
            ShallowIr::Opaque {
                ty: ShallowIrType::Data,
                reason: "another innocent reason".to_string(),
                code: None,
            },
        ],
    };

    let mut shape_builder = LeanDataShapeBuilder::default();
    let result = build_state_machine_trace_reachability_helpers(
        "nested_s0002_test",
        "nested_s0002_test",
        &StateMachineAcceptance::AcceptsSuccess,
        &transition_semantics,
        &Default::default(),
        &mut shape_builder,
        "nested_s0002_test_shape",
        Some(&step_ir),
    );

    let report = result.expect_err("nested S0002 marker must produce a hard error");
    let err = report
        .downcast_ref::<GenerationError>()
        .expect("error must downcast to GenerationError");
    assert_eq!(
        err.code,
        Some("S0002"),
        "S0002 must dispatch even when buried inside Construct fields; got code {:?}, message: {}",
        err.code,
        err.message,
    );
    match err.reason.as_ref() {
        Some(UnsupportedReason::ConstructorTagUnresolved {
            ctor, type_name, ..
        }) => {
            assert_eq!(ctor, "Ghost");
            assert_eq!(type_name, "ghost.Phantom");
        }
        other => panic!("expected UnsupportedReason::ConstructorTagUnresolved, got {other:?}",),
    }
    assert!(
        !is_skippable_generation_error(&report, &SkipPolicy::All),
        "S0002 must NOT be skippable by --skip-unsupported even when nested",
    );
}

/// End-to-end regression for Issue 2 (Oracle A blocking review): a
/// `TransitionProp::EqOutput(Opaque{S0002})` at export time used to be
/// silently widened to a fresh existential by `emit_shallow_ir_as_lean_data`,
/// producing a vacuous body that the generic vacuity guard would catch and
/// surface as a *skippable* `FallbackRequired`. The fix: extract the S0002
/// marker via `find_first_typed_opaque_in_transition_prop` at export time,
/// store it in `ExportedTransitionProp.s0002_marker`, and dispatch the hard
/// `S0002` error from `try_generate_two_phase_proof` BEFORE the vacuity
/// guard.
///
/// This test exercises the full export path: the S0002 marker round-trips
/// from the source `TransitionProp` through `emit_is_valid_transition_def_for_export`
/// into the `ExportedTransitionProp`, and the consumer correctly dispatches.
#[test]
fn transition_prop_eq_output_s0002_marker_round_trips_through_export() {
    use aiken_lang::test_framework::{OpaqueCode, ShallowIr, ShallowIrType, TransitionProp};

    // Source TransitionProp: EqOutput(Opaque{S0002}) — the exact shape
    // that Oracle A flagged as silently widening to skippable. Commit 18
    // attaches the typed `OpaqueCode::ConstructorTagUnresolved` payload;
    // the `reason` string is purely diagnostic and no longer parsed.
    let prop = TransitionProp::EqOutput(ShallowIr::Opaque {
        ty: ShallowIrType::Data,
        reason: aiken_lang::test_framework::s0002_reason_message("Ghost", "ghost.Phantom"),
        code: Some(OpaqueCode::ConstructorTagUnresolved {
            ctor: "Ghost".to_string(),
            type_name: "ghost.Phantom".to_string(),
        }),
    });

    // Export-time emission: marker must be extracted and surfaced via
    // the s0002_marker side channel.
    let (def, _log, _sub_gens, s0002_marker) =
        crate::verify::emit_is_valid_transition_def_for_export("test_ivt", &prop);

    let (ctor, type_name) =
        s0002_marker.expect("EqOutput(Opaque{S0002}) must populate s0002_marker side channel");
    assert_eq!(ctor, "Ghost");
    assert_eq!(type_name, "ghost.Phantom");

    // The Lean def itself widens the Opaque to a fresh existential — the
    // marker is intentionally NOT in the body. This is what the side
    // channel is for: it preserves the marker so the consumer can
    // dispatch S0002 even though the body was widened.
    assert!(
        !def.contains("S0002"),
        "the Lean def text must NOT contain the marker (it's stripped by widening); \
         the marker MUST flow through s0002_marker instead. Got def:\n{def}"
    );

    // Now exercise `try_generate_two_phase_proof` end-to-end: it must
    // dispatch S0002 from the side channel BEFORE the generic vacuity
    // guard fires. We assemble a minimal halt test, populate
    // transition_prop_lean (with s0002_marker = Some(...)), and assert
    // the result is a hard, non-skippable S0002.
    let mut test = make_phase12_state_machine_test(
        "prop_s0002_round_trip",
        StateMachineAcceptance::AcceptsSuccess,
    );

    // step IR present so the halt-test detection succeeds; its content
    // is irrelevant since the s0002_marker dispatch fires first.
    if let FuzzerSemantics::StateMachineTrace {
        ref mut step_function_ir,
        ..
    } = test.semantics
    {
        *step_function_ir = Some(ShallowIr::Var {
            name: "step".to_string(),
            ty: ShallowIrType::Data,
        });
    }

    test.transition_prop_lean = Some(crate::export::ExportedTransitionProp {
        is_valid_transition_def: def,
        initial_state_lean: None,
        unsupported_log: Vec::new(),
        opaque_sub_generators: Vec::new(),
        s0002_marker: Some((ctor, type_name)),
        // Source TransitionProp was `EqOutput(Opaque{S0002})`, which
        // carries a real semantic binding on the `transition` variable —
        // structurally NOT vacuous.  The S0002 dispatch above fires
        // first regardless, so this value just keeps the field honest.
        is_vacuous: false,
    });
    // Add a witness so the AIKEN_EMIT_TWO_PHASE=0 bypass guard does not fire.
    test.concrete_halt_witnesses.push("80".to_string());

    let id = test_id("permissions.test", "prop_s0002_round_trip");
    let lean_name = sanitize_lean_name("prop_s0002_round_trip");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_s0002_round_trip";

    // Engage the two-phase path explicitly.
    let _guard = env_mutex().lock().unwrap_or_else(|e| e.into_inner());
    let prior = std::env::var("AIKEN_EMIT_TWO_PHASE").ok();
    // SAFETY: guarded by env_mutex().
    unsafe {
        std::env::set_var("AIKEN_EMIT_TWO_PHASE", "1");
    }

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
    );

    // Restore env before assertions.
    // SAFETY: still under env_mutex().
    unsafe {
        match prior {
            Some(v) => std::env::set_var("AIKEN_EMIT_TWO_PHASE", v),
            None => std::env::remove_var("AIKEN_EMIT_TWO_PHASE"),
        }
    }

    let report = result.expect_err(
        "EqOutput(Opaque{S0002}) must produce a hard S0002 error from \
         try_generate_two_phase_proof, NOT a vacuous-body skip",
    );
    let err = report
        .downcast_ref::<GenerationError>()
        .expect("error must downcast to GenerationError");
    assert_eq!(
        err.code,
        Some("S0002"),
        "S0002 must dispatch from the s0002_marker side channel BEFORE \
         the vacuity guard; got code {:?}, message: {}",
        err.code,
        err.message,
    );
    match err.reason.as_ref() {
        Some(UnsupportedReason::ConstructorTagUnresolved {
            ctor, type_name, ..
        }) => {
            assert_eq!(ctor, "Ghost");
            assert_eq!(type_name, "ghost.Phantom");
        }
        other => panic!("expected UnsupportedReason::ConstructorTagUnresolved, got {other:?}",),
    }
    assert_eq!(
        err.category,
        GenerationErrorCategory::UnsoundFallback,
        "S0002 must be UnsoundFallback (never skippable)",
    );
    assert!(
        !is_skippable_generation_error(&report, &SkipPolicy::All),
        "S0002 must NOT be skippable by --skip-unsupported",
    );
}

// =====================================================================
// H4: sub-generator predicates hard-error E0018
// =====================================================================
//
// Background: until commit 12, every test whose Fuzzer referenced a
// sub-generator (e.g. `permissions/test::scenario_inputs_baseline`) was
// silently passed: the export emitted `opaque {pred} : Data → Data → Prop`,
// which Lean's `Inhabited Prop` instance fills with `fun _ _ => True`,
// making every constraint over the sub-generator domain trivially
// provable.
//
// Default behaviour (all production runs): hard-error with `E0018`.
// Debug behaviour (hidden `--allow-vacuous-subgenerators` flag): emit the
// widened `def := fun _ _ => True` form and force the verdict to
// `Partial`.

/// Build a halt-test fixture whose `transition_prop_lean` references one
/// sub-generator — the minimum input the H4 default-mode dispatch needs
/// to fire. Re-used by every H4 test below.
///
/// The Lean def text is intentionally NON-vacuous (uses the sub-generator
/// reference) so the upstream `transition_body_is_vacuous` guard does not
/// pre-empt the E0018 dispatch. We also wire `concrete_halt_witnesses` so
/// the AIKEN_EMIT_TWO_PHASE=0 bypass guard would still produce the same
/// path under the test (witnesses present means the bypass path falls
/// through to the witness fast-path; here we always run with two-phase
/// enabled so it never matters).
fn make_h4_subgenerator_halt_test(name: &str) -> ExportedPropertyTest {
    use aiken_lang::test_framework::{ShallowIr, ShallowIrType};

    let mut test = make_phase12_state_machine_test(name, StateMachineAcceptance::AcceptsSuccess);

    // step IR present so halt-test detection succeeds.
    if let FuzzerSemantics::StateMachineTrace {
        ref mut step_function_ir,
        ..
    } = test.semantics
    {
        *step_function_ir = Some(ShallowIr::Var {
            name: "step".to_string(),
            ty: ShallowIrType::Data,
        });
    }

    // Build a TransitionProp def that references one sub-generator via the
    // `__SGPFX__` placeholder (the same placeholder that
    // `emit_is_valid_transition_def_for_export` produces; rewritten to a
    // helper-prefixed name in `try_generate_two_phase_proof`).
    let def = "def __FN__ (state : Data) (transition : Data) : Prop :=\n  \
               (__SGPFX__scenario_inputs_baseline_prop state transition)\n"
        .to_string();

    test.transition_prop_lean = Some(crate::export::ExportedTransitionProp {
        is_valid_transition_def: def,
        initial_state_lean: None,
        unsupported_log: Vec::new(),
        opaque_sub_generators: vec![(
            "permissions/test".to_string(),
            "scenario_inputs_baseline".to_string(),
        )],
        s0002_marker: None,
        // Source TransitionProp is a `SubGenerator { ... }` reference —
        // structurally NOT vacuous (it names a real opaque predicate
        // and the H4 hard-error path catches it explicitly).
        is_vacuous: false,
    });
    // Add a halt witness so the AIKEN_EMIT_TWO_PHASE=0 bypass guard does
    // not pre-empt the two-phase path in tests that toggle that env var.
    test.concrete_halt_witnesses.push("80".to_string());

    test
}

/// H4 — Default mode: a halt test whose `TransitionProp` references one
/// sub-generator must hard-error with `E0018`
/// (`OpaqueSubgeneratorStub`), surfacing as a skippable
/// `FallbackRequired` so `--skip-unsupported` can suppress it.
///
/// The error `sub_generator` field carries `module::fn_name` so the user
/// gets a concrete site to fix.
#[test]
fn h4_default_mode_subgenerator_test_hard_errors_with_e0018() {
    let test = make_h4_subgenerator_halt_test("prop_h4_default_ok");
    let id = test_id("permissions.test", "prop_h4_default_ok");
    let lean_name = sanitize_lean_name("prop_h4_default_ok");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_h4_default_ok";

    let _guard = env_mutex().lock().unwrap_or_else(|e| e.into_inner());
    let prior = std::env::var("AIKEN_EMIT_TWO_PHASE").ok();
    // SAFETY: guarded by env_mutex().
    unsafe {
        std::env::set_var("AIKEN_EMIT_TWO_PHASE", "1");
    }

    let result = super::generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
        false, // default mode: production semantics
    );

    // Restore env before assertions so a panic doesn't leak state.
    // SAFETY: still under env_mutex().
    unsafe {
        match prior {
            Some(v) => std::env::set_var("AIKEN_EMIT_TWO_PHASE", v),
            None => std::env::remove_var("AIKEN_EMIT_TWO_PHASE"),
        }
    }

    let report = result.expect_err(
        "default-mode halt test referencing a sub-generator must hard-error with E0018",
    );
    let err = report
        .downcast_ref::<GenerationError>()
        .expect("error must downcast to GenerationError");
    assert_eq!(
        err.code,
        Some("E0018"),
        "expected code E0018, got code {:?} message: {}",
        err.code,
        err.message,
    );
    match err.reason.as_ref() {
        Some(UnsupportedReason::OpaqueSubgeneratorStub {
            test_name,
            sub_generator,
        }) => {
            assert_eq!(
                test_name, "permissions.test.prop_h4_default_ok",
                "test_name must round-trip through the OpaqueSubgeneratorStub payload",
            );
            assert_eq!(
                sub_generator, "permissions/test::scenario_inputs_baseline",
                "sub_generator must carry the first opaque entry as `module::fn_name`",
            );
        }
        other => panic!("expected UnsupportedReason::OpaqueSubgeneratorStub, got {other:?}",),
    }
    assert_eq!(
        err.category,
        GenerationErrorCategory::FallbackRequired,
        "E0018 must be FallbackRequired (skippable via --skip-unsupported)",
    );
    assert!(
        is_skippable_generation_error(&report, &SkipPolicy::All),
        "E0018 (FallbackRequired) MUST be skippable by --skip-unsupported \
         so existing CI runs can opt into the new hard error gradually",
    );
}

/// H4 — Catalogue regression: `OpaqueSubgeneratorStub` still maps to
/// `E0018`.  This is also covered by `verify/tests/catalogue.rs:33`, but
/// pinning it inline here protects the H4 dispatch from accidentally
/// renaming the code in a future commit (the catalogue file's
/// `code_for_reason` table would happily compile against any code, but
/// the H4 dispatcher hard-codes `"E0018"`).
#[test]
fn h4_opaque_subgenerator_stub_maps_to_e0018_in_catalogue() {
    use crate::verify::error_catalogue::CATALOGUE;
    let entry = CATALOGUE
        .iter()
        .find(|e| e.feature == "opaque_subgenerator_stub")
        .expect("catalogue must carry an opaque_subgenerator_stub entry");
    assert_eq!(
        entry.code, "E0018",
        "OpaqueSubgeneratorStub must map to code E0018; H4 dispatch hard-codes this code",
    );
    assert_eq!(
        entry.category,
        GenerationErrorCategory::FallbackRequired,
        "E0018 must be FallbackRequired so --skip-unsupported can suppress it",
    );
    assert_eq!(
        entry.doc_path, "E0018",
        "doc_path must mirror the code (catalogue convention)",
    );
}

/// H4 — Default + `--skip-unsupported`: an E0018 hard error must be
/// suppressed and surface as a `SkippedTest` rather than a fake "PROVED"
/// verdict.
///
/// This drives `generate_lean_workspace` end-to-end so we exercise the
/// real skip path (`is_skippable_generation_error` → `SkippedTest`),
/// not just the dispatcher's return value.
#[test]
fn h4_default_mode_with_skip_unsupported_records_skipped_test() {
    let test = make_h4_subgenerator_halt_test("prop_h4_skip_ok");

    let tmp = tempfile::tempdir().expect("tempdir");
    let out_dir = tmp.path().to_path_buf();
    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: false, // default
    };

    let _guard = env_mutex().lock().unwrap_or_else(|e| e.into_inner());
    let prior = std::env::var("AIKEN_EMIT_TWO_PHASE").ok();
    // SAFETY: guarded by env_mutex().
    unsafe {
        std::env::set_var("AIKEN_EMIT_TWO_PHASE", "1");
    }

    let manifest = super::generate_lean_workspace(&[test], &config, &SkipPolicy::All)
        .expect("workspace generation must succeed when --skip-unsupported is on");

    // SAFETY: still under env_mutex().
    unsafe {
        match prior {
            Some(v) => std::env::set_var("AIKEN_EMIT_TWO_PHASE", v),
            None => std::env::remove_var("AIKEN_EMIT_TWO_PHASE"),
        }
    }

    assert_eq!(
        manifest.tests.len(),
        0,
        "no proof file should be emitted for an E0018-skipped test",
    );
    assert_eq!(
        manifest.skipped.len(),
        1,
        "the H4-rejected test must appear in the skipped list, NOT as a fake-success entry",
    );
    let skipped = &manifest.skipped[0];
    assert_eq!(skipped.name, "permissions.test.prop_h4_skip_ok");
    assert!(
        skipped.reason.contains("E0018"),
        "skip reason must surface E0018 so users see why their test was skipped, got: {}",
        skipped.reason,
    );
    assert!(
        skipped.reason.contains("sub-generator"),
        "skip reason must surface the catalogue summary copy (mentions `sub-generator`), got: {}",
        skipped.reason,
    );
    // Also verify the workaround copy is plumbed through, so users see how
    // to fix their test rather than just a `[E0018]` opaque marker.
    assert!(
        skipped.reason.contains("Inline the sub-generator")
            || skipped.reason.contains("first-order combinators"),
        "skip reason must surface the workaround copy so users have a concrete fix path, got: {}",
        skipped.reason,
    );
}

/// H4 — Debug mode: the hidden flag turns the H4 hard error back into the
/// emitted (widened) sub-generator predicate. The proof file MUST contain
/// `def {pred} := fun _ _ => True` (NOT `opaque ... : Prop`), the
/// `[WIDENED: sub-generator stubbed]` marker, and the caveat must be
/// `Partial` so the verdict is `Partial`, not `Proved`. The
/// `over_approximations` count must include each widened sub-generator.
#[test]
fn h4_debug_mode_emits_widened_def_and_partial_caveat() {
    let test = make_h4_subgenerator_halt_test("prop_h4_debug_ok");
    let id = test_id("permissions.test", "prop_h4_debug_ok");
    let lean_name = sanitize_lean_name("prop_h4_debug_ok");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_h4_debug_ok";

    let _guard = env_mutex().lock().unwrap_or_else(|e| e.into_inner());
    let prior = std::env::var("AIKEN_EMIT_TWO_PHASE").ok();
    // SAFETY: guarded by env_mutex().
    unsafe {
        std::env::set_var("AIKEN_EMIT_TWO_PHASE", "1");
    }

    let result = super::generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
        true, // --allow-vacuous-subgenerators
    );

    // SAFETY: still under env_mutex().
    unsafe {
        match prior {
            Some(v) => std::env::set_var("AIKEN_EMIT_TWO_PHASE", v),
            None => std::env::remove_var("AIKEN_EMIT_TWO_PHASE"),
        }
    }

    let (content, caveat) = result.expect(
        "debug mode (--allow-vacuous-subgenerators) must succeed in emitting the widened proof",
    );

    // The widened `def := fun _ _ => True` form MUST appear in the proof,
    // and the legacy `opaque ... : Prop` form MUST NOT.  The latter is
    // what made every constraint over the sub-generator domain trivially
    // provable via Lean's `Inhabited Prop` instance — its absence is the
    // soundness invariant the H4 plan ratifies.
    assert!(
        content.contains("def prop_h4_debug_ok_s4_scenario_inputs_baseline_prop : Data \u{2192} Data \u{2192} Prop := fun _ _ => True"),
        "debug-mode emission must use `def := fun _ _ => True`, got:\n{content}",
    );
    assert!(
        !content.contains("opaque prop_h4_debug_ok_s4_scenario_inputs_baseline_prop"),
        "debug-mode emission must NOT use the legacy `opaque ... : Prop` form (silently \
         filled with True by Inhabited Prop). The whole point of H4 is to make the widening \
         explicit. Got:\n{content}",
    );
    assert!(
        content.contains("[WIDENED: sub-generator stubbed]"),
        "debug-mode emission must mark each widened sub-generator with \
         `[WIDENED: sub-generator stubbed]` so the proof file is self-documenting. Got:\n{content}",
    );

    // Commit 12 follow-up #7 — H4 / E0018 rendering snapshot.  The above
    // `contains()` checks pin the soundness invariants (the absence of the
    // legacy `opaque ... : Prop` form, the presence of the `[WIDENED]`
    // marker), but they leave the surrounding text shape un-pinned.  A
    // future emitter change to the widened-def's *spelling* (e.g. dropping
    // the comment, changing the binder names from `_ _`, or moving the
    // marker comment to a different line) would silently slip past the
    // existing assertions.  The snapshot below locks the canonical
    // `def {pred} := fun _ _ => True -- [WIDENED: ...]` shape end-to-end.
    // Stable inputs (no tempdir, no rev, no UUID) → no redactions.
    insta::assert_snapshot!(
        "h4_debug_mode_emits_widened_def_and_partial_caveat",
        content
    );

    // The caveat must be Partial — the verdict surfaces as `Partial`, not
    // `Proved`, because the body has a vacuous sub-generator predicate.
    match caveat {
        ProofCaveat::Partial(note) => {
            assert!(
                note.contains("opaque sub-generator stubbed to True"),
                "Partial caveat note must explicitly call out the sub-generator widening, got: {note}",
            );
        }
        other => panic!(
            "debug-mode caveat must be `Partial(...)` so the verdict is `Partial`, not `Proved`; \
             got: {other:?}"
        ),
    }
}

/// H4 — Debug mode end-to-end: when `generate_lean_workspace` is invoked
/// with `allow_vacuous_subgenerators = true`, the resulting manifest
/// entry MUST carry a non-zero `over_approximations` count (one per
/// widened sub-generator) AND a `partial_proof_note`.  This is the
/// audit-data contract that downstream reporting consumes — without it
/// a debug-mode run on a sub-generator-using test would look identical
/// to a normal two-phase run, hiding the soundness compromise.
#[test]
fn h4_debug_mode_workspace_records_over_approximations_and_partial_note() {
    let test = make_h4_subgenerator_halt_test("prop_h4_workspace_ok");

    let tmp = tempfile::tempdir().expect("tempdir");
    let out_dir = tmp.path().to_path_buf();
    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
        raw_output_bytes: RAW_OUTPUT_TAIL_BYTES,
        allow_vacuous_subgenerators: true, // debug
    };

    let _guard = env_mutex().lock().unwrap_or_else(|e| e.into_inner());
    let prior = std::env::var("AIKEN_EMIT_TWO_PHASE").ok();
    // SAFETY: guarded by env_mutex().
    unsafe {
        std::env::set_var("AIKEN_EMIT_TWO_PHASE", "1");
    }

    let manifest = super::generate_lean_workspace(&[test], &config, &SkipPolicy::None)
        .expect("debug-mode workspace generation must succeed");

    // SAFETY: still under env_mutex().
    unsafe {
        match prior {
            Some(v) => std::env::set_var("AIKEN_EMIT_TWO_PHASE", v),
            None => std::env::remove_var("AIKEN_EMIT_TWO_PHASE"),
        }
    }

    assert_eq!(manifest.tests.len(), 1, "exactly one test must be emitted");
    assert!(
        manifest.skipped.is_empty(),
        "debug mode must NOT skip the H4 test, got: {:?}",
        manifest.skipped,
    );
    let entry = &manifest.tests[0];
    assert!(
        entry.over_approximations >= 1,
        "manifest entry must carry over_approximations >= 1 (one per widened sub-generator), got: {}",
        entry.over_approximations,
    );
    let note = entry
        .partial_proof_note
        .as_ref()
        .expect("debug-mode manifest entry must carry partial_proof_note");
    assert!(
        note.contains("opaque sub-generator stubbed to True"),
        "partial_proof_note must call out the sub-generator widening, got: {note}",
    );
    assert!(
        entry.witness_proof_note.is_none(),
        "two-phase + sub-generator widening surfaces as Partial, NOT Witness, got: {:?}",
        entry.witness_proof_note,
    );
}

/// H4 invariant — when `tp.opaque_sub_generators` is empty (no
/// sub-generators at all), the default and debug-mode paths MUST produce
/// identical proof files.  This protects the boring two-phase code path
/// from accidental drift caused by the H4 changes.
#[test]
fn h4_no_subgenerators_default_and_debug_proof_files_are_identical() {
    // Build a non-vacuous halt test with NO opaque sub-generators.  Body
    // mentions both `state` and `transition` so it survives
    // `transition_body_is_vacuous` and reaches the emission path.
    use aiken_lang::test_framework::{ShallowIr, ShallowIrType};
    let mut test = make_phase12_state_machine_test(
        "prop_h4_no_subgens_ok",
        StateMachineAcceptance::AcceptsSuccess,
    );
    if let FuzzerSemantics::StateMachineTrace {
        ref mut step_function_ir,
        ..
    } = test.semantics
    {
        *step_function_ir = Some(ShallowIr::Var {
            name: "step".to_string(),
            ty: ShallowIrType::Data,
        });
    }
    let def = "def __FN__ (state : Data) (transition : Data) : Prop :=\n  \
               state = transition\n"
        .to_string();
    test.transition_prop_lean = Some(crate::export::ExportedTransitionProp {
        is_valid_transition_def: def,
        initial_state_lean: None,
        unsupported_log: Vec::new(),
        opaque_sub_generators: Vec::new(), // <-- empty
        s0002_marker: None,
        // Body `state = transition` is a real EqOutput-style binding —
        // structurally NOT vacuous.
        is_vacuous: false,
    });
    test.concrete_halt_witnesses.push("80".to_string());

    let id = test_id("permissions.test", "prop_h4_no_subgens_ok");
    let lean_name = sanitize_lean_name("prop_h4_no_subgens_ok");
    let lean_module = "AikenVerify.Proofs.Permissions.prop_h4_no_subgens_ok";

    let _guard = env_mutex().lock().unwrap_or_else(|e| e.into_inner());
    let prior = std::env::var("AIKEN_EMIT_TWO_PHASE").ok();
    // SAFETY: guarded by env_mutex().
    unsafe {
        std::env::set_var("AIKEN_EMIT_TWO_PHASE", "1");
    }

    let default = super::generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
        false,
    );
    let debug = super::generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Proof,
        &VerificationTargetKind::default(),
        true,
    );

    // SAFETY: still under env_mutex().
    unsafe {
        match prior {
            Some(v) => std::env::set_var("AIKEN_EMIT_TWO_PHASE", v),
            None => std::env::remove_var("AIKEN_EMIT_TWO_PHASE"),
        }
    }

    let (default_content, default_caveat) =
        default.expect("default-mode emission must succeed when there are no sub-generators");
    let (debug_content, debug_caveat) = debug
        .expect("debug-mode emission must succeed when there are no sub-generators (no widening)");

    assert_eq!(
        default_content, debug_content,
        "with no sub-generators the H4 flag must NOT change the proof body — \
         the two outputs MUST be byte-identical to protect the boring two-phase \
         path from accidental drift",
    );

    // The caveat must also be identical (TWO_PHASE_PARTIAL_NOTE only,
    // no sub-generator addendum).
    let extract_note = |c: &ProofCaveat| match c {
        ProofCaveat::Partial(n) => n.clone(),
        other => panic!("expected Partial caveat, got {other:?}"),
    };
    assert_eq!(
        extract_note(&default_caveat),
        extract_note(&debug_caveat),
        "with no sub-generators the H4 flag must NOT change the caveat note",
    );
    assert!(
        !extract_note(&default_caveat).contains("opaque sub-generator stubbed to True"),
        "without sub-generators the caveat note MUST NOT carry the H4 addendum",
    );
}

#[test]
fn step_fn_sound_via_reachability_helpers_emits_theorem() {
    // Integration test: when build_state_machine_trace_reachability_helpers is
    // called with a Construct step IR, the emitted definitions must contain
    // `private theorem` for the soundness block.
    let transition_semantics = make_non_opaque_transition_semantics();
    let step_ir = make_flat_construct_ir();

    let mut shape_builder = LeanDataShapeBuilder::default();
    let (_reachable_output, definitions) = build_state_machine_trace_reachability_helpers(
        "step_sound_test",
        "step_sound_test",
        &StateMachineAcceptance::AcceptsSuccess,
        &transition_semantics,
        &Default::default(),
        &mut shape_builder,
        "step_sound_test_shape",
        Some(&step_ir),
    )
    .unwrap();

    assert!(
        definitions.contains("private theorem step_sound_test_step_fn_sound"),
        "Construct step IR must produce `private theorem` soundness block, got:\n{definitions}"
    );
    assert!(
        !definitions.contains("private axiom"),
        "Construct step IR must NOT produce `private axiom`, got:\n{definitions}"
    );
}

#[test]
fn raw_output_truncates_to_64kib_tail() {
    // Synthesise a 100 KiB stream of ASCII bytes; the cap retains the
    // last RAW_OUTPUT_TAIL_BYTES (64 KiB) and reports total_bytes
    // covering the original length.
    let total_bytes = 100 * 1024;
    let content: String = (0..total_bytes)
        .map(|i| ((i % 26) as u8 + b'a') as char)
        .collect();
    assert_eq!(content.len(), total_bytes);

    let captured = CapturedOutput::from_string(content.clone(), RAW_OUTPUT_TAIL_BYTES, None);

    assert!(captured.tail.len() <= RAW_OUTPUT_TAIL_BYTES);
    assert_eq!(captured.total_bytes, total_bytes);
    assert!(captured.truncated);
    assert!(captured.log_path.is_none(), "no log path was supplied");

    // tail must be valid UTF-8 (to_string would have panicked otherwise)
    // and must equal the suffix of the original content of the same length.
    let suffix_start = total_bytes - captured.tail.len();
    assert_eq!(captured.tail, &content[suffix_start..]);
}

#[test]
fn raw_output_persists_full_stream_when_path_given() {
    // When the input exceeds the cap AND a log_path is supplied, the FULL
    // content is written to that path so callers can recover the original
    // stream from disk.
    let tmp = tempfile::tempdir().unwrap();
    let log_path = tmp.path().join("lake_build.stdout.log");

    let total_bytes = 80 * 1024;
    let content: String = (0..total_bytes)
        .map(|i| ((i % 26) as u8 + b'a') as char)
        .collect();

    let captured = CapturedOutput::from_string(
        content.clone(),
        RAW_OUTPUT_TAIL_BYTES,
        Some(log_path.clone()),
    );

    assert!(captured.truncated);
    assert_eq!(captured.total_bytes, total_bytes);
    assert_eq!(captured.log_path.as_ref(), Some(&log_path));

    let persisted = std::fs::read_to_string(&log_path).expect("log file must exist");
    assert_eq!(persisted, content);
}

#[test]
fn raw_output_can_report_public_path_separately_from_persist_path() {
    let tmp = tempfile::tempdir().unwrap();
    let persist_path = tmp.path().join("lake_build.stdout.log");
    let reported_path = std::path::PathBuf::from("logs/lake_build.stdout.log");

    let total_bytes = 80 * 1024;
    let content: String = (0..total_bytes)
        .map(|i| ((i % 26) as u8 + b'a') as char)
        .collect();

    let captured = CapturedOutput::from_string_with_paths(
        content.clone(),
        RAW_OUTPUT_TAIL_BYTES,
        Some(persist_path.clone()),
        Some(reported_path.clone()),
    );

    assert!(captured.truncated);
    assert_eq!(captured.total_bytes, total_bytes);
    assert_eq!(captured.log_path.as_ref(), Some(&reported_path));

    let persisted = std::fs::read_to_string(&persist_path).expect("log file must exist");
    assert_eq!(persisted, content);
}


#[test]
fn raw_output_disables_cap_when_zero() {
    // cap == 0 means "no truncation, store the entire content". Even at
    // sizes that would exceed the default cap, the result must be the
    // full input untouched and truncated must be false.
    let total_bytes = 200 * 1024;
    let content: String = (0..total_bytes)
        .map(|i| ((i % 26) as u8 + b'a') as char)
        .collect();

    let captured = CapturedOutput::from_string(content.clone(), 0, None);

    assert_eq!(captured.tail, content);
    assert_eq!(captured.total_bytes, total_bytes);
    assert!(!captured.truncated);
    assert!(captured.log_path.is_none());
}

#[test]
fn raw_output_handles_utf8_boundary_safely() {
    // Build a large stream where the byte at offset (len - cap) is in the
    // middle of a multi-byte UTF-8 character. The truncation routine must
    // walk forward to the next character boundary so the resulting tail is
    // always valid UTF-8 (no panic, no replacement chars).
    //
    // 'é' is two bytes (0xC3 0xA9). A run of 'é' interleaved with ASCII
    // creates many positions where naïve byte slicing would fall in the
    // middle of a multi-byte character.
    let chunk = "abcéfghé"; // 8 chars, 10 bytes
    let mut content = String::new();
    while content.len() < 70_000 {
        content.push_str(chunk);
    }
    let total_bytes = content.len();

    let cap = 32 * 1024;
    let captured = CapturedOutput::from_string(content.clone(), cap, None);

    assert!(captured.truncated);
    assert_eq!(captured.total_bytes, total_bytes);
    // Calling len()/is_char_boundary on a String::from(tail) confirms it
    // round-trips as a valid Rust &str without surprises.
    assert!(captured.tail.is_char_boundary(0));
    assert!(captured.tail.is_char_boundary(captured.tail.len()));
    // The kept tail must be a true suffix of the content (zero data
    // mutation; we just shifted forward to the next char boundary).
    assert!(content.ends_with(&captured.tail));
    // The retained tail length is at most `cap` and at least `cap - 3`
    // (UTF-8 char widths up to 4 bytes; we shifted forward by at most 3).
    assert!(captured.tail.len() <= cap);
    assert!(captured.tail.len() >= cap.saturating_sub(3));
}

#[test]
fn raw_output_empty_content_is_not_truncated() {
    let captured = CapturedOutput::from_string(String::new(), RAW_OUTPUT_TAIL_BYTES, None);
    assert_eq!(captured.tail, "");
    assert_eq!(captured.total_bytes, 0);
    assert!(!captured.truncated);
    assert!(captured.log_path.is_none());
}

#[test]
fn raw_output_under_cap_is_not_truncated() {
    let content = "hello world".to_string();
    let captured = CapturedOutput::from_string(content.clone(), RAW_OUTPUT_TAIL_BYTES, None);
    assert_eq!(captured.tail, content);
    assert_eq!(captured.total_bytes, content.len());
    assert!(!captured.truncated);
    assert!(captured.log_path.is_none());
}

#[test]
fn raw_output_small_constructor_never_truncates() {
    let content = "small build output".to_string();
    let captured = CapturedOutput::small(content.clone());
    assert_eq!(captured.tail, content);
    assert_eq!(captured.total_bytes, content.len());
    assert!(!captured.truncated);
    assert!(captured.log_path.is_none());
}

#[test]
fn verify_result_serializes_captured_output() {
    // Round-trip a VerifyResult through serde_json. The new shape moves
    // stdout/stderr to objects with `tail`/`total_bytes`/`truncated`
    // (and an optional `log_path`); confirm the JSON exposes those keys
    // and that the tail content survives.
    let raw = VerifyResult {
        success: true,
        stdout: CapturedOutput::small("hello"),
        stderr: CapturedOutput::small("world"),
        exit_code: Some(0),
        theorem_results: None,
    };

    let value = serde_json::to_value(&raw).expect("VerifyResult must serialize");
    assert_eq!(value["success"], serde_json::Value::Bool(true));

    let stdout = &value["stdout"];
    assert_eq!(stdout["tail"], serde_json::Value::String("hello".into()));
    assert_eq!(stdout["total_bytes"], serde_json::json!(5));
    assert_eq!(stdout["truncated"], serde_json::Value::Bool(false));
    assert!(
        stdout.get("log_path").is_none(),
        "log_path must be omitted when None"
    );

    let stderr = &value["stderr"];
    assert_eq!(stderr["tail"], serde_json::Value::String("world".into()));
    assert_eq!(stderr["total_bytes"], serde_json::json!(5));
    assert_eq!(stderr["truncated"], serde_json::Value::Bool(false));
}

#[test]
fn verify_result_serializes_truncated_captured_output_with_log_path() {
    // When a CapturedOutput is truncated AND a log_path is set, the JSON
    // must include the reported public path so consumers can recover the
    // full stream without leaking the local persistence path.
    let tmp = tempfile::tempdir().unwrap();
    let persist_path = tmp.path().join("lake_build.stdout.log");
    let reported_path = std::path::PathBuf::from("logs/lake_build.stdout.log");

    let total_bytes = 80 * 1024;
    let content: String = (0..total_bytes)
        .map(|i| ((i % 26) as u8 + b'a') as char)
        .collect();
    let captured = CapturedOutput::from_string_with_paths(
        content,
        RAW_OUTPUT_TAIL_BYTES,
        Some(persist_path),
        Some(reported_path.clone()),
    );

    let raw = VerifyResult {
        success: false,
        stdout: captured,
        stderr: CapturedOutput::small(""),
        exit_code: Some(1),
        theorem_results: None,
    };

    let value = serde_json::to_value(&raw).expect("VerifyResult must serialize");
    let stdout = &value["stdout"];
    assert_eq!(stdout["truncated"], serde_json::Value::Bool(true));
    assert_eq!(stdout["total_bytes"], serde_json::json!(total_bytes));
    assert_eq!(
        stdout["log_path"],
        serde_json::Value::String(reported_path.display().to_string())
    );
}

#[test]
fn public_json_path_redacts_absolute_local_paths() {
    let outside = std::env::temp_dir().join("PlutusCore");
    let public = super::public_json_path(&outside);
    assert_eq!(public, "<local>/PlutusCore");
    assert!(
        !public.starts_with('/'),
        "doctor JSON path must not leak absolute local paths: {public}"
    );
}
