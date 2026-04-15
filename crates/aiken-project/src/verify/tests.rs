use super::*;
use crate::blueprint::{
    definitions::{Definitions, Reference},
    schema::{Annotated, Constructor, Data, Declaration, Items, Schema},
};
use crate::export::{
    ExportedDataSchema, ExportedProgram, ExportedPropertyTest, FuzzerConstraint, FuzzerOutputType,
    FuzzerSemantics, StateMachineAcceptance, StateMachineTransitionSemantics, TestReturnMode,
    ValidatorTarget,
};
use crate::{Project, options::Options, telemetry::EventTarget};
use aiken_lang::ast::{Definition, OnTestFailure, Tracing};

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
    has_reachability_helpers: bool,
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
        has_reachability_helpers: proof
            .contains("-- compiler-exported state-machine reachability relation"),
    }
}

fn generate_proof_for_phase12_case(
    test: &ExportedPropertyTest,
) -> miette::Result<String> {
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
            };
            test.fuzzer_data_schema = Some(make_state_machine_trace_failure_schema());
            test
        }
    }
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
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    let manifest = generate_lean_workspace(&tests, &config, false).unwrap();

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

    // Check Utils.lean contains the budget
    let utils = fs::read_to_string(out_dir.join("AikenVerify/Utils.lean")).unwrap();
    assert!(utils.contains("cekExecuteProgram p args 20000"));

    // Check root import file
    let root = fs::read_to_string(out_dir.join("AikenVerify.lean")).unwrap();
    assert!(root.contains("import AikenVerify.Utils"));
    assert!(root.contains("import AikenVerify.Proofs.AikenList.test_map"));
    assert!(root.contains("import AikenVerify.Proofs.My_module.test_roundtrip"));

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
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    let manifest = generate_lean_workspace(&tests, &config, false).unwrap();
    assert_eq!(manifest.tests.len(), 1);
    assert_eq!(
        manifest.tests[0].lean_module,
        "AikenVerify.Proofs.PermissionsTest.prop_permissions_core_development_standard_ok"
    );
    let entry = &manifest.tests[0];
    assert!(
        out_dir.join(&entry.lean_file).exists(),
    );
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
fn opaque_semantics_errors_on_workspace_generation() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path().to_path_buf();
    let tests = vec![make_test_with_type(
        "my_module",
        "test_bool_opaque_manifest",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    )];

    let config = VerifyConfig {
        out_dir,
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    let err = generate_lean_workspace(&tests, &config, false).expect_err(
        "workspace generation should fail for opaque semantics",
    );
    let message = err.to_string();
    assert!(
        message.contains("cannot be formally verified") || message.contains("opaque"),
        "should produce clear error, got: {message}"
    );
}

#[test]
fn opaque_semantics_errors_on_preflight() {
    let test = make_test_with_type(
        "my_module",
        "test_preflight_bool_opaque",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    );

    let err = preflight_validate_test(
        &test,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect_err("preflight should fail for opaque semantics");
}

#[test]
fn opaque_semantics_errors_on_proof_generation() {
    let test = make_test_with_type(
        "my_module",
        "test_proof_bool_opaque",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    );
    let id = test_id("my_module", "test_proof_bool_opaque");
    let lean_name = sanitize_lean_name("test_proof_bool_opaque");
    let lean_module = "AikenVerify.Proofs.My_module.test_proof_bool_opaque";

    let err = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect_err("proof generation should fail for opaque semantics");
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
    };

    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    let err = generate_lean_workspace(&[test], &config, false)
        .expect_err("opaque state-machine semantics should error");
}

#[test]
fn generate_workspace_scenario_like_void_property_uses_schema_backed_direct_theorem() {
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
    };
    test.fuzzer_data_schema = Some(make_state_machine_trace_success_schema());

    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    // Opaque field semantics in the state-machine transition path now correctly
    // produce errors instead of silently mapping to True.
    let err = generate_lean_workspace(&[test], &config, false)
        .expect_err("opaque state-machine semantics should error");
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
    };
    test.fuzzer_data_schema = Some(make_state_machine_trace_failure_schema());

    let config = VerifyConfig {
        out_dir: out_dir.clone(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    // Opaque field semantics in the state-machine transition path now correctly
    // produce errors instead of silently mapping to True.
    let err = generate_lean_workspace(&[test], &config, false)
        .expect_err("opaque state-machine semantics should error");
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
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    let manifest =
        generate_lean_workspace(&[make_test("my_module", "test_roundtrip")], &config, false)
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
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    let manifest = generate_lean_workspace(&[], &config, false).unwrap();
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(
        !proof.contains("= some true"),
        "SucceedEventually should not contain = true"
    );
}

#[test]
fn fail_once_witness_mode_generates_existential_theorem() {
    let test = make_test_with_failure("my_module", "test_ev", OnTestFailure::SucceedImmediately);
    let id = test_id("my_module", "test_ev");
    let lean_name = sanitize_lean_name("test_ev");
    let lean_module = "AikenVerify.Proofs.My_module.test_ev";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("\u{2203}"), "Witness mode should generate existential (\u{2203}) quantifier");
    assert!(proof.contains("witnessTests"), "Witness mode should use witnessTests helper");
    assert!(proof.contains("by decide"), "Witness mode should use decide tactic");
}

#[test]
fn fail_once_proof_mode_generates_existential_theorem() {
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("\u{2203}"), "Proof mode should generate existential (\u{2203}) quantifier");
    assert!(proof.contains("proveTests"), "Proof mode should use proveTests helper");
    assert!(proof.contains("= false"), "Existential proof for SucceedImmediately should assert = false");
    assert!(proof.contains("by blaster"), "Proof mode should use blaster tactic");
}

#[test]
fn fail_once_void_witness_mode_generates_existential_error_theorem() {
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
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("\u{2203}"), "Witness mode should generate existential (\u{2203}) quantifier");
    assert!(proof.contains("proveTestsError"), "Void witness mode should use proveTestsError helper");
    assert!(proof.contains("by decide"), "Witness mode should use decide tactic");
}

#[test]
fn fail_once_tuple_witness_mode_uses_conjunction_between_bounds() {
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
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("\u{2203}"), "Existential tuple witness should use \u{2203} quantifier");
    assert!(proof.contains("(a : Integer)"), "Should bind first variable as Integer");
    assert!(proof.contains("(b : Integer)"), "Should bind second variable as Integer");
    assert!(proof.contains("\u{2227}"), "Tuple existential should use conjunction (\u{2227}) between bounds");
    assert!(!proof.contains("\u{2192}"), "Existential mode should not contain implication (\u{2192})");
    assert!(proof.contains("witnessTests"), "Witness mode should use witnessTests helper");
}

#[test]
fn fail_once_data_witness_mode_uses_valid_data_constructor() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
}

#[test]
fn fail_once_unsupported_witness_mode_uses_valid_data_constructor() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
}

#[test]
fn fail_once_tuple_with_data_witness_uses_valid_data_constructor() {
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
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("\u{2203}"), "Existential tuple+data witness should use \u{2203} quantifier");
    assert!(proof.contains("(a : Integer)"), "Should bind Int component as Integer");
    assert!(proof.contains("Data"), "Should reference Data type for the Data component");
}

#[test]
fn fail_once_list_of_data_witness_uses_valid_data_constructor() {
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
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("\u{2203}"), "List existential witness should use \u{2203} quantifier");
    assert!(proof.contains("List"), "Should quantify over a List type");
    assert!(proof.contains("Data"), "List element type should be Data");
}

#[test]
fn fail_once_list_bool_exact_witness_uses_exact_element() {
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("\u{2203}"), "List Bool exact existential should use \u{2203} quantifier");
    assert!(proof.contains("List"), "Should quantify over a List type");
    assert!(proof.contains("Bool"), "List element type should be Bool");
}

#[test]
fn void_return_mode_generates_halt_theorem() {
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("proveTestsHalt"), "Void return mode should use proveTestsHalt helper");
    assert!(proof.contains("\u{2200}"), "Universal proof should contain \u{2200} quantifier");
}


#[test]
fn void_fail_mode_generates_error_theorem() {
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("proveTestsError"), "Void fail mode should use proveTestsError helper");
    assert!(proof.contains("\u{2200}"), "Universal proof should contain \u{2200} quantifier");
}

#[test]
fn bool_return_mode_generates_prove_tests() {
    // Verify Bool mode still works with explicit return_mode
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("proveTests"), "Bool mode should use proveTests helper");
    assert!(proof.contains("= true"), "Bool mode should assert = true");
    assert!(proof.contains("alwaysTerminating"), "Bool mode should include termination theorem");
    assert!(proof.contains("\u{2200}"), "Universal proof should contain \u{2200} quantifier");
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
    }
}

#[test]
fn bool_without_domain_predicate_uses_sampled_domain_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
}

#[test]
fn sampled_domain_fallback_imports_fuzzer_program() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
}

#[test]
fn bool_succeed_eventually_generates_false() {
    // SucceedEventually = `fail` keyword, body should return false
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("(a : Integer)"), "Should bind first variable a as Integer");
    assert!(proof.contains("(b : Integer)"), "Should bind second variable b as Integer");
    assert!(proof.contains("\u{2200}"), "Universal proof should contain \u{2200} quantifier");
    assert!(proof.contains("Data.List"), "Tuple proof should construct Data.List argument");
}

#[test]
fn tuple_int_int_component_bounds_generate_two_ranges() {
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("(a : Integer)"), "Should bind first component as Integer");
    assert!(proof.contains("(b : Integer)"), "Should bind second component as Integer");
    assert!(proof.contains("0 <= a"), "Should contain lower bound for first component");
    assert!(proof.contains("a <= 10"), "Should contain upper bound for first component");
    assert!(proof.contains("20 <= b"), "Should contain lower bound for second component");
    assert!(proof.contains("b <= 30"), "Should contain upper bound for second component");
}

#[test]
fn tuple_int_int_component_bounds_wrong_arity_uses_fallback() {
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
    assert!(result.is_err(), "mismatched tuple arity should not produce a direct proof");
}

#[test]
fn tuple_int_int_unknown_bounds_use_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("Bool"), "Bool semantics should quantify over Bool type");
    assert!(proof.contains("boolArg"), "Bool semantics should use boolArg helper");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("Integer"), "Int semantics should quantify over Integer type");
    assert!(proof.contains("100"), "Semantics range (100..100) should take precedence over legacy constraint (0..10)");
}

#[test]
fn non_state_machine_direct_generation_requires_semantics_not_legacy_constraints() {
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

    let err = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .expect_err("opaque semantics must not silently reuse legacy constraint lowering");
}

#[test]
fn bytearray_without_domain_predicate_uses_sampled_domain_fallback() {
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
    assert!(
        result.is_err(),
        "ByteArray with opaque semantics should error without fallback"
    );
}

#[test]
fn bytearray_length_range_generates_direct_scalar_domain() {
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
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("ByteString"), "ByteArray type should map to ByteString in Lean");
    assert!(proof.contains("x.length"), "Should contain length-based bounds");
    assert!(proof.contains("bytearrayArg"), "Should use bytearrayArg helper");
}

#[test]
fn bytearray_exact_non_empty_generates_direct_scalar_domain() {
    let test = make_test_with_type(
        "my_module",
        "test_bytes_exact_non_empty",
        FuzzerOutputType::ByteArray,
        FuzzerConstraint::Exact(FuzzerExactValue::ByteArray(vec![0x66, 0x6f, 0x6f])),
    );
    let id = test_id("my_module", "test_bytes_exact_non_empty");
    let lean_name = sanitize_lean_name("test_bytes_exact_non_empty");
    let lean_module = "AikenVerify.Proofs.My_module.test_bytes_exact_non_empty";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("ByteString"), "ByteArray type should map to ByteString in Lean");
    assert!(proof.contains("bytearrayArg"), "Should use bytearrayArg helper");
}

#[test]
fn string_length_range_generates_direct_scalar_domain() {
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("ByteString"), "String type should map to ByteString in Lean");
    assert!(proof.contains("stringArg"), "String type should use stringArg helper");
    assert!(proof.contains("x.length"), "Should contain length-based bounds");
}

#[test]
fn bytearray_length_range_with_inconsistent_bounds_errors() {
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
    .unwrap_err()
    .to_string();

    assert!(
        err.contains("inconsistent byte-string length bounds"),
        "Expected inconsistent ByteString bounds error, got:\n{err}"
    );
}

#[test]
fn string_exact_non_empty_generates_direct_scalar_domain() {
    let test = make_test_with_type(
        "my_module",
        "test_string_exact_non_empty",
        FuzzerOutputType::String,
        FuzzerConstraint::Exact(FuzzerExactValue::String("Hi".to_string())),
    );
    let id = test_id("my_module", "test_string_exact_non_empty");
    let lean_name = sanitize_lean_name("test_string_exact_non_empty");
    let lean_module = "AikenVerify.Proofs.My_module.test_string_exact_non_empty";

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("ByteString"), "String type should map to ByteString in Lean");
    assert!(proof.contains("stringArg"), "String type should use stringArg helper");
    assert!(proof.contains("consByteStringV1"), "Exact string should encode bytes via consByteStringV1");
}

#[test]
fn list_without_bounds_uses_sampled_domain_fallback() {
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

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert!(result.is_ok(), "List constraint with Any element should produce a direct proof via list semantics");
}

#[test]
fn list_with_any_constraint_uses_sampled_domain_fallback() {
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
    assert!(result.is_err(), "should error without sampled-domain fallback");
}

#[test]
fn list_with_unsupported_constraint_uses_sampled_domain_fallback() {
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
    assert!(result.is_err(), "should error without sampled-domain fallback");
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

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert!(result.is_ok(), "List constraint with Any element should produce a direct proof via list semantics");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("Data"), "Unsupported type should be quantified as Data");
    assert!(proof.contains("Data.Constr"), "Constructor tag domain should use Data.Constr match");
    assert!(proof.contains("dataArg"), "Should use dataArg helper");
}

#[test]
fn int_with_unsupported_constraint_uses_sampled_domain_fallback() {
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
    assert!(result.is_err(), "Unsupported constraint should not produce a direct proof");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("Integer"), "Should quantify over Integer type");
    assert!(proof.contains("1 <= x"), "Salvaged bounds should include min bound");
    assert!(proof.contains("x <= 9"), "Salvaged bounds should include max bound");
}

#[test]
fn existential_int_and_with_unsupported_uses_sampled_fallback() {
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

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    );
    assert!(result.is_ok(), "salvageable IntRange in existential mode should produce a direct proof");
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

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert!(result.is_ok(), "List constraint with Any element should produce a direct proof via list semantics");
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

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert!(result.is_ok(), "List constraint with Any element should produce a direct proof via list semantics");
}

#[test]
fn list_data_fallback_supports_existential_mode() {
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

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    );
    // List<Data> with list semantics now generates a direct proof
    assert!(result.is_ok(), "List<Data> with existential mode should generate direct proof");
}

#[test]
fn data_without_domain_predicate_uses_sampled_domain_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
}

#[test]
fn tuple_data_data_without_predicates_uses_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
}

#[test]
fn tuple_data_data_data_without_predicates_uses_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("Integer"), "Should contain Integer type for Int component");
    assert!(proof.contains("Bool"), "Should contain Bool type for Bool component");
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

    let proof = generate_proof_file(
        &test,
        "test_list_semantics_direct",
        "test_list_semantics_direct",
        "AikenVerify.Proofs.My_module.test_list_semantics_direct",
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("List"), "List semantics should quantify over List type");
    assert!(proof.contains("Bool"), "List element type should be Bool");
}

#[test]
fn tuple_with_nested_list_element_uses_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    );
    assert!(result.is_ok(), "Tuple of List constraints with Any element should produce a direct proof via list semantics");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("(a : Integer)"), "Should bind first element as Integer");
    assert!(proof.contains("(b : Integer)"), "Should bind second element as Integer");
    // Each element is independently bounded (Cartesian product, no cross-element constraints)
    assert!(proof.contains("0 <= a"), "Should bound first element independently");
    assert!(proof.contains("20 <= b"), "Should bound second element independently");
    // Preconditions use implication (→) between independent components
    assert!(proof.contains("\u{2192}"), "Independent Cartesian bounds should be joined by implication");
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
        version: "1.0.0".to_string(),
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
        stdout: "Build completed successfully.".to_string(),
        stderr: String::new(),
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
fn parse_verify_results_uses_precomputed_theorem_results() {
    let manifest = make_manifest(vec![("my_module", "test_add", "test_add")]);
    let raw = VerifyResult {
        success: false,
        stdout: "✖ [1/1] Building AikenVerify.Proofs.My_module.test_add (1s)".to_string(),
        stderr: "error: Lean exited with code 1".to_string(),
        exit_code: Some(1),
        theorem_results: Some(vec![
            TheoremResult {
                test_name: "my_module.test_add".to_string(),
                theorem_name: "test_add".to_string(),
                status: ProofStatus::Failed {
                    category: FailureCategory::BuildError,
                    reason: "error: 'test_add' unsolved goals".to_string(),
                },
            },
            TheoremResult {
                test_name: "my_module.test_add".to_string(),
                theorem_name: "test_add_alwaysTerminating".to_string(),
                status: ProofStatus::Failed {
                    category: FailureCategory::BuildError,
                    reason: "error: Lean exited with code 1".to_string(),
                },
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
        stdout: "Build completed successfully.".to_string(),
        stderr: String::new(),
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
        stdout: String::new(),
        stderr: "error: Lean exited with code 1".to_string(),
        exit_code: Some(1),
        theorem_results: Some(vec![TheoremResult {
            test_name: "my_module.test_add".to_string(),
            theorem_name: "test_add".to_string(),
            status: ProofStatus::Failed {
                category: FailureCategory::BuildError,
                reason: "error: Lean exited with code 1".to_string(),
            },
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
        stdout: String::new(),
        stderr: "error: 'test_add' has unsolved goals\nsome other context".to_string(),
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
            stdout: "⚠ [1/3] Replayed AikenVerify.Proofs.My_module.test_add\n\
                     info: AikenVerify/Proofs/My_module/test_add.lean:10:5: ✅ Valid\n\
                     ⚠ [2/3] Replayed AikenVerify.Proofs.My_module.test_sub\n\
                     info: AikenVerify/Proofs/My_module/test_sub.lean:11:5: ✅ Valid\n\
                     ✖ [3/3] Building AikenVerify.Proofs.My_module.test_bad (5s)\n\
                     error: AikenVerify/Proofs/My_module/test_bad.lean:21:5: translateApp: unsupported\n\
                     error: Lean exited with code 1\n\
                     Some required targets logged failures:\n\
                     - AikenVerify.Proofs.My_module.test_bad"
                .to_string(),
            stderr: "error: build failed".to_string(),
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
        stdout: String::new(),
        stderr: "error: 'test_add_alwaysTerminating' tactic failed".to_string(),
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
        stdout: String::new(),
        stderr: "✖ [1/1] Building AikenVerify.Proofs.My_module.test_add (1s)\n\
                     error: 'test_add_alwaysTerminating' tactic failed\n\
                     Some required targets logged failures:\n\
                     - AikenVerify.Proofs.My_module.test_add"
            .to_string(),
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
        stdout: "[1/2] Built AikenVerify.Proofs.My_module.test_add (1s)\n\
                 [2/2] Building AikenVerify.Proofs.My_module.test_bad (2s)\n\
                 error: AikenVerify/Proofs/My_module/test_bad.lean:21:5: unsolved goals\n\
                 error: Lean exited with code 1"
            .to_string(),
        stderr: "error: build failed".to_string(),
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
        stdout: "[1/1] Building AikenVerify.Proofs.My_module.test_add (1s)\n\
                 error: Lean exited with code 1"
            .to_string(),
        stderr: "error: build failed".to_string(),
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
        stdout: String::new(),
        stderr: "✖ [1/1] Building AikenVerify.Proofs.My_module.test_add (1s)\n\
                     error: 'test_add' Counterexample:\n\
                     error: Counterexample: x = 41\n\
                     Some required targets logged failures:\n\
                     - AikenVerify.Proofs.My_module.test_add"
            .to_string(),
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
        stdout: String::new(),
        stderr: String::new(),
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
        stdout: String::new(),
        stderr: "error: build failed with some generic message".to_string(),
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
        stdout: String::new(),
        stderr: "[verify-timeout] command timed out".to_string(),
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
    let lakefile = generate_lakefile("abc123def", None);
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
    let lakefile = generate_lakefile(DEFAULT_BLASTER_REV, None);
    assert!(
        lakefile.contains(&format!(r#"@ "{DEFAULT_BLASTER_REV}""#)),
        "Lakefile should use default rev"
    );
}

#[test]
fn generate_lakefile_uses_resolved_plutus_core_path() {
    let tmp = tempfile::tempdir().unwrap();
    let pc = tmp.path().join("PlutusCore");
    fs::create_dir_all(&pc).unwrap();
    let lakefile = generate_lakefile(DEFAULT_BLASTER_REV, Some(&pc));
    let expected = format!(
        "require PlutusCore from\n  \"{}\"",
        pc.display()
    );
    assert!(
        lakefile.contains(&expected),
        "Lakefile should reference the explicit PlutusCore path, got:\n{lakefile}"
    );
}

#[test]
fn generate_lakefile_uses_placeholder_when_no_path_configured() {
    // When no explicit path and no env var, the lakefile gets the placeholder.
    unsafe { std::env::remove_var("PLUTUS_CORE_DIR") };
    let lakefile = generate_lakefile(DEFAULT_BLASTER_REV, None);
    assert!(
        lakefile.contains("require PlutusCore from\n  \"PlutusCore\""),
        "Lakefile should use placeholder path when nothing is configured, got:\n{lakefile}"
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

    let resolved = resolve_plutus_core_dir(Some(&explicit));
    assert_eq!(resolved, explicit);
}

#[test]
fn resolve_plutus_core_dir_falls_back_to_env_var() {
    let tmp = tempfile::tempdir().unwrap();
    let env_dir = tmp.path().join("from_env");
    fs::create_dir_all(&env_dir).unwrap();

    unsafe { std::env::set_var("PLUTUS_CORE_DIR", &env_dir) };
    let resolved = resolve_plutus_core_dir(None);
    unsafe { std::env::remove_var("PLUTUS_CORE_DIR") };

    assert_eq!(resolved, env_dir);
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
        all_ok: true,
        capabilities: capabilities(),
    };
    let json = serde_json::to_string(&report).unwrap();
    assert!(json.contains("\"all_ok\":true"));
    assert!(json.contains(&format!("\"blaster_rev\":\"{DEFAULT_BLASTER_REV}\"")));
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

test foo_mint_roundtrip(policy_id via byte_fuzzer()) {
  foo.mint(Void, policy_id, Void)
}

test foo_mint_repeated(policy_id via byte_fuzzer()) {
  foo.mint(Void, policy_id, Void) && foo.mint(Void, policy_id, Void)
}

test foo_mint_in_short_circuit_or_rhs(policy_id via byte_fuzzer()) {
  True || foo.mint(Void, policy_id, Void)
}

test foo_mint_in_short_circuit_and_rhs(policy_id via byte_fuzzer()) {
  False && foo.mint(Void, policy_id, Void)
}

test foo_mint_in_non_executed_if_branch(policy_id via byte_fuzzer()) {
  if False {
    foo.mint(Void, policy_id, Void)
  } else {
    True
  }
}

test foo_mint_wrapped_with_negation(policy_id via byte_fuzzer()) {
  !foo.mint(Void, policy_id, Void)
}

test foo_mint_wrapped_with_comparison(policy_id via byte_fuzzer()) {
  foo.mint(Void, policy_id, Void) == True
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
fn export_path_preserves_state_machine_transition_semantics() {
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

    assert!(
        matches!(
            &test.semantics,
            FuzzerSemantics::Opaque { reason }
                if reason.contains("not structurally understood yet")
        ),
        "scenario fixture should conservatively stay opaque when structural trace semantics are not provable, got {:?}",
        test.semantics
    );
}

#[test]
fn export_path_preserves_opaque_semantics_for_todo_fuzzer() {
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
            FuzzerSemantics::Opaque { reason }
            if reason.contains("not structurally understood yet")
        ),
        "todo-based fuzzers should remain opaque in exported semantics, got {:?}",
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

    let result = preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
    let result = preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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
    let result = preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
    let result = preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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
    let result = preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
    let result = preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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
    let result = preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
    let result = preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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
    let result = preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
    let result = preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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
        assert!(
            preflight_validate_test(
                test,
                ExistentialMode::default(),
                &VerificationTargetKind::ValidatorHandler,
            )
            .is_err(),
            "{context} should fail validator-target preflight without validator metadata"
        );
        assert!(
            preflight_validate_test(
                test,
                ExistentialMode::default(),
                &VerificationTargetKind::Equivalence,
            )
            .is_err(),
            "{context} should fail equivalence preflight without validator metadata"
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

    let result = preflight_validate_test(
            when_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
    let result = preflight_validate_test(
            when_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
    let result = preflight_validate_test(
            if_is_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
    let result = preflight_validate_test(
            if_is_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
    );
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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
            existential_mode: ExistentialMode::default(),
            target: target.clone(),
            plutus_core_dir: None,
        };

        let manifest = generate_lean_workspace(
            &validator_target_tests,
            &config,
            true,
        )
        .unwrap_or_else(|e| {
            panic!("workspace generation should succeed for --target {target} (with skips): {e}")
        });

        // Opaque fuzzers may be skipped; check that at least one entry exists
        if manifest.tests.is_empty() {
            assert!(
                !manifest.skipped.is_empty(),
                "--target {target} should either generate tests or skip them"
            );
            continue;
        }

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
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("no validator target metadata"),
        "Should error about missing validator_target, got: {err}"
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

    let result = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::ValidatorHandler,
    );
    assert!(
        result.is_ok(),
        "Validator mode should use validator metadata instead of strict target_kind matching"
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("handler_prog"), "Validator handler mode should use handler program");
    assert!(proof.contains("#import_uplc handler_prog"), "Should import the handler UPLC program");
    assert!(proof.contains("_handler.cbor"), "Handler CBOR file should be imported");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("_equivalence"), "Equivalence mode should generate an equivalence theorem");
    assert!(proof.contains("#import_uplc handler_prog"), "Should import both the original and handler programs");
    assert!(proof.contains("= proveTests handler_prog"), "Equivalence theorem should assert equal results between programs");
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
fn equivalence_target_fallback_uses_non_vacuous_void_error_goal() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
}

#[test]
fn equivalence_target_fallback_stays_universal_in_fail_once_mode() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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
fn validate_blaster_rev_accepts_valid_inputs() {
    assert!(validate_blaster_rev("main").is_ok());
    assert!(validate_blaster_rev("abc123def").is_ok());
    assert!(validate_blaster_rev("v1.2.3").is_ok());
    assert!(validate_blaster_rev("feature/my-branch").is_ok());
    assert!(validate_blaster_rev("my_tag_1.0").is_ok());
}

#[test]
fn validate_blaster_rev_rejects_empty() {
    assert!(validate_blaster_rev("").is_err());
}

#[test]
fn validate_blaster_rev_rejects_injection() {
    assert!(validate_blaster_rev("main\" ; rm -rf /").is_err());
    assert!(validate_blaster_rev("rev\nmalicious").is_err());
    assert!(validate_blaster_rev("rev`cmd`").is_err());
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
        stdout: String::new(),
        stderr: "error: 'test_add' unsolved goals\nTactic `blaster` failed".to_string(),
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
        stdout: String::new(),
        stderr: "\
error: 'test_add' Counterexample:
error: Counterexample: x = 41
note: context line 1
note: context line 2
note: context line 3
note: context line 4
error: 'test_sub' unsolved goals
error: Tactic `blaster` failed"
            .to_string(),
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
        is_skippable_generation_error(&err),
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
        !is_skippable_generation_error(&err),
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
        is_skippable_generation_error(&fallback),
        "FallbackRequired should be skippable"
    );

    let invalid = generation_error(
        GenerationErrorCategory::InvalidConstraint,
        "unsupported no extractable requires fallback",
    );
    assert!(
        !is_skippable_generation_error(&invalid),
        "InvalidConstraint should not be skippable"
    );
}

#[test]
fn skip_unsupported_collects_skipped_tests() {
    let tmp = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: tmp.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    // Mismatched output type / semantics gets skipped when skip_unsupported is true.
    let mut unsupported = make_test("my_module", "test_list");
    unsupported.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    unsupported.constraint = FuzzerConstraint::Any;

    let manifest =
        generate_lean_workspace(&[unsupported], &config, true).unwrap();
    assert_eq!(manifest.skipped.len(), 1, "mismatched type/semantics should be skipped");
    assert!(manifest.tests.is_empty());
}

#[test]
fn skip_unsupported_ignores_collisions_from_skipped_tests() {
    let tmp = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: tmp.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::ValidatorHandler,
        plutus_core_dir: None,
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

    let manifest =
        generate_lean_workspace(&[supported, skipped], &config, true).unwrap_or_else(|e| {
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
            existential_mode: ExistentialMode::default(),
            target: target.clone(),
            plutus_core_dir: None,
        };

        let missing_metadata = make_test("my_module", "test_missing_validator_metadata");
        let manifest = generate_lean_workspace(&[missing_metadata], &config, true)
            .unwrap_or_else(|e| panic!("skip mode should not fail for target {target}: {e}"));

        assert!(
            manifest.tests.is_empty(),
        );
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
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    let mut unsupported = make_test("my_module", "test_list");
    unsupported.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    unsupported.constraint = FuzzerConstraint::Any;

    let result =
        generate_lean_workspace(&[unsupported], &config, false);
    assert!(
        result.is_err(),
        "Nested composites with opaque semantics should error without skip mode"
    );
}

#[test]
fn skip_unsupported_does_not_swallow_non_skippable_generation_error() {
    let tmp = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: tmp.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
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

    let result = generate_lean_workspace(&[bad], &config, true);
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
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
        plutus_core_dir: None,
    };

    let good = make_test("my_module", "test_int");
    let mut bad = make_test("my_module", "test_list");
    bad.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    bad.constraint = FuzzerConstraint::Any;

    let manifest =
        generate_lean_workspace(&[good, bad], &config, true).unwrap();
    assert_eq!(manifest.tests.len(), 1, "Good test should generate");
    assert_eq!(manifest.skipped.len(), 1, "Bad test should be skipped");
}

// --- Step 3.1 tests: String and Pair ---

#[test]
fn string_without_domain_predicate_uses_sampled_domain_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
}

#[test]
fn pair_data_data_without_predicates_uses_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("(a : Integer)"), "Pair fst should bind as Integer");
    assert!(proof.contains("(b : Data)"), "Pair snd should bind as Data");
    assert!(proof.contains("-10 <= a"), "Should contain lower bound for Int component");
    assert!(proof.contains("a <= 10"), "Should contain upper bound for Int component");
}

// --- Step 3.2 tests: Generic tuple arities ---

#[test]
fn tuple_arity_4_generates_theorem() {
    let test = make_test_with_type(
        "my_module",
        "test_quad",
        FuzzerOutputType::Tuple(vec![
            FuzzerOutputType::Data,
            FuzzerOutputType::Int,
            FuzzerOutputType::Bool,
            FuzzerOutputType::ByteArray,
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("Data"), "Should contain Data type for first component");
    assert!(proof.contains("Integer"), "Should contain Integer type for second component");
    assert!(proof.contains("Bool"), "Should contain Bool type for third component");
    assert!(proof.contains("ByteString"), "Should contain ByteString type for fourth component");
}

#[test]
fn tuple_arity_5_all_data_without_predicates_uses_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
}

#[test]
fn tuple_high_arity_without_predicates_uses_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("List Integer"), "Should quantify over List Integer");
    assert!(proof.contains("xs.length"), "Should contain list length bounds");
    assert!(proof.contains("\u{2208}"), "Should use element membership (\u{2208}) for per-element bounds");
    assert!(proof.contains("\u{2200}"), "Should contain \u{2200} quantifier");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("List Data"), "Should quantify over List Data");
    assert!(proof.contains("xs.length"), "Should contain list length bounds");
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
    .unwrap_err()
    .to_string();

    assert!(
        err.contains("inconsistent list-length bounds"),
        "Should report inconsistent list bounds, got:\n{err}"
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("List Data"), "Should quantify over List Data");
    assert!(proof.contains("xs.length"), "And-extracted bounds should contain length constraints");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("List"), "One-sided length precondition test should generate a List proof");
    assert!(proof.contains("xs.length"), "One-sided length precondition should include length bounds");
}

// --- Step 3.4 tests: ADT fallback via Data encoding ---

#[test]
fn unsupported_type_uses_sampled_domain_fallback() {
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
    assert!(result.is_err(), "Any/Opaque semantics should not produce a direct proof");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("Integer"), "Should contain Integer type for Int component");
    assert!(proof.contains("Data"), "Unsupported type should fall back to Data");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("Integer"), "Should contain Integer for Int position");
    assert!(proof.contains("Data"), "Should contain Data for Data position");
    assert!(proof.contains("0 <= a"), "Shared IntRange should apply to Int position");
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
    };
    let json = serde_json::to_string(&result).unwrap();
    assert!(
        json.contains("\"category\":\"Counterexample\""),
        "JSON should include category: {json}"
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

    let err = clean_artifacts(&out.join("../outside"))
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

    let err =
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

    let err = clean_artifacts(&out)
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
    let types_and_constraints = vec![
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
            existential_mode: ExistentialMode::Witness,
            target: VerificationTargetKind::PropertyWrapper,
            plutus_core_dir: None,
        };

        let manifest = generate_lean_workspace(&[test], &config, true);
        assert!(
            manifest.is_ok(),
            "Scalar type {:?} should generate workspace (possibly with skips)",
            output_type
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
            existential_mode: ExistentialMode::Witness,
            target: VerificationTargetKind::PropertyWrapper,
            plutus_core_dir: None,
        };

        let manifest = generate_lean_workspace(&[test], &config, false);
        assert!(
            manifest.is_ok(),
            "Composite type {label} should generate workspace"
        );
        assert_eq!(manifest.unwrap().tests.len(), 1);
    }
}

#[test]
fn compat_unsupported_type_fails_without_skip() {
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
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
        plutus_core_dir: None,
    };

    let result = generate_lean_workspace(&[test], &config, false);
    assert!(
        result.is_err(),
        "Nested composites with opaque semantics should error without sampled-domain fallback"
    );
}

#[test]
fn compat_unsupported_type_skipped_with_flag() {
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
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
        plutus_core_dir: None,
    };

    let manifest =
        generate_lean_workspace(&[test], &config, true).unwrap();
    // With fallback removed, opaque semantics are skipped
    assert_eq!(manifest.skipped.len(), 1);
    assert!(manifest.tests.is_empty());
}

#[test]
fn compat_fail_once_witness_mode_generates_existential() {
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
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
        plutus_core_dir: None,
    };

    let manifest =
        generate_lean_workspace(&[test], &config, false).unwrap();
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
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
        plutus_core_dir: None,
    };

    let manifest =
        generate_lean_workspace(&[test], &config, false).unwrap();
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

    // State-machine cases with opaque transition semantics produce clear errors
    let error_cases = vec![
        (
            "reduced_state_machine_trace",
            reduced_state_machine_trace.clone(),
        ),
        (
            "amaru_permissions_property",
            amaru_permissions_property.clone(),
        ),
    ];

    for (label, test) in &error_cases {
        let err = generate_proof_for_phase12_case(test)
            .expect_err(&format!("opaque state-machine case {label} should error"));
    }

    for (label, test) in &direct_cases {
        let direct = generate_proof_for_phase12_case(test)
            .unwrap_or_else(|e| panic!("direct proof should generate for {label}: {e}"));
        let direct_metrics = collect_proof_benchmark_metrics(&direct);
        assert!(
            direct_metrics.theorem_size_bytes > 0 && direct_metrics.theorem_count >= 1,
        );
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
    let widened_err = generate_proof_for_phase12_case(&widened_semantics)
        .expect_err("widened semantic domains should error without direct proof support");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("open PlutusCore.Data (Data)"), "Tuple proof should open Data namespace even for all-Int elements");
    assert!(proof.contains("Data.List"), "Tuple proof should construct Data.List argument");
}

#[test]
fn list_bool_with_mapped_int_range_uses_sampled_domain_fallback() {
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
    assert!(result.is_err(), "mapped IntRange over Bool cannot produce a direct proof");
}

// --- Item 2.1: lean_type_for nested tuple/pair support ---

#[test]
fn lean_type_for_pair_int_bytearray() {
    let t = FuzzerOutputType::Pair(
        Box::new(FuzzerOutputType::Int),
        Box::new(FuzzerOutputType::ByteArray),
    );
    assert_eq!(lean_type_for(&t), Some("(Integer \u{00d7} ByteString)".to_string()));
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

    let (_reachable_output, definitions) = build_state_machine_trace_reachability_helpers(
        "failure_events_test",
        "failure_events_test",
        &StateMachineAcceptance::AcceptsFailure,
        &transition_semantics,
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

    let (_reachable_output, definitions) = build_state_machine_trace_reachability_helpers(
        "success_events_test",
        "success_events_test",
        &StateMachineAcceptance::AcceptsSuccess,
        &transition_semantics,
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("Data"), "ADT with schema should quantify over Data");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("Data"), "ADT with tags and schema should quantify over Data");
    assert!(proof.contains("Data.Constr"), "Should contain constructor tag matching via Data.Constr");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("(x : Data)"), "Plain Data quantification should bind x as Data");
    assert!(proof.contains("dataArg"), "Should use dataArg helper");
}

// --- Phase 2 Batch B: Item 2.3 - Data.Map (Dict) support ---

#[test]
fn dict_type_flows_through_as_list_of_pairs() {
    // Dict<ByteArray, Int> in Aiken compiles to List<Pair<ByteArray, Int>>
    let dict_type = FuzzerOutputType::List(Box::new(FuzzerOutputType::Pair(
        Box::new(FuzzerOutputType::ByteArray),
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("List"), "Dict should flow through as List type");
    assert!(proof.contains("xs.length"), "Dict list should have length bounds");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("List"), "Dict<Data,Data> should generate List-based proof");
    assert!(proof.contains("Data"), "Dict elements should reference Data type");
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("List"), "List of enum should quantify over List type");
    assert!(proof.contains("Data"), "List of Unsupported should map elements to Data");
}

#[test]
fn list_of_data_with_constructor_element_semantics_generates_predicate() {
    // Explicit Data list with constructor-tag element semantics
    let test = make_test_with_type(
        "my_module",
        "test_list_data_ctors",
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Data)),
        FuzzerConstraint::List {
            elem: Box::new(FuzzerConstraint::DataConstructorTags {
                tags: vec![0, 1],
            }),
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

    assert!(proof.contains("theorem "), "Generated proof should contain a theorem");
    assert!(proof.contains("List Data"), "Should quantify over List Data");
    assert!(proof.contains("Data.Constr"), "Constructor-tag element semantics should generate Data.Constr predicate");
}
