use super::*;
use crate::export::{
    ExportedProgram, ExportedPropertyTest, FuzzerConstraint, FuzzerOutputType, TestReturnMode,
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
        fuzzer_output_type: FuzzerOutputType::Int,
        constraint: FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "255".to_string(),
        },
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
    assert!(out_dir.join(format!("flat/{id0}.flat")).exists());
    assert!(out_dir.join(format!("flat/{id0}_fuzzer.flat")).exists());
    assert!(out_dir.join(format!("flat/{id1}.flat")).exists());
    assert!(out_dir.join(format!("flat/{id1}_fuzzer.flat")).exists());

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
    let flat_content = fs::read_to_string(out_dir.join(format!("flat/{id0}.flat"))).unwrap();
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
    assert!(proof1.contains("theorem test_roundtrip"));
    assert!(proof1.contains("theorem test_roundtrip_alwaysTerminating"));
    assert!(proof1.contains("by blaster"));
    assert!(proof1.contains(&format!("#import_uplc prog_{id0} single_cbor_hex")));
    assert!(
        !proof1.contains(&format!("#import_uplc fuzzer_prog_{id0} single_cbor_hex")),
        "Direct-domain theorem should not import fuzzer flat program, got:\n{proof1}"
    );
    assert!(proof1.contains("(0 <= x && x <= 255)"));
    assert!(proof1.contains("namespace AikenVerify.Proofs.My_module.test_roundtrip"));

    let proof2 =
        fs::read_to_string(out_dir.join("AikenVerify/Proofs/AikenList/test_map.lean")).unwrap();
    assert!(proof2.contains("theorem test_map"));
    assert!(proof2.contains("theorem test_map_alwaysTerminating"));
    assert!(proof2.contains(&format!("#import_uplc prog_{id1} single_cbor_hex")));
    assert!(
        !proof2.contains(&format!("#import_uplc fuzzer_prog_{id1} single_cbor_hex")),
        "Direct-domain theorem should not import fuzzer flat program, got:\n{proof2}"
    );
    assert!(proof2.contains(&format!("./flat/{id1}.flat")));
    assert!(proof2.contains("(0 <= x && x <= 255)"));
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
        "Generated proof file should exist at manifest path"
    );
    assert!(
        !entry.id.contains('.'),
        "Manifest IDs are embedded into Lean identifiers and must not contain dots"
    );

    let proof = fs::read_to_string(out_dir.join(&entry.lean_file)).unwrap();
    assert!(
        proof.contains(&format!("#import_uplc prog_{} single_cbor_hex", entry.id)),
        "Proof should reference a sanitized `prog_...` Lean identifier"
    );
    assert!(
        !proof.contains(&format!(
            "#import_uplc fuzzer_prog_{} single_cbor_hex",
            entry.id
        )),
        "Direct-domain proof should not import `fuzzer_prog_...`"
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
fn generate_workspace_records_sampled_fallback_reasons() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path().to_path_buf();
    let tests = vec![make_test_with_type(
        "my_module",
        "test_bool_fallback_manifest",
        FuzzerOutputType::Bool,
        FuzzerConstraint::Any,
    )];

    let config = VerifyConfig {
        out_dir,
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
    };

    let manifest = generate_lean_workspace(&tests, &config, false).unwrap();
    assert_eq!(manifest.fallbacks.len(), 1);
    assert_eq!(
        manifest.fallbacks[0].name,
        "my_module.test_bool_fallback_manifest"
    );
    assert!(
        manifest.fallbacks[0]
            .reason
            .contains("no extractable scalar-domain predicates"),
        "Expected explicit fallback reason, got: {:?}",
        manifest.fallbacks[0]
    );
}

#[test]
fn generate_workspace_force_sampled_fallback_marks_all_tests() {
    let tmp = tempfile::tempdir().unwrap();
    let out_dir = tmp.path().to_path_buf();
    let tests = vec![
        make_test_with_type(
            "my_module",
            "test_int_direct_domain",
            FuzzerOutputType::Int,
            FuzzerConstraint::IntRange {
                min: "0".to_string(),
                max: "10".to_string(),
            },
        ),
        make_test_with_type(
            "my_module",
            "test_bytes_direct_domain",
            FuzzerOutputType::ByteArray,
            FuzzerConstraint::ByteStringLenRange {
                min_len: 1,
                max_len: 2,
            },
        ),
    ];

    let config = VerifyConfig {
        out_dir,
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        target: VerificationTargetKind::default(),
    };

    let manifest = generate_lean_workspace_with_options(
        &tests,
        &config,
        false,
        ProofGenerationOptions {
            force_sampled_fallback: true,
        },
    )
    .unwrap();

    assert_eq!(manifest.tests.len(), 2);
    assert_eq!(manifest.fallbacks.len(), 2);
    assert!(
        manifest
            .fallbacks
            .iter()
            .all(|entry| entry.reason.contains("--force-sampled-fallback"))
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
    fs::create_dir_all(out_dir.join("flat")).unwrap();
    fs::write(out_dir.join("flat/stale.flat"), "stale").unwrap();
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
    };

    let manifest =
        generate_lean_workspace(&[make_test("my_module", "test_roundtrip")], &config, false)
            .unwrap();
    let id = &manifest.tests[0].id;

    assert!(!out_dir.join("AikenVerify/Proofs/Stale/old.lean").exists());
    assert!(!out_dir.join("flat/stale.flat").exists());
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
    assert!(out_dir.join(format!("flat/{id}.flat")).exists());
    assert!(out_dir.join(format!("flat/{id}_fuzzer.flat")).exists());
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
        proof.contains(&format!("(proveTests prog_{id} (intArg x)) = true")),
        "FailImmediately (default) should generate = true assertion, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!("Option.isSome (proveTests prog_{id} (intArg x))")),
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
        proof.contains(&format!("(proveTests prog_{id} (intArg x)) = false")),
        "SucceedEventually (fail keyword) should generate = false assertion, got:\n{proof}"
    );
    assert!(
        !proof.contains("= true"),
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

    assert!(
        proof.contains("∃ (x : Integer),"),
        "Witness mode should use existential quantifier, got:\n{proof}"
    );
    assert!(
        proof.contains("witnessTests"),
        "Witness mode should use witnessTests, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!("witnessTests prog_{id} (intArg x) false")),
        "Fail-once witness theorem should search for a falsifying input (= false), got:\n{proof}"
    );
    assert!(
        proof.contains("∧"),
        "Existential should use ∧ (conjunction), not → (implication), got:\n{proof}"
    );
    assert!(
        proof.contains("⟨(0 : Integer), by decide⟩"),
        "Witness mode should provide concrete witness with decide tactic, got:\n{proof}"
    );
    assert!(
        proof.contains("-- Mode: witness"),
        "Should have mode comment, got:\n{proof}"
    );
    assert!(
        !proof.contains("alwaysTerminating"),
        "Existential theorems should not have termination theorem, got:\n{proof}"
    );
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

    assert!(
        proof.contains("∃ (x : Integer),"),
        "Proof mode should use existential quantifier, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!("(proveTests prog_{id} (intArg x)) = false")),
        "Fail-once proof theorem should assert existence of a falsifying input (= false), got:\n{proof}"
    );
    assert!(
        !proof.contains("= true"),
        "Fail-once proof theorem should not assert success witnesses (= true), got:\n{proof}"
    );
    assert!(
        proof.contains("∧"),
        "Existential should use ∧ (conjunction), not → (implication), got:\n{proof}"
    );
    assert!(
        proof.contains("by blaster"),
        "Proof mode should use blaster tactic, got:\n{proof}"
    );
    assert!(
        !proof.contains("⟨"),
        "Proof mode should not provide concrete witness, got:\n{proof}"
    );
    assert!(
        proof.contains("-- Mode: proof"),
        "Should have mode comment, got:\n{proof}"
    );
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

    assert!(
        proof.contains("∃ (x : Integer),"),
        "Void witness should use existential quantifier, got:\n{proof}"
    );
    assert!(
        proof.contains("proveTestsError"),
        "Void witness should use proveTestsError, got:\n{proof}"
    );
    assert!(
        proof.contains("∧"),
        "Existential should use ∧ (conjunction), got:\n{proof}"
    );
    assert!(
        proof.contains("⟨(0 : Integer), by decide⟩"),
        "Void witness should provide concrete witness, got:\n{proof}"
    );
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

    assert!(
        proof.contains("∃ (a : Integer) (b : Integer),"),
        "Should use existential quantifier for tuple, got:\n{proof}"
    );
    // All connectors must be ∧, no → anywhere in the theorem
    assert!(
        !proof.contains("→"),
        "Existential tuple theorem must not contain → (implication), got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= a && a <= 10)"),
        "First element bounds, got:\n{proof}"
    );
    assert!(
        proof.contains("(20 <= b && b <= 30)"),
        "Second element bounds, got:\n{proof}"
    );
    assert!(
        proof.contains("∧"),
        "All connectors must be ∧ (conjunction), got:\n{proof}"
    );
    assert!(
        proof.contains("⟨(0 : Integer), (20 : Integer), by decide⟩"),
        "Witness mode should provide tuple witness, got:\n{proof}"
    );
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("∃ (seed : Data),"),
        "Data fail-once witness should use existential seed quantifier in sampled fallback, got:\n{proof}"
    );
    assert!(
        proof.contains("-- Mode: witness (sampled-domain fallback auto-escalated to proof)"),
        "Sampled-domain witness mode should document auto-escalation, got:\n{proof}"
    );
    assert!(
        proof.contains("by blaster"),
        "Sampled-domain existential witness mode should escalate to proof search, got:\n{proof}"
    );
    assert!(
        !proof.contains("refine ⟨Data.I 0, ?_⟩"),
        "Sampled-domain existential witness mode should not hardcode a single seed witness, got:\n{proof}"
    );
    assert!(
        !proof.contains("Data.Integer"),
        "Data fail-once witness should not use invalid Data.Integer constructor, got:\n{proof}"
    );
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("∃ (seed : Data),"),
        "Unsupported fail-once witness should use existential seed quantifier, got:\n{proof}"
    );
    assert!(
        proof.contains("-- Mode: witness (sampled-domain fallback auto-escalated to proof)"),
        "Sampled-domain witness mode should document auto-escalation, got:\n{proof}"
    );
    assert!(
        proof.contains("by blaster"),
        "Sampled-domain existential witness mode should escalate to proof search, got:\n{proof}"
    );
    assert!(
        !proof.contains("refine ⟨Data.I 0, ?_⟩"),
        "Sampled-domain existential witness mode should not hardcode a single seed witness, got:\n{proof}"
    );
    assert!(
        !proof.contains("Data.Integer"),
        "Unsupported fail-once witness should not use invalid Data.Integer constructor, got:\n{proof}"
    );
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

    assert!(
        proof.contains("∃ (a : Integer) (b : Data),"),
        "Tuple fail-once witness should quantify Integer and Data, got:\n{proof}"
    );
    assert!(
        proof.contains("⟨(0 : Integer), Data.I 0, by decide⟩"),
        "Tuple fail-once witness should use Data.I constructor for Data elements, got:\n{proof}"
    );
    assert!(
        !proof.contains("Data.Integer"),
        "Tuple fail-once witness should not use invalid Data.Integer constructor, got:\n{proof}"
    );
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

    assert!(
        proof.contains("∃ (xs : List Data),"),
        "List<Data> fail-once witness should quantify over List Data, got:\n{proof}"
    );
    assert!(
        proof.contains("⟨[Data.I 0, Data.I 0], by decide⟩"),
        "List<Data> fail-once witness should use Data.I constructor for elements, got:\n{proof}"
    );
    assert!(
        !proof.contains("Data.Integer"),
        "List<Data> fail-once witness should not use invalid Data.Integer constructor, got:\n{proof}"
    );
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

    assert!(
        proof.contains("∃ (xs : List Bool),"),
        "List<Bool> fail-once witness should quantify over List Bool, got:\n{proof}"
    );
    assert!(
        proof.contains("(xs.all (fun x_i => x_i = false))"),
        "List<Bool> exact predicate should be emitted in preconditions, got:\n{proof}"
    );
    assert!(
        proof.contains("⟨[false], by decide⟩"),
        "List<Bool> fail-once witness should satisfy exact element constraints, got:\n{proof}"
    );
    assert!(
        !proof.contains("⟨[true], by decide⟩"),
        "List<Bool> fail-once witness must not use default true when exact false is required, got:\n{proof}"
    );
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

    assert!(
        proof.contains("proveTestsHalt"),
        "Void mode should use proveTestsHalt, got:\n{proof}"
    );
    assert!(
        !proof.contains("proveTests prog_"),
        "Void mode should not use proveTests (Bool path), got:\n{proof}"
    );
    assert!(
        !proof.contains("alwaysTerminating"),
        "Void mode should not generate separate termination theorem (halt implies it), got:\n{proof}"
    );
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

    assert!(
        proof.contains("proveTestsError"),
        "Void+fail mode should use proveTestsError, got:\n{proof}"
    );
    assert!(
        !proof.contains("= true") && !proof.contains("= false"),
        "Void+fail mode should not contain = true or = false, got:\n{proof}"
    );
    assert!(
        !proof.contains("alwaysTerminating"),
        "Void+fail mode should not generate separate termination theorem, got:\n{proof}"
    );
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

    assert!(
        proof.contains("= true"),
        "Bool mode should contain = true, got:\n{proof}"
    );
    assert!(
        proof.contains("alwaysTerminating"),
        "Bool mode should generate termination theorem, got:\n{proof}"
    );
    assert!(
        proof.contains("Option.isSome"),
        "Bool mode should use Option.isSome for termination, got:\n{proof}"
    );
}

fn make_test_with_type(
    module: &str,
    name: &str,
    fuzzer_output_type: FuzzerOutputType,
    constraint: FuzzerConstraint,
) -> ExportedPropertyTest {
    let module_path = module.replace('.', "/");
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
        proof.contains("∀ (seed : Data),"),
        "Bool fallback theorem should quantify over seed, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!(
            "match sampleFuzzerValue fuzzer_prog_{id} seed with"
        )),
        "Bool fallback theorem should sample the exported fuzzer domain, got:\n{proof}"
    );
    assert!(
        proof.contains("-- sampled-domain fallback reason:"),
        "Fallback proofs should include an explicit fallback reason comment, got:\n{proof}"
    );
    assert!(
        proof.contains("theorem test_bool_alwaysTerminating"),
        "Should have termination theorem, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!("Option.isSome (proveTests prog_{id} (dataArg x))")),
        "Termination theorem should evaluate sampled value x via dataArg, got:\n{proof}"
    );
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
        proof.contains(&format!("#import_uplc fuzzer_prog_{id} single_cbor_hex")),
        "Sampled fallback must import fuzzer flat program, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!(
            "match sampleFuzzerValue fuzzer_prog_{id} seed with"
        )),
        "Sampled fallback theorem should reference sampled fuzzer domain, got:\n{proof}"
    );
}

#[test]
fn force_sampled_fallback_overrides_direct_constraint_translation() {
    let test = make_test_with_type(
        "my_module",
        "test_forced_fallback_overrides_direct",
        FuzzerOutputType::Int,
        FuzzerConstraint::IntRange {
            min: "0".to_string(),
            max: "10".to_string(),
        },
    );
    let id = test_id("my_module", "test_forced_fallback_overrides_direct");
    let lean_name = sanitize_lean_name("test_forced_fallback_overrides_direct");
    let lean_module = "AikenVerify.Proofs.My_module.test_forced_fallback_overrides_direct";

    let proof = generate_proof_file_with_options(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
        ProofGenerationOptions {
            force_sampled_fallback: true,
        },
    )
    .unwrap();

    assert!(
        proof.contains(&format!("#import_uplc fuzzer_prog_{id} single_cbor_hex")),
        "Forced sampled fallback must import fuzzer flat program, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!(
            "match sampleFuzzerValue fuzzer_prog_{id} seed with"
        )),
        "Forced sampled fallback should sample fuzzer domain, got:\n{proof}"
    );
    assert!(
        proof.contains("-- sampled-domain fallback reason: forced sampled-domain fallback mode enabled (--force-sampled-fallback)"),
        "Forced sampled fallback should expose explicit reason, got:\n{proof}"
    );
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
        proof.contains("(dataArg x)) = false"),
        "SucceedEventually (fail keyword) Bool fallback should assert = false on sampled x, got:\n{proof}"
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
        proof.contains("∀ (a : Integer) (b : Integer),"),
        "Tuple theorem should quantify over two Integers, got:\n{proof}"
    );
    assert!(
        proof.contains("(1 <= a && a <= 10)"),
        "Should have bounds for a, got:\n{proof}"
    );
    assert!(
        proof.contains("(1 <= b && b <= 10)"),
        "Should have bounds for b, got:\n{proof}"
    );
    assert!(
        proof.contains("Data.List [Data.I a, Data.I b]"),
        "Should inline tuple encoding, got:\n{proof}"
    );
    assert!(
        proof.contains("= true"),
        "Should have correctness assertion, got:\n{proof}"
    );
    assert!(
        proof.contains("theorem test_pair_alwaysTerminating"),
        "Termination theorem should be present, got:\n{proof}"
    );
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

    assert!(
        proof.contains("∀ (a : Integer) (b : Integer),"),
        "Tuple theorem should quantify over two Integers, got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= a && a <= 10)"),
        "Should have first component bounds for a, got:\n{proof}"
    );
    assert!(
        proof.contains("(20 <= b && b <= 30)"),
        "Should have second component bounds for b, got:\n{proof}"
    );
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
        proof.contains("match sampleFuzzerValue"),
        "Wrong-arity tuple constraints should route to sampled-domain fallback, got:\n{proof}"
    );
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
        proof.contains("match sampleFuzzerValue"),
        "Unknown tuple bounds should route to sampled-domain fallback, got:\n{proof}"
    );
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
    assert!(result.is_ok());
    let proof = result.unwrap();
    assert!(
        proof.contains("∀ (seed : Data),"),
        "ByteArray fallback theorem should quantify over seed, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!(
            "match sampleFuzzerValue fuzzer_prog_{id} seed with"
        )),
        "ByteArray fallback theorem should sample fuzzer outputs, got:\n{proof}"
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

    assert!(
        !proof.contains("match sampleFuzzerValue"),
        "Bounded ByteArray should stay in direct scalar theorem path, got:\n{proof}"
    );
    assert!(
        proof.contains("(1 <= x.length && x.length <= 3)"),
        "Expected ByteArray length-domain precondition, got:\n{proof}"
    );
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

    assert!(
        !proof.contains("match sampleFuzzerValue"),
        "Exact non-empty ByteArray should stay in direct scalar theorem path, got:\n{proof}"
    );
    assert!(
        proof.contains("PlutusCore.ByteString.consByteStringV1 102"),
        "Expected emitted ByteString literal for non-empty bytes, got:\n{proof}"
    );
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

    assert!(
        !proof.contains("match sampleFuzzerValue"),
        "Bounded String should stay in direct scalar theorem path, got:\n{proof}"
    );
    assert!(
        proof.contains("(2 <= x.length && x.length <= 5)"),
        "Expected String length-domain precondition, got:\n{proof}"
    );
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

    assert!(
        !proof.contains("match sampleFuzzerValue"),
        "Exact non-empty String should stay in direct scalar theorem path, got:\n{proof}"
    );
    assert!(
        proof.contains("PlutusCore.ByteString.consByteStringV1 72"),
        "Expected UTF-8 ByteString literal emission for String exact value, got:\n{proof}"
    );
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
        proof.contains("∀ (seed : Data),"),
        "Fallback theorem should quantify over seed, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!(
            "match sampleFuzzerValue fuzzer_prog_{id} seed with"
        )),
        "Fallback theorem should branch on sampled fuzzer value, got:\n{proof}"
    );
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
    assert!(result.is_ok());
    let proof = result.unwrap();
    assert!(
        proof.contains("match sampleFuzzerValue"),
        "Should use sampled-domain fallback theorem, got:\n{proof}"
    );
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
    assert!(result.is_ok());
    let proof = result.unwrap();
    assert!(
        proof.contains(&format!(
            "match sampleFuzzerValue fuzzer_prog_{id} seed with"
        )),
        "Unsupported constraints should use sampled-domain fallback theorem, got:\n{proof}"
    );
}

#[test]
fn list_data_with_zero_lower_bound_generates_direct_theorem() {
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
    .unwrap();

    assert!(
        proof.contains("∀ (xs : List Data),"),
        "List<Data>-like scenario domains should be quantified directly, got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= xs.length)"),
        "List lower-bound predicate should be preserved, got:\n{proof}"
    );
    assert!(
        !proof.contains("match sampleFuzzerValue"),
        "Explicit list domains should avoid sampled fallback, got:\n{proof}"
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
        proof.contains("∀ (x : Data),"),
        "Finite ADT domains should quantify directly over Data, got:\n{proof}"
    );
    assert!(
        proof.contains("((x = Data.Constr 0 []) || (x = Data.Constr 1 []))"),
        "Constructor tag disjunction should be emitted as domain precondition, got:\n{proof}"
    );
    assert!(
        !proof.contains("match sampleFuzzerValue"),
        "Finite ADT domains should avoid sampled-domain fallback, got:\n{proof}"
    );
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
        proof.contains(&format!(
            "match sampleFuzzerValue fuzzer_prog_{id} seed with"
        )),
        "Unsupported scalar constraints should use sampled-domain fallback theorem, got:\n{proof}"
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
        proof.contains("∀ (x : Integer),"),
        "Universal mode should keep direct Int quantification when salvageable, got:\n{proof}"
    );
    assert!(
        proof.contains("(1 <= x && x <= 9)"),
        "Supported IntRange fragment should be preserved, got:\n{proof}"
    );
    assert!(
        !proof.contains("match sampleFuzzerValue"),
        "Universal mode with salvageable constraints should not force sampled fallback, got:\n{proof}"
    );
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::Witness,
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(
        proof.contains("∃ (seed : Data),"),
        "Existential mode should fall back to sampled-domain seed quantification, got:\n{proof}"
    );
    assert!(
        proof.contains("match sampleFuzzerValue"),
        "Existential mode with unsupported fragments must use sampled fallback, got:\n{proof}"
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
    .unwrap();
    assert!(
        proof.contains("∀ (seed : Data),"),
        "Fallback theorem should quantify over seed only, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!(
            "match sampleFuzzerValue fuzzer_prog_{id} seed with"
        )),
        "Fallback theorem should branch on sampled fuzzer value, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!(
            "| some x => (proveTests prog_{id} (dataArg x)) = true"
        )),
        "Fallback theorem should evaluate property on sampled x, got:\n{proof}"
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
    .unwrap();
    assert!(
        proof.contains(&format!(
            "match sampleFuzzerValue fuzzer_prog_{id} seed with"
        )),
        "Fallback theorem should branch on sampled fuzzer value, got:\n{proof}"
    );
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
    assert!(result.is_ok());
    let proof = result.unwrap();
    assert!(
        proof.contains("∃ (seed : Data),"),
        "Fallback existential theorem should quantify existentially over seed, got:\n{proof}"
    );
    assert!(
        proof.contains("-- Mode: witness (sampled-domain fallback auto-escalated to proof)"),
        "Sampled-domain witness mode should document auto-escalation, got:\n{proof}"
    );
    assert!(
        proof.contains("by blaster"),
        "Sampled-domain existential witness mode should escalate to proof search, got:\n{proof}"
    );
    assert!(
        !proof.contains("refine ⟨Data.I 0, ?_⟩"),
        "Sampled-domain existential witness mode should not hardcode a single seed witness, got:\n{proof}"
    );
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("∀ (seed : Data),"));
    assert!(proof.contains(&format!(
        "match sampleFuzzerValue fuzzer_prog_{id} seed with"
    )));
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("match sampleFuzzerValue"));
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("match sampleFuzzerValue"));
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
        proof.contains("∀ (a : Integer) (b : Bool),"),
        "Mixed tuple should quantify over Integer and Bool, got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= a && a <= 100)"),
        "Should have bounds for Int element, got:\n{proof}"
    );
    assert!(
        proof.contains("Data.List [Data.I a, (Data.Constr (if b then 1 else 0) [])]"),
        "Should inline mixed encoding, got:\n{proof}"
    );
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
        proof.contains("match sampleFuzzerValue"),
        "Nested tuple elements should use sampled-domain fallback, got:\n{proof}"
    );
}

#[test]
fn tuple_of_lists_with_explicit_domains_generates_direct_theorem() {
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
    .unwrap();

    assert!(
        proof.contains("∀ (a : List Data) (b : List Data),"),
        "Tuple(List<Data>, List<Data>) should be quantified directly, got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= a.length)"),
        "First list lower bound should be preserved, got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= b.length)"),
        "Second list lower bound should be preserved, got:\n{proof}"
    );
    assert!(
        proof.contains(
            "Data.List [Data.List (a.map (fun x_0 => x_0)), Data.List (b.map (fun x_0 => x_0))]"
        ),
        "Tuple list encoding should be inlined as Data.List payloads, got:\n{proof}"
    );
    assert!(
        !proof.contains("match sampleFuzzerValue"),
        "Explicit tuple/list domains should avoid sampled-domain fallback, got:\n{proof}"
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
    assert_eq!(ExistentialMode::Proof.to_string(), "proof");
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

    // Each element is independently bounded (Cartesian product, no cross-element constraints)
    assert!(
        proof.contains("∀ (a : Integer) (b : Integer),"),
        "map2 decomposition should produce independent quantifiers, got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= a && a <= 10)"),
        "First element bounds preserved independently, got:\n{proof}"
    );
    assert!(
        proof.contains("(20 <= b && b <= 30)"),
        "Second element bounds preserved independently, got:\n{proof}"
    );
    // Verify the preconditions use → (implication) connecting them, not ∧
    // This shows they are independent constraints joined as implications
    assert!(
        proof.contains("→"),
        "Element bounds should be connected with →, got:\n{proof}"
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
    // Int with Map(IntRange) still needs bounds since Map describes input-domain.
    assert!(requires_explicit_bounds(
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
                flat_file: format!("flat/{}.flat", test_id(module, name)),
                has_termination_theorem,
            })
            .collect(),
        skipped: Vec::new(),
        fallbacks: Vec::new(),
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
        flat_file: "flat/my_module__test_eq_deadbeef.flat".to_string(),
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
        flat_file: "flat/my_module__test_eq_deadbeef.flat".to_string(),
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
    let lakefile = generate_lakefile("abc123def");
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
    let lakefile = generate_lakefile(DEFAULT_BLASTER_REV);
    assert!(
        lakefile.contains(&format!(r#"@ "{DEFAULT_BLASTER_REV}""#)),
        "Lakefile should use default rev"
    );
}

#[test]
fn generate_lakefile_uses_hardcoded_plutus_core_path() {
    let lakefile = generate_lakefile(DEFAULT_BLASTER_REV);
    let expected = format!(
        "require PlutusCore from\n  \"{}\"",
        plutus_core_dir().display()
    );
    assert!(
        lakefile.contains(&expected),
        "Lakefile should reference hard-coded PlutusCore path, got:\n{lakefile}"
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
fn resolve_plutus_core_dir_prefers_fallback_when_preferred_has_known_bug() {
    let tmp = tempfile::tempdir().unwrap();
    let preferred = tmp.path().join("preferred");
    let fallback = tmp.path().join("fallback");

    fs::create_dir_all(preferred.join("PlutusCore/Data")).unwrap();
    fs::create_dir_all(fallback.join("PlutusCore/Data")).unwrap();
    fs::write(
        preferred.join("PlutusCore/Data/Basic.lean"),
        "def dataPairsType := mkApp2 (.const ``Prod.mk [.zero, .zero]) (.const ``Data []) (.const ``Data [])\n",
    )
    .unwrap();
    fs::write(
        fallback.join("PlutusCore/Data/Basic.lean"),
        "def dataPairsType := mkApp2 (.const ``Prod [.zero, .zero]) (.const ``Data []) (.const ``Data [])\n",
    )
    .unwrap();

    let resolved = resolve_plutus_core_dir(preferred.clone(), fallback.clone());
    assert_eq!(resolved, fallback);
}

#[test]
fn resolve_plutus_core_dir_keeps_preferred_when_no_bug() {
    let tmp = tempfile::tempdir().unwrap();
    let preferred = tmp.path().join("preferred");
    let fallback = tmp.path().join("fallback");

    fs::create_dir_all(preferred.join("PlutusCore/Data")).unwrap();
    fs::create_dir_all(fallback.join("PlutusCore/Data")).unwrap();
    fs::write(
        preferred.join("PlutusCore/Data/Basic.lean"),
        "def dataPairsType := mkApp2 (.const ``Prod [.zero, .zero]) (.const ``Data []) (.const ``Data [])\n",
    )
    .unwrap();
    fs::write(
        fallback.join("PlutusCore/Data/Basic.lean"),
        "def dataPairsType := mkApp2 (.const ``Prod [.zero, .zero]) (.const ``Data []) (.const ``Data [])\n",
    )
    .unwrap();

    let resolved = resolve_plutus_core_dir(preferred.clone(), fallback);
    assert_eq!(resolved, preferred);
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

    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        )
        .is_ok(),
        "validator target preflight should succeed for exported fixture"
    );
    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        )
        .is_ok(),
        "equivalence target preflight should succeed for exported fixture"
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

    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        )
        .is_ok(),
        "validator target preflight should succeed for cross-module fixture"
    );
    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        )
        .is_ok(),
        "equivalence target preflight should succeed for cross-module fixture"
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
    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        )
        .is_ok(),
        "validator target preflight should succeed for helper-routed fixture"
    );
    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        )
        .is_ok(),
        "equivalence target preflight should succeed for helper-routed fixture"
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
    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        )
        .is_ok(),
        "validator target preflight should succeed for local-alias-routed fixture"
    );
    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        )
        .is_ok(),
        "equivalence target preflight should succeed for local-alias-routed fixture"
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
    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        )
        .is_ok(),
        "validator target preflight should succeed for local-callee-alias-routed fixture"
    );
    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        )
        .is_ok(),
        "equivalence target preflight should succeed for local-callee-alias-routed fixture"
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
    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        )
        .is_ok(),
        "validator target preflight should succeed for tuple-destructuring-routed fixture"
    );
    assert!(
        preflight_validate_test(
            test,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        )
        .is_ok(),
        "equivalence target preflight should succeed for tuple-destructuring-routed fixture"
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

    assert!(
        preflight_validate_test(
            when_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        )
        .is_ok(),
        "validator target preflight should succeed for when-shadowed argument fixture"
    );
    assert!(
        preflight_validate_test(
            when_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        )
        .is_ok(),
        "equivalence target preflight should succeed for when-shadowed argument fixture"
    );
    assert!(
        preflight_validate_test(
            if_is_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::ValidatorHandler,
        )
        .is_ok(),
        "validator target preflight should succeed for if-is-shadowed argument fixture"
    );
    assert!(
        preflight_validate_test(
            if_is_shadowed,
            ExistentialMode::default(),
            &VerificationTargetKind::Equivalence,
        )
        .is_ok(),
        "equivalence target preflight should succeed for if-is-shadowed argument fixture"
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
            existential_mode: ExistentialMode::default(),
            target: target.clone(),
        };

        let manifest = generate_lean_workspace(&validator_target_tests, &config, false)
            .unwrap_or_else(|e| {
                panic!("workspace generation should succeed for --target {target}: {e}")
            });

        assert_eq!(manifest.tests.len(), 1);

        let entry = &manifest.tests[0];
        let handler_flat = out.path().join(format!("flat/{}_handler.flat", entry.id));
        assert!(
            handler_flat.exists(),
            "workspace for --target {target} should include handler flat file"
        );

        let proof = fs::read_to_string(out.path().join(&entry.lean_file)).unwrap();
        assert!(
            proof.contains("_handler.flat"),
            "workspace for --target {target} should import handler program"
        );

        if matches!(target, VerificationTargetKind::Equivalence) {
            assert!(
                proof.contains(&format!("theorem {}_equivalence :", entry.lean_theorem)),
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

    assert!(
        proof.contains("handler_prog_"),
        "Validator mode should use handler_prog in theorems, got:\n{proof}"
    );
    assert!(
        proof.contains("_handler.flat"),
        "Should import handler flat file, got:\n{proof}"
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

    // Should have the standard property theorem
    assert!(
        proof.contains("theorem test_eq :"),
        "Should have standard property theorem, got:\n{proof}"
    );
    // Should have the equivalence theorem
    assert!(
        proof.contains("theorem test_eq_equivalence :"),
        "Should have equivalence theorem, got:\n{proof}"
    );
    assert!(
        proof.contains("proveTests prog_"),
        "Equivalence should reference property prog, got:\n{proof}"
    );
    assert!(
        proof.contains("proveTests handler_prog_"),
        "Equivalence should reference handler prog, got:\n{proof}"
    );
    // Both flat imports should be present
    assert!(
        proof.contains("_handler.flat"),
        "Should import handler flat file, got:\n{proof}"
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
        proof.contains(&format!(
            "proveTestsHalt prog_{id} (intArg x) ↔ proveTestsHalt handler_prog_{id} (intArg x)"
        )),
        "Void equivalence should compare halt predicates, got:\n{proof}"
    );
    assert!(
        !proof.contains(&format!(
            "proveTests prog_{id} (intArg x) = proveTests handler_prog_{id} (intArg x)"
        )),
        "Void equivalence must not use Option Bool equality, got:\n{proof}"
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
        proof.contains("match sampleFuzzerValue"),
        "Fallback equivalence should still use sampled-domain theorem generation, got:\n{proof}"
    );
    assert!(
        proof.contains(&format!(
            "proveTestsError prog_{id} (dataArg x) ↔ proveTestsError handler_prog_{id} (dataArg x)"
        )),
        "Void fallback equivalence should compare error predicates, got:\n{proof}"
    );
    assert!(
        !proof.contains(&format!(
            "proveTests prog_{id} (dataArg x) = proveTests handler_prog_{id} (dataArg x)"
        )),
        "Void fallback equivalence must not use Option Bool equality, got:\n{proof}"
    );
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
        proof.contains("-- Mode: witness (sampled-domain fallback auto-escalated to proof)"),
        "Expected sampled-domain fallback in fail-once witness mode, got:\n{proof}"
    );

    let marker = format!("theorem {lean_name}_equivalence :");
    let (_, equivalence_theorem) = proof
        .split_once(&marker)
        .expect("equivalence theorem marker should be present");

    assert!(
        equivalence_theorem.contains("∀ (seed : Data),"),
        "Fallback equivalence theorem must stay universal, got:\n{equivalence_theorem}"
    );
    assert!(
        equivalence_theorem.contains("| none => True :="),
        "Fallback equivalence theorem should be vacuously true on unsampled seeds, got:\n{equivalence_theorem}"
    );
    assert!(
        !equivalence_theorem.contains("∃ (seed : Data),"),
        "Fallback equivalence theorem must not switch to existential quantification, got:\n{equivalence_theorem}"
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
        GenerationErrorCategory::MissingDomain,
        "category-driven skip should not depend on message wording",
    );
    assert!(
        is_skippable_generation_error(&err),
        "MissingDomain category should be skippable regardless of message text"
    );
}

#[test]
fn non_skippable_generation_errors_ignore_legacy_message_fragments() {
    let err = generation_error(
        GenerationErrorCategory::InvalidConstraint,
        "unsupported no extractable cannot generate a proof for this shape",
    );
    assert!(
        !is_skippable_generation_error(&err),
        "Skip behavior must be category-driven, not substring-driven"
    );
}

#[test]
fn sampled_fallback_classification_is_category_driven() {
    let fallback = generation_error(
        GenerationErrorCategory::FallbackRequired,
        "message text is irrelevant",
    );
    assert!(
        should_use_sampled_fallback_for_error(&fallback),
        "Fallback classification should follow structured categories"
    );

    let invalid = generation_error(
        GenerationErrorCategory::InvalidConstraint,
        "unsupported no extractable requires fallback",
    );
    assert!(
        !should_use_sampled_fallback_for_error(&invalid),
        "Fallback classification should not depend on message fragments"
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
    };

    // Nested composites are now handled via sampled-domain fallback, so
    // generation succeeds without skip entries.
    let mut unsupported = make_test("my_module", "test_list");
    unsupported.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    unsupported.constraint = FuzzerConstraint::Any;

    let manifest = generate_lean_workspace(&[unsupported], &config, true).unwrap();
    assert_eq!(manifest.tests.len(), 1);
    assert!(manifest.skipped.is_empty());
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
        };

        let missing_metadata = make_test("my_module", "test_missing_validator_metadata");
        let manifest = generate_lean_workspace(&[missing_metadata], &config, true)
            .unwrap_or_else(|e| panic!("skip mode should not fail for target {target}: {e}"));

        assert!(
            manifest.tests.is_empty(),
            "no proof entries should be generated for skipped target {target}"
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
    };

    let mut unsupported = make_test("my_module", "test_list");
    unsupported.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    unsupported.constraint = FuzzerConstraint::Any;

    let result = generate_lean_workspace(&[unsupported], &config, false);
    assert!(
        result.is_ok(),
        "Nested composites should generate via fallback even without skip mode"
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
    };

    let good = make_test("my_module", "test_int");
    let mut bad = make_test("my_module", "test_list");
    bad.fuzzer_output_type = FuzzerOutputType::Tuple(vec![
        FuzzerOutputType::List(Box::new(FuzzerOutputType::Int)),
        FuzzerOutputType::Data,
    ]);
    bad.constraint = FuzzerConstraint::Any;

    let manifest = generate_lean_workspace(&[good, bad], &config, true).unwrap();
    assert_eq!(manifest.tests.len(), 2, "Both tests should generate");
    assert!(manifest.skipped.is_empty(), "No tests should be skipped");
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("∀ (seed : Data),"));
    assert!(proof.contains("match sampleFuzzerValue"));
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("match sampleFuzzerValue"));
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
        proof.contains("∀ (a : Integer) (b : Data),"),
        "Pair(Int,Data) should quantify over Integer and Data, got:\n{proof}"
    );
    assert!(
        proof.contains("(-10 <= a && a <= 10)"),
        "Should have bounds for Int element, got:\n{proof}"
    );
    assert!(
        proof.contains("Data.List [Data.I a, b]"),
        "Should encode pair with Int and Data, got:\n{proof}"
    );
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

    assert!(
        proof.contains("∀ (a : Data) (b : Integer) (c : Bool) (d : ByteString),"),
        "4-tuple should quantify over four vars, got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= b && b <= 99)"),
        "Should have bounds for Int element b, got:\n{proof}"
    );
    assert!(
        proof.contains("Data.List [a, Data.I b, (Data.Constr (if c then 1 else 0) []), Data.B d]"),
        "Should inline all element encoders, got:\n{proof}"
    );
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("match sampleFuzzerValue"));
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("match sampleFuzzerValue"));
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
        proof.contains("∀ (xs : List Integer),"),
        "List<Int> should quantify over List Integer, got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= xs.length && xs.length <= 10)"),
        "Should have length bounds, got:\n{proof}"
    );
    assert!(
        proof.contains("xs.all (fun x_i => 0 <= x_i && x_i <= 10)"),
        "Should use List.all for element bounds, got:\n{proof}"
    );
    assert!(
        !proof.contains("∀ x_i ∈ xs"),
        "Should avoid List.Mem-style quantification in generated bounds, got:\n{proof}"
    );
    assert!(
        proof.contains("Data.I x_i"),
        "Should encode elements as Data.I, got:\n{proof}"
    );
    assert!(proof.contains("xs.map"));
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
        proof.contains("∀ (xs : List Data),"),
        "List<Data> should quantify over List Data, got:\n{proof}"
    );
    assert!(
        proof.contains("(1 <= xs.length && xs.length <= 5)"),
        "Should have length bounds, got:\n{proof}"
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

    assert!(
        proof.contains("(1 <= xs.length && xs.length <= 5)"),
        "Should extract list bounds through And constraint, got:\n{proof}"
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
        proof.contains("(0 <= xs.length)"),
        "Should emit lower-bound-only length precondition, got:\n{proof}"
    );
    assert!(
        !proof.contains("xs.length <= "),
        "Should not emit upper length bound when max_len is missing, got:\n{proof}"
    );
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

    let proof = generate_proof_file(
        &test,
        &id,
        &lean_name,
        lean_module,
        ExistentialMode::default(),
        &VerificationTargetKind::default(),
    )
    .unwrap();

    assert!(proof.contains("∀ (seed : Data),"));
    assert!(proof.contains("match sampleFuzzerValue"));
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
        proof.contains("∀ (a : Integer) (b : Data),"),
        "Tuple(Int, ADT) should map ADT to Data, got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= a && a <= 50)"),
        "Should have Int bounds, got:\n{proof}"
    );
    assert!(
        proof.contains("Data.List [Data.I a, b]"),
        "Should encode ADT element as plain Data var, got:\n{proof}"
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
        proof.contains("∀ (a : Integer) (b : Data),"),
        "Tuple(Int, Data) should quantify over Integer and Data, got:\n{proof}"
    );
    assert!(
        proof.contains("(0 <= a && a <= 50)"),
        "Shared IntRange should constrain Int slot, got:\n{proof}"
    );
    assert!(
        !proof.contains("0 <= b"),
        "Shared IntRange must not constrain non-Int tuple slots, got:\n{proof}"
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
    assert_eq!(err.kind(), std::io::ErrorKind::InvalidInput);
}

#[test]
fn clean_artifacts_rejects_generic_paths_without_verify_markers() {
    let dir = tempfile::tempdir().unwrap();
    let out = dir.path().join("outside_project_workspace");
    fs::create_dir_all(out.join("logs")).unwrap();
    fs::create_dir_all(out.join("flat")).unwrap();
    fs::write(out.join("manifest.json"), "{}").unwrap();
    fs::write(out.join("lakefile.lean"), "-- unrelated").unwrap();

    let err =
        clean_artifacts(&out).expect_err("generic paths without verify markers should be rejected");
    assert_eq!(err.kind(), std::io::ErrorKind::InvalidInput);
    assert!(out.join("logs").exists());
    assert!(out.join("flat").exists());
    assert!(out.join("manifest.json").exists());
    assert!(out.join("lakefile.lean").exists());
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
    assert_eq!(err.kind(), std::io::ErrorKind::InvalidInput);
    assert!(
        outside
            .join("build/lib/AikenVerify/should-not-delete.olean")
            .exists(),
        "cleanup must not delete directories outside the verification workspace"
    );
}

#[test]
fn clear_generated_workspace_removes_generated_outputs_and_preserves_cache() {
    let dir = tempfile::tempdir().unwrap();
    let out = dir.path();

    fs::create_dir_all(out.join("AikenVerify/Proofs/Stale")).unwrap();
    fs::write(out.join("AikenVerify/Proofs/Stale/old.lean"), "-- stale").unwrap();
    fs::create_dir_all(out.join("flat")).unwrap();
    fs::write(out.join("flat/stale.flat"), "stale").unwrap();
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
    assert!(!out.join("flat").exists());
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
    assert!(caps.existential_modes.contains(&"proof".to_string()));
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

        let dir = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: dir.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            existential_mode: ExistentialMode::Witness,
            target: VerificationTargetKind::PropertyWrapper,
        };

        let manifest = generate_lean_workspace(&[test], &config, false);
        assert!(
            manifest.is_ok(),
            "Scalar type {:?} should generate workspace",
            output_type
        );
        assert_eq!(manifest.unwrap().tests.len(), 1);
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

        let dir = tempfile::tempdir().unwrap();
        let config = VerifyConfig {
            out_dir: dir.path().to_path_buf(),
            cek_budget: 20000,
            blaster_rev: DEFAULT_BLASTER_REV.to_string(),
            existential_mode: ExistentialMode::Witness,
            target: VerificationTargetKind::PropertyWrapper,
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

    let dir = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: dir.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
    };

    let result = generate_lean_workspace(&[test], &config, false);
    assert!(
        result.is_ok(),
        "Nested composites should now generate via sampled-domain fallback"
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

    let dir = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: dir.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
    };

    let manifest = generate_lean_workspace(&[test], &config, true).unwrap();
    assert_eq!(manifest.tests.len(), 1);
    assert!(manifest.skipped.is_empty());
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

    let dir = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: dir.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
    };

    let manifest = generate_lean_workspace(&[test], &config, false).unwrap();
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

    let dir = tempfile::tempdir().unwrap();
    let config = VerifyConfig {
        out_dir: dir.path().to_path_buf(),
        cek_budget: 20000,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        existential_mode: ExistentialMode::Witness,
        target: VerificationTargetKind::PropertyWrapper,
    };

    let manifest = generate_lean_workspace(&[test], &config, false).unwrap();
    assert_eq!(manifest.tests.len(), 1);

    let proof_path = dir.path().join(&manifest.tests[0].lean_file);
    let content = fs::read_to_string(proof_path).unwrap();
    assert!(
        content.contains("proveTestsHalt"),
        "Void-returning test should use proveTestsHalt"
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

    // 4. Nested composite types are supported via sampled-domain fallback.
    assert!(
        caps.supported_fuzzer_types
            .iter()
            .any(|t| t.contains("sampled-domain fallback"))
    );

    // 5. Blaster translation gaps are documented explicitly so failures can
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
        proof.contains("open PlutusCore.Data (Data)"),
        "Tuple proof should open Data namespace for Data.List encoding, got:\n{proof}"
    );
    assert!(
        proof.contains("Data.List [Data.I a, Data.I b]"),
        "Tuple proof should encode arguments through Data constructors, got:\n{proof}"
    );
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
        proof.contains(&format!(
            "match sampleFuzzerValue fuzzer_prog_{id} seed with"
        )),
        "Should use sampled-domain fallback theorem, got:\n{proof}"
    );
    assert!(
        proof.contains("open PlutusCore.Data (Data)"),
        "List proof should open Data namespace for Data.List encoding, got:\n{proof}"
    );
    assert!(
        !proof.contains("0 <= x_i"),
        "Should NOT emit numeric element bounds for Bool type, got:\n{proof}"
    );
    assert!(
        !proof.contains("x_i <= 1"),
        "Should NOT emit numeric element bounds for Bool type, got:\n{proof}"
    );
}
