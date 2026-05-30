use super::capabilities::Args as CapabilitiesArgs;
use super::run::{Args as RunArgs, exec as exec_run};
use super::{Cmd, capabilities::*, clean::*, doctor::*, output::*, paths::*, run::*};
use aiken_project::{
    Project,
    export::{ExportedPropertyTest, FuzzerConstraint, FuzzerOutputType, VerificationTargetKind},
    telemetry::EventTarget,
    verify::{
        self, ArtifactRetention, DEFAULT_BLASTER_REV, DEFAULT_PLUTUS_CORE_REV, ExistentialMode,
        MAX_RAW_OUTPUT_TAIL_BYTES, SkipPolicy,
    },
    watch::ExitFailure,
};
use clap::{CommandFactory, Parser};
#[cfg(unix)]
use std::os::unix::fs::symlink;
use std::{
    fs,
    path::{Path, PathBuf},
    time::SystemTime,
};

#[test]
fn json_and_silent_modes_use_quiet_project_loading() {
    assert!(matches!(
        OutputMode::Json.project_event_target(),
        EventTarget::Silent
    ));
    assert!(matches!(
        OutputMode::Silent.project_event_target(),
        EventTarget::Silent
    ));
    assert!(!OutputMode::Json.reports_project_diagnostics());
    assert!(!OutputMode::Silent.reports_project_diagnostics());
    assert!(OutputMode::Text.reports_project_diagnostics());
}

#[test]
fn workspace_json_aggregates_package_roots() {
    let repeated_artifact = "AikenVerify/Proofs/example.lean";
    let first_summary = serde_json::json!({
        "verify_summary_version": verify::VERIFY_SUMMARY_VERSION,
        "artifacts": { "lean_files": [repeated_artifact] }
    });
    let second_summary = serde_json::json!({
        "verify_summary_version": verify::VERIFY_SUMMARY_VERSION,
        "artifacts": { "lean_files": [repeated_artifact] }
    });
    let project_results = vec![
        RunProjectResult {
            root: "/workspace/member-a".to_string(),
            output: serde_json::to_string(&first_summary).unwrap(),
            exit_code: 0,
        },
        RunProjectResult {
            root: "/workspace/member-b".to_string(),
            output: serde_json::to_string(&second_summary).unwrap(),
            exit_code: 0,
        },
    ];

    let aggregate = aggregate_workspace_json(&project_results).unwrap();

    assert_eq!(
        aggregate["verify_summary_version"],
        serde_json::json!(verify::VERIFY_SUMMARY_VERSION)
    );
    assert_eq!(aggregate["packages"].as_array().unwrap().len(), 2);
    assert_eq!(aggregate["packages"][0]["root"], "/workspace/member-a");
    assert_eq!(aggregate["packages"][1]["root"], "/workspace/member-b");
    assert_eq!(aggregate["packages"][0]["summary"], first_summary);
    assert_eq!(aggregate["packages"][1]["summary"], second_summary);
    assert_eq!(
        aggregate["packages"][0]["summary"]["artifacts"]["lean_files"][0],
        aggregate["packages"][1]["summary"]["artifacts"]["lean_files"][0],
        "repeated relative artifact paths stay attributable through each package root"
    );
}

fn dummy_property_test(
    name: &str,
    fuzzer_output_type: FuzzerOutputType,
    constraint: FuzzerConstraint,
) -> ExportedPropertyTest {
    serde_json::from_value(serde_json::json!({
        "name": name,
        "module": "example",
        "input_path": "lib/example.ak",
        "on_test_failure": "fail_immediately",
        "return_mode": "bool",
        "target_kind": "property",
        "validator_target": null,
        "test_program": { "hex": "" },
        "fuzzer_program": { "hex": "" },
        "fuzzer_type": "Int",
        "fuzzer_output_type": fuzzer_output_type,
        "constraint": constraint,
        "semantics": {
            "opaque": {
                "reason": "test fixture semantics not set"
            }
        },
        "fuzzer_data_schema": null,
        "inner_data_schemas": {},
        "concrete_halt_witnesses": [],
        "concrete_error_witnesses": [],
        "transition_prop_lean": null
    }))
    .expect("dummy property test JSON should deserialize")
}

fn unique_temp_dir(prefix: &str) -> PathBuf {
    let timestamp = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("system clock should be after UNIX_EPOCH")
        .as_nanos();
    std::env::temp_dir().join(format!("{prefix}-{}-{timestamp}", std::process::id()))
}

fn write_verify_target_fixture(root: &Path) {
    fs::create_dir_all(root.join("validators"))
        .expect("fixture validators directory should be creatable");

    fs::write(
        root.join("aiken.toml"),
        r#"
name = "test/verify_target_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify command target fixture"
"#,
    )
    .expect("fixture aiken.toml should be writable");

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
"#,
    )
    .expect("fixture module should be writable");
}

fn write_verify_skip_only_fixture(root: &Path) {
    fs::create_dir_all(root.join("validators"))
        .expect("fixture validators directory should be creatable");

    fs::write(
        root.join("aiken.toml"),
        r#"
name = "test/verify_skip_only_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify command skip-only fixture"
"#,
    )
    .expect("fixture aiken.toml should be writable");

    fs::write(
        root.join("validators/fixture.ak"),
        r#"
fn int_fuzzer() -> Fuzzer<Int> {
  todo
}

test unsupported_for_validator_target(x via int_fuzzer()) {
  x == x
}
"#,
    )
    .expect("fixture module should be writable");
}

fn write_verify_unit_only_fixture(root: &Path) {
    fs::create_dir_all(root.join("validators"))
        .expect("fixture validators directory should be creatable");

    fs::write(
        root.join("aiken.toml"),
        r#"
name = "test/verify_no_property_fixture"
version = "0.0.0"
plutusVersion = "v3"
description = "verify no-property-tests JSON fixture"
"#,
    )
    .expect("fixture aiken.toml should be writable");

    fs::write(
        root.join("validators/fixture.ak"),
        r#"
test unit_smoke() {
  True
}
"#,
    )
    .expect("fixture module should be writable");
}

fn fixture_run_options(out_dir: PathBuf, target: VerificationTargetKind) -> RunCommandOptions {
    RunCommandOptions {
        match_tests: None,
        exact_match: false,
        env: None,
        generate_only: true,
        out_dir,
        artifact_policy: ArtifactRetention::OnFailure,
        timeout: 300,
        cek_budget: 200_000,
        jobs_override: None,
        output_mode: OutputMode::Json,
        skip_policy: SkipPolicy::None,
        allow_skips: false,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        solver_profile: verify::SolverProfile::default(),
        trust_profile: verify::TrustProfile::default(),
        target,
        plutus_core_dir: None,
        raw_output_bytes: 65536,
        accept_partial: false,
        accept_witness: false,
        allow_vacuous_subgenerators: false,
    }
}

fn sample_doctor_report(
    all_ok: bool,
    blaster_rev: &str,
    plutus_core_rev: &str,
) -> verify::DoctorReport {
    let capabilities = verify::capabilities();
    verify::DoctorReport::new(
        verify::DOCTOR_REPORT_VERSION.to_string(),
        vec![
            verify::ToolCheck::new(
                "lean".to_string(),
                true,
                Some("4.11.0".to_string()),
                true,
                "4.11.0".to_string(),
                None,
            ),
            verify::ToolCheck::new(
                "lake".to_string(),
                true,
                Some("4.11.0".to_string()),
                true,
                "0.0.0".to_string(),
                None,
            ),
            verify::ToolCheck::new(
                "z3".to_string(),
                true,
                Some("4.13.0".to_string()),
                true,
                "4.13.0".to_string(),
                None,
            ),
        ],
        verify::PlutusCoreCheck::new(
            true,
            "git: input-output-hk/PlutusCoreBlaster".to_string(),
            true,
            None,
        ),
        blaster_rev.to_string(),
        plutus_core_rev.to_string(),
        all_ok,
        capabilities.backends.clone(),
        capabilities.certification.clone(),
        capabilities,
    )
}

fn trusted_domain_json() -> serde_json::Value {
    serde_json::json!({
        "relation": {
            "kind": "and",
            "items": [
                {
                    "kind": "constraint",
                    "constraint": { "int_range": { "min": "0", "max": "255" } }
                },
                {
                    "kind": "semantics",
                    "semantics": { "int_range": { "min": "0", "max": "255" } }
                }
            ]
        },
        "precision": "OverApprox",
        "certificate": "TrustedVersionedModel",
        "obligations_open": [],
        "obligations_discharged": [
            "FuzzerReturnsImpliesDomain",
            "FuzzerModelHashMatches",
            "ValueDecoderRoundTrip",
            "FuzzerOutputTypeMatchesPropertyInputType",
            "PropertyHarnessAcceptsDecodedInput",
            "PropertyHarnessMatchesExportedUPLC"
        ],
        "lowering_path": ["constraint_ir", "known_combinator"],
        "widenings": [],
        "production_allowed": true,
        "diagnostics": []
    })
}

fn skipped_domain_json(reason: &str) -> serde_json::Value {
    serde_json::json!({
        "relation": {
            "kind": "unknown",
            "reason": reason
        },
        "precision": "Unknown",
        "certificate": "Unchecked",
        "obligations_open": [],
        "obligations_discharged": [],
        "lowering_path": ["unsupported_generation"],
        "widenings": [],
        "production_allowed": false,
        "diagnostics": [reason]
    })
}

fn trusted_domain() -> verify::LoweredDomain {
    serde_json::from_value(trusted_domain_json()).expect("trusted domain JSON should deserialize")
}

fn trusted_input_type() -> verify::InputValueBridge {
    serde_json::from_value(serde_json::json!({
        "aiken_type": "Int",
        "runtime_encoding": "integer_constant",
        "data_encoding": "integer",
        "lean_encoding": "integer",
        "schema_hash": "88".repeat(32),
        "schema": { "kind": "int" }
    }))
    .expect("trusted input bridge JSON should deserialize")
}

fn witness_domain(values: &[&str]) -> verify::LoweredDomain {
    serde_json::from_value(serde_json::json!({
        "relation": {
            "kind": "witness",
            "encoded_values": values.iter().map(|v| (*v).to_string()).collect::<Vec<_>>()
        },
        "precision": "WitnessOnly",
        "certificate": "WitnessReplay",
        "obligations_open": [],
        "obligations_discharged": [
            "WitnessReplaysThroughFuzzer",
            "WitnessSatisfiesDomain",
            "FuzzerModelHashMatches",
            "ValueDecoderRoundTrip",
            "FuzzerOutputTypeMatchesPropertyInputType",
            "PropertyHarnessAcceptsDecodedInput",
            "PropertyHarnessMatchesExportedUPLC"
        ],
        "lowering_path": ["witness_replay"],
        "widenings": [],
        "production_allowed": true,
        "diagnostics": []
    }))
    .expect("witness domain JSON should deserialize")
}

fn partial_domain(reason: &str) -> verify::LoweredDomain {
    serde_json::from_value(serde_json::json!({
        "relation": {
            "kind": "true_with_explicit_widening",
            "reason": reason,
            "allowed_only_under": "unsafe-dev"
        },
        "precision": "Unknown",
        "certificate": "Unchecked",
        "obligations_open": [
            "FuzzerReturnsImpliesDomain",
            "ValueDecoderRoundTrip",
            "FuzzerOutputTypeMatchesPropertyInputType",
            "PropertyHarnessAcceptsDecodedInput"
        ],
        "obligations_discharged": [
            "FuzzerModelHashMatches",
            "PropertyHarnessMatchesExportedUPLC"
        ],
        "lowering_path": ["placeholder_scan"],
        "widenings": [
            {
                "kind": "placeholder",
                "message": reason,
                "allowed_only_under": "unsafe-dev"
            }
        ],
        "production_allowed": false,
        "diagnostics": [reason]
    }))
    .expect("partial domain JSON should deserialize")
}
fn sample_generated_manifest() -> verify::GeneratedManifest {
    serde_json::from_value(serde_json::json!({
        "schema_version": verify::GENERATED_MANIFEST_SCHEMA_VERSION,
        "version": verify::GENERATE_ONLY_VERSION,
        "aiken": {
            "version": "vtest",
            "commit": "deadbeef",
            "prelude_hash": "11".repeat(32),
            "fuzz_package_hash": "22".repeat(32)
        },
        "execution": {
            "plutus_version": "v3",
            "cek_budget": {
                "fuel": 200000
            },
            "decode_policy": "decode_error_is_test_failure"
        },
        "compatibility_limitations": [
            "execution.cek_budget records only the unified CEK fuel bound; CPU and memory sub-budgets are not currently exported."
        ],
        "tests": [
            {
                "id": "example_test_ok",
                "aiken_module": "example",
                "aiken_name": "test_ok",
                "lean_module": "Example.TestOk",
                "lean_theorem": "example_test_ok",
                "lean_file": "Example/TestOk.lean",
                "flat_file": "example_test_ok.flat",
                "fuzzer_flat_file": "example_test_ok_fuzzer.flat",
                "return_mode": "bool",
                "test_mode": "normal",
                "on_test_failure": "fail_immediately",
                "property_uplc_hash": "33".repeat(32),
                "property_harness_hash": "44".repeat(28),
                "fuzzer_uplc_hash": "55".repeat(32),
                "fuzzer_harness_hash": "66".repeat(28),
                "fuzzer_model_hash": "77".repeat(32),
                "sampler": {
                    "harness_abi": "aiken_fuzzer_prng_to_option_pair_prng_data_v1",
                    "run_kind": "seeded_generation",
                    "prng_mode": "seeded",
                    "decode_policy": "decode_error_is_test_failure"
                },
                "domain": trusted_domain_json(),
                "has_termination_theorem": true,
                "has_equivalence_theorem": true,
                "over_approximations": 2,
                "partial_proof_note": "partial proof note",
                "witness_proof_note": {
                    "instances": 2,
                    "witnesses": ["00", "01"],
                    "note": "witness-only"
                }
            }
        ],
        "skipped": [
            {
                "name": "example.test_skip",
                "module": "example",
                "reason": "unsupported shape",
                "trust_profile": "production",
                "domain": skipped_domain_json("unsupported shape")
            }
        ]
    }))
    .expect("generated manifest JSON should deserialize")
}

#[derive(Debug)]
struct DocsGoldenFixture {
    name: String,
    description: String,
    manifest_entry: verify::ManifestEntry,
    theorem_result: verify::TheoremResult,
}

fn docs_fixture_path(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../docs/verification/fixtures")
        .join(name)
}

fn read_docs_fixture(name: &str) -> DocsGoldenFixture {
    let path = docs_fixture_path(name);
    let content = fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()));
    let value: serde_json::Value = serde_json::from_str(&content)
        .unwrap_or_else(|err| panic!("failed to parse {}: {err}", path.display()));
    let name = value
        .get("name")
        .and_then(serde_json::Value::as_str)
        .unwrap_or_else(|| panic!("{} is missing string field 'name'", path.display()))
        .to_string();
    let description = value
        .get("description")
        .and_then(serde_json::Value::as_str)
        .unwrap_or_else(|| panic!("{} is missing string field 'description'", path.display()))
        .to_string();
    let manifest_entry =
        serde_json::from_value(value.get("manifest_entry").cloned().unwrap_or_else(|| {
            panic!(
                "{} is missing object field 'manifest_entry'",
                path.display()
            )
        }))
        .unwrap_or_else(|err| {
            panic!(
                "failed to parse manifest_entry in {}: {err}",
                path.display()
            )
        });
    let theorem_result =
        serde_json::from_value(value.get("theorem_result").cloned().unwrap_or_else(|| {
            panic!(
                "{} is missing object field 'theorem_result'",
                path.display()
            )
        }))
        .unwrap_or_else(|err| {
            panic!(
                "failed to parse theorem_result in {}: {err}",
                path.display()
            )
        });

    DocsGoldenFixture {
        name,
        description,
        manifest_entry,
        theorem_result,
    }
}

#[test]
fn docs_readme_links_verification_guides() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../README.md");
    let readme = fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()));
    assert!(readme.contains("docs/verification/README.md"));
    assert!(readme.contains("docs/verification/developer-guide.md"));
    assert!(readme.contains("docs/verification/schema-migration.md"));
    assert!(readme.contains("docs/verification/fixtures/README.md"));
}

#[test]
fn docs_fixture_primitive_normal_bool_is_current() {
    let fixture = read_docs_fixture("01-primitive-normal-bool.json");
    assert_eq!(fixture.name, "primitive-normal-bool");
    assert!(!fixture.description.is_empty());
    assert_eq!(
        fixture.manifest_entry.return_mode,
        Some(aiken_project::export::TestReturnMode::Bool)
    );
    assert_eq!(
        fixture.manifest_entry.test_mode,
        Some(verify::ManifestTestMode::Normal)
    );
    assert_eq!(
        fixture.theorem_result.status,
        verify::VerificationStatus::SolverValidated
    );
    assert_eq!(
        fixture.theorem_result.domain.precision,
        verify::DomainPrecision::Exact
    );
    assert_eq!(
        fixture.theorem_result.domain.certificate,
        verify::DomainCertificate::TrustedVersionedModel
    );
}

#[test]
fn docs_fixture_primitive_fail_void_is_current() {
    let fixture = read_docs_fixture("02-primitive-fail-void.json");
    assert_eq!(fixture.name, "primitive-fail-void");
    assert_eq!(
        fixture.manifest_entry.return_mode,
        Some(aiken_project::export::TestReturnMode::Void)
    );
    assert_eq!(
        fixture.manifest_entry.test_mode,
        Some(verify::ManifestTestMode::Fail)
    );
    assert_eq!(
        fixture.theorem_result.status,
        verify::VerificationStatus::SolverValidated
    );
    assert_eq!(
        fixture.theorem_result.domain.precision,
        verify::DomainPrecision::OverApprox
    );
}

#[test]
fn docs_fixture_relational_custom_fuzzer_is_current() {
    let fixture = read_docs_fixture("03-relational-custom-fuzzer.json");
    assert_eq!(fixture.name, "relational-custom-fuzzer");
    assert!(matches!(
        fixture.theorem_result.domain.relation,
        verify::DomainRel::Image { .. }
    ));
    match &fixture.theorem_result.domain.relation {
        verify::DomainRel::Image { sources, .. } => assert_eq!(sources.len(), 2),
        other => panic!("expected image relation, got {other:?}"),
    }
}

#[test]
fn docs_fixture_fail_once_witness_is_current() {
    let fixture = read_docs_fixture("04-fail-once-witness.json");
    assert_eq!(fixture.name, "fail-once-witness");
    assert_eq!(
        fixture.manifest_entry.test_mode,
        Some(verify::ManifestTestMode::FailOnce)
    );
    assert_eq!(
        fixture.theorem_result.status,
        verify::VerificationStatus::WitnessValidated
    );
    assert_eq!(
        fixture.theorem_result.domain.precision,
        verify::DomainPrecision::WitnessOnly
    );
    assert!(matches!(
        fixture
            .theorem_result
            .domain
            .sampler
            .as_ref()
            .map(|s| s.run_kind),
        Some(verify::SamplerRunKind::WitnessReplay)
    ));
    assert!(matches!(
        fixture.theorem_result.proof_status,
        verify::ProofStatus::WitnessProved { .. }
    ));
    let explanation = fixture
        .theorem_result
        .explanation
        .as_deref()
        .expect("witness fixture must carry an explanation");
    assert!(
        explanation.contains("listed concrete instance(s), not a universal theorem"),
        "witness fixture must explain that witness validation is not universal: {explanation}"
    );
}

#[test]
fn docs_fixture_sampler_fallback_partial_is_current() {
    let fixture = read_docs_fixture("05-sampler-fallback-partial.json");
    assert_eq!(fixture.name, "sampler-fallback-partial");
    assert_eq!(
        fixture.theorem_result.status,
        verify::VerificationStatus::Partial
    );
    assert!(matches!(
        fixture.theorem_result.domain.relation,
        verify::DomainRel::SamplerReturns { .. }
    ));
    assert!(!fixture.theorem_result.domain.production_allowed);
    assert!(
        fixture
            .theorem_result
            .domain
            .obligations_open
            .contains(&verify::DomainObligation::ValueDecoderRoundTrip)
    );
}

#[test]
fn docs_fixture_scenario_trace_partial_is_current() {
    let fixture = read_docs_fixture("06-scenario-trace-partial.json");
    assert_eq!(fixture.name, "scenario-trace-partial");
    assert_eq!(
        fixture.theorem_result.status,
        verify::VerificationStatus::Partial
    );
    let trace = fixture
        .theorem_result
        .domain
        .scenario_trace
        .as_ref()
        .expect("scenario fixture should carry scenario trace metadata");
    assert_eq!(
        trace.symbolic_encoding,
        verify::ScenarioSymbolicEncoding::BoundedBooleanChecker
    );
    assert!(trace.uses_global_reachability_over_approx);
    assert_eq!(trace.trace_length.min, 2);
    assert_eq!(trace.trace_length.max, Some(4));
}

fn skipped_only_manifest() -> verify::GeneratedManifest {
    serde_json::from_value(serde_json::json!({
        "schema_version": verify::GENERATED_MANIFEST_SCHEMA_VERSION,
        "version": verify::GENERATE_ONLY_VERSION,
        "aiken": {
            "version": "vtest",
            "commit": "deadbeef"
        },
        "execution": {
            "plutus_version": "v3",
            "cek_budget": {
                "fuel": 200000
            },
            "decode_policy": "decode_error_is_test_failure"
        },
        "compatibility_limitations": [
            "execution.cek_budget records only the unified CEK fuel bound; CPU and memory sub-budgets are not currently exported."
        ],
        "tests": [],
        "skipped": [
            {
                "name": "example.test_unsupported",
                "module": "example",
                "reason": "unsupported shape",
                "trust_profile": "production",
                "domain": skipped_domain_json("unsupported shape")
            }
        ]
    }))
    .expect("skipped-only manifest JSON should deserialize")
}

fn single_test_manifest(include_skipped: bool) -> verify::GeneratedManifest {
    let skipped = if include_skipped {
        serde_json::json!([
            {
                "name": "example.test_unsupported",
                "module": "example",
                "reason": "unsupported shape",
                "trust_profile": "production",
                "domain": skipped_domain_json("unsupported shape")
            }
        ])
    } else {
        serde_json::json!([])
    };
    serde_json::from_value(serde_json::json!({
        "schema_version": verify::GENERATED_MANIFEST_SCHEMA_VERSION,
        "version": verify::GENERATE_ONLY_VERSION,
        "aiken": {
            "version": "vtest",
            "commit": "deadbeef"
        },
        "execution": {
            "plutus_version": "v3",
            "cek_budget": {
                "fuel": 200000
            },
            "decode_policy": "decode_error_is_test_failure"
        },
        "compatibility_limitations": [
            "execution.cek_budget records only the unified CEK fuel bound; CPU and memory sub-budgets are not currently exported."
        ],
        "tests": [
            {
                "id": "example_test_ok",
                "aiken_module": "example",
                "aiken_name": "test_ok",
                "lean_module": "Example.TestOk",
                "lean_theorem": "example_test_ok",
                "lean_file": "Example/TestOk.lean",
                "flat_file": "example_test_ok.flat",
                "return_mode": "bool",
                "test_mode": "normal",
                "on_test_failure": "fail_immediately",
                "property_uplc_hash": "33".repeat(32),
                "property_harness_hash": "44".repeat(28),
                "fuzzer_uplc_hash": "55".repeat(32),
                "fuzzer_harness_hash": "66".repeat(28),
                "fuzzer_model_hash": "77".repeat(32),
                "domain": trusted_domain_json(),
                "has_termination_theorem": false,
                "has_equivalence_theorem": false,
                "over_approximations": 0
            }
        ],
        "skipped": skipped
    }))
    .expect("single-test manifest JSON should deserialize")
}

fn successful_raw_result() -> verify::VerifyResult {
    serde_json::from_value(serde_json::json!({
        "success": true,
        "stdout": {
            "tail": "",
            "total_bytes": 0,
            "truncated": false
        },
        "stderr": {
            "tail": "",
            "total_bytes": 0,
            "truncated": false
        },
        "exit_code": 0,
        "theorem_results": [
            {
                "test_name": "example.test_ok",
                "theorem_name": "Example.TestOk",
                "status": { "kind": "proved" },
                "over_approximations": 0
            }
        ]
    }))
    .expect("raw result JSON should deserialize")
}

#[test]
fn doctor_command_branch_reports_success_output_and_exit_code() {
    let result = run_doctor_command_with(
        false,
        "abc123".to_string(),
        "pc-rev-1".to_string(),
        |blaster_rev, plutus_core_rev| {
            let mut report = sample_doctor_report(true, blaster_rev, plutus_core_rev);
            report.certification.lean_certified.available = true;
            report.certification.proof_reconstruction.available = true;
            report.certification.strict_cert_profile.available = true;
            report
        },
    )
    .expect("doctor branch should render output");

    assert_eq!(result.exit_code, 0);
    assert!(
        result.output.contains("Verify Doctor Report"),
        "doctor text output should include the section header"
    );
    assert!(
        result.output.contains("All checks passed."),
        "doctor success output should include the success summary"
    );
}

#[test]
fn doctor_command_branch_reports_json_output_and_failure_exit_code() {
    let result = run_doctor_command_with(
        true,
        "deadbeef".to_string(),
        "pc-rev-2".to_string(),
        |blaster_rev, plutus_core_rev| sample_doctor_report(false, blaster_rev, plutus_core_rev),
    )
    .expect("doctor branch should render JSON output");

    assert_eq!(result.exit_code, 1);
    let value: serde_json::Value =
        serde_json::from_str(&result.output).expect("doctor JSON output should parse");
    assert_eq!(value["version"], verify::DOCTOR_REPORT_VERSION);
    assert_eq!(value["all_ok"], false);
    assert_eq!(value["blaster_rev"], "deadbeef");
    assert_eq!(value["plutus_core_rev"], "pc-rev-2");
}

#[test]
fn clean_command_branch_reports_removed_artifacts_and_exit_code() {
    let removed = vec![
        PathBuf::from("build/verify/manifest.json"),
        PathBuf::from("build/verify/logs"),
    ];
    let result = run_clean_command_with(
        Path::new("workspace/member-a"),
        PathBuf::from("build/verify"),
        move |_out_dir| Ok(removed),
    )
    .expect("clean branch should succeed");

    assert_eq!(result.exit_code, 0);
    assert!(
        result.output.contains("Removed build/verify/manifest.json"),
        "clean output should list removed files"
    );
    assert!(
        result.output.contains("Removed build/verify/logs"),
        "clean output should include each removed artifact"
    );
}

#[test]
fn clean_command_branch_resolves_relative_out_dir_under_project_root() {
    let project_root = PathBuf::from("workspace/member-a");
    let mut resolved_out_dir = None;

    let _result = run_clean_command_with(&project_root, PathBuf::from("build/verify"), |out_dir| {
        resolved_out_dir = Some(out_dir.to_path_buf());
        Ok(vec![])
    })
    .expect("clean branch should resolve out_dir against project root");

    assert_eq!(resolved_out_dir, Some(project_root.join("build/verify")));
}

#[test]
fn clean_command_branch_rejects_parent_traversal_out_dir() {
    let project_root = PathBuf::from("workspace/member-a");

    let result = run_clean_command_with(&project_root, PathBuf::from("../verify"), |_out_dir| {
        Ok(vec![])
    });

    assert!(
        result.is_err(),
        "clean command should reject out_dir paths with parent traversal"
    );
}

#[test]
fn clean_command_branch_rejects_absolute_out_dir_outside_project_root() {
    let cwd = std::env::current_dir().expect("test process must be able to determine cwd");
    let project_root = cwd.join("workspace/member-a");
    let outside_out_dir = cwd.join("workspace/member-b/build/verify");

    let result = run_clean_command_with(&project_root, outside_out_dir, |_out_dir| Ok(vec![]));

    assert!(
        result.is_err(),
        "clean command should reject absolute out_dir outside project root"
    );
}

#[test]
fn clean_command_rejects_nonexistent_roots() {
    let fixture_root = unique_temp_dir("verify-clean-nonexistent-root");
    if fixture_root.exists() {
        fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
    }

    let mut attempted_cleanup = false;
    let result = run_clean_for_directory_with(
        Some(fixture_root.as_path()),
        PathBuf::from("build/verify"),
        |_path| {
            attempted_cleanup = true;
            Ok(vec![])
        },
    );

    let message = match result {
        Ok(_) => panic!("nonexistent clean root should be rejected"),
        Err(error) => error.to_string(),
    };
    assert!(
        message.contains("requested directory"),
        "expected nonexistent-root error, got: {message}"
    );
    assert!(!attempted_cleanup, "clean must not run without a real root");
}

#[test]
fn clean_command_rejects_missing_manifest_boundaries() {
    let fixture_root = unique_temp_dir("verify-clean-missing-manifest");
    if fixture_root.exists() {
        fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
    }
    fs::create_dir_all(fixture_root.join("build/verify/AikenVerify"))
        .expect("fixture workspace marker should be creatable");
    fs::write(fixture_root.join("build/verify/manifest.json"), "{}")
        .expect("manifest artifact should be writable");

    let mut attempted_cleanup = false;
    let result = run_clean_for_directory_with(
        Some(fixture_root.as_path()),
        PathBuf::from("build/verify"),
        |_path| {
            attempted_cleanup = true;
            Ok(vec![])
        },
    );

    let message = match result {
        Ok(_) => panic!("missing-manifest clean root should be rejected"),
        Err(error) => error.to_string(),
    };
    assert!(
        message.contains("aiken.toml"),
        "expected missing-manifest error, got: {message}"
    );
    assert!(
        fixture_root.join("build/verify/manifest.json").exists(),
        "clean must not remove artifacts when the manifest boundary is missing"
    );
    assert!(
        !attempted_cleanup,
        "clean must not run without a manifest boundary"
    );

    fs::remove_dir_all(&fixture_root).expect("fixture root should be removable");
}

#[test]
fn clean_command_rejects_invalid_project_manifests() {
    let fixture_root = unique_temp_dir("verify-clean-invalid-project");
    if fixture_root.exists() {
        fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
    }
    fs::create_dir_all(fixture_root.join("build/verify/AikenVerify"))
        .expect("fixture workspace marker should be creatable");
    fs::create_dir_all(fixture_root.join("build/verify/logs"))
        .expect("fixture logs dir should be creatable");
    fs::write(
        fixture_root.join("aiken.toml"),
        "name = \"aiken-lang/broken\"\nversion = [\n",
    )
    .expect("broken manifest should be writable");
    fs::write(fixture_root.join("build/verify/manifest.json"), "{}")
        .expect("manifest artifact should be writable");
    fs::write(
        fixture_root.join("build/verify/logs/lake_build.stdout.log"),
        "log",
    )
    .expect("log artifact should be writable");

    let mut attempted_cleanup = false;
    let result = run_clean_for_directory_with(
        Some(fixture_root.as_path()),
        PathBuf::from("build/verify"),
        |_path| {
            attempted_cleanup = true;
            Ok(vec![])
        },
    );

    let message = match result {
        Ok(_) => panic!("invalid project manifest should be rejected"),
        Err(error) => error.to_string(),
    };
    assert!(
        message.contains("Failed to parse manifest at"),
        "expected manifest parse error, got: {message}"
    );
    assert!(
        fixture_root.join("build/verify/manifest.json").exists(),
        "clean must not remove artifacts when the project manifest is invalid"
    );
    assert!(
        !attempted_cleanup,
        "clean must not run when the manifest is invalid"
    );

    fs::remove_dir_all(&fixture_root).expect("fixture root should be removable");
}

#[test]
fn clean_command_cleans_workspace_members() {
    let fixture_root = unique_temp_dir("verify-clean-workspace-root");
    if fixture_root.exists() {
        fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
    }

    fs::create_dir_all(fixture_root.join("build/verify/AikenVerify"))
        .expect("workspace root marker should be creatable");
    fs::create_dir_all(fixture_root.join("member-a/build/verify/AikenVerify"))
        .expect("member-a marker should be creatable");
    fs::create_dir_all(fixture_root.join("member-b/build/verify/AikenVerify"))
        .expect("member-b marker should be creatable");
    fs::write(
        fixture_root.join("aiken.toml"),
        "members = [\"member-a\", \"member-b\"]\n",
    )
    .expect("workspace manifest should be writable");
    fs::write(fixture_root.join("build/verify/manifest.json"), "{}")
        .expect("workspace root artifact should be writable");
    fs::write(
        fixture_root.join("member-a/build/verify/manifest.json"),
        "{}",
    )
    .expect("member-a artifact should be writable");
    fs::write(
        fixture_root.join("member-b/build/verify/manifest.json"),
        "{}",
    )
    .expect("member-b artifact should be writable");

    let result = run_clean_for_directory_with(
        Some(fixture_root.as_path()),
        PathBuf::from("build/verify"),
        verify::clean_artifacts,
    )
    .expect("clean should target workspace members");

    assert!(result.output.contains("Removed"));
    assert!(
        fixture_root.join("build/verify/manifest.json").exists(),
        "workspace root artifacts are not member artifacts",
    );
    assert!(
        !fixture_root
            .join("member-a/build/verify/manifest.json")
            .exists(),
        "workspace member-a artifacts should be removed",
    );
    assert!(
        !fixture_root
            .join("member-b/build/verify/manifest.json")
            .exists(),
        "workspace member-b artifacts should be removed",
    );

    fs::remove_dir_all(&fixture_root).expect("fixture root should be removable");
}

#[test]
fn clean_command_ignores_nested_members_config() {
    let fixture_root = unique_temp_dir("verify-clean-nested-members");
    if fixture_root.exists() {
        fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
    }

    fs::create_dir_all(fixture_root.join("build/verify/AikenVerify"))
        .expect("root marker should be creatable");
    fs::create_dir_all(fixture_root.join("member-a/build/verify/AikenVerify"))
        .expect("member marker should be creatable");
    fs::write(
        fixture_root.join("aiken.toml"),
        r#"
name = "test/verify_clean_nested_members"
version = "0.0.0"
plutusVersion = "v3"
description = "verify clean nested-members fixture"

[config.workspace]
members = [1, 2]
"#,
    )
    .expect("fixture manifest should be writable");
    fs::write(fixture_root.join("build/verify/manifest.json"), "{}")
        .expect("root artifact should be writable");
    fs::write(
        fixture_root.join("member-a/build/verify/manifest.json"),
        "{}",
    )
    .expect("member artifact should be writable");

    let result = run_clean_for_directory_with(
        Some(fixture_root.as_path()),
        PathBuf::from("build/verify"),
        verify::clean_artifacts,
    )
    .expect("nested config.members must not trigger workspace handling");

    assert!(result.output.contains("Removed"));
    assert!(
        !fixture_root.join("build/verify/manifest.json").exists(),
        "root artifact should be removed for normal project configs",
    );
    assert!(
        fixture_root
            .join("member-a/build/verify/manifest.json")
            .exists(),
        "nested config.members must not trigger sibling workspace cleanup",
    );

    fs::remove_dir_all(&fixture_root).expect("fixture root should be removable");
}

#[test]
fn clean_command_reports_malformed_workspace_config_errors() {
    let fixture_root = unique_temp_dir("verify-clean-malformed-workspace");
    if fixture_root.exists() {
        fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
    }
    fs::create_dir_all(fixture_root.join("build/verify/AikenVerify"))
        .expect("fixture workspace marker should be creatable");
    fs::write(fixture_root.join("aiken.toml"), "members = [\n")
        .expect("broken workspace manifest should be writable");
    fs::write(fixture_root.join("build/verify/manifest.json"), "{}")
        .expect("manifest artifact should be writable");

    let result = run_clean_for_directory_with(
        Some(fixture_root.as_path()),
        PathBuf::from("build/verify"),
        verify::clean_artifacts,
    );

    let message = match result {
        Ok(_) => panic!("malformed workspace config should be reported"),
        Err(error) => error.to_string(),
    };
    assert!(
        message.contains("Failed to parse manifest at"),
        "expected manifest parse error, got: {message}"
    );
    assert!(
        fixture_root.join("build/verify/manifest.json").exists(),
        "clean must not silently remove artifacts when the workspace config is malformed"
    );

    fs::remove_dir_all(&fixture_root).expect("fixture root should be removable");
}

#[test]
fn capabilities_command_branch_reports_output_and_exit_code() {
    let text_result = run_capabilities_command(CapabilitiesArgs { json: false })
        .expect("capabilities text output should render");
    assert_eq!(text_result.exit_code, 0);
    assert!(
        text_result.output.contains("Verification Capabilities"),
        "capabilities text output should include section header"
    );
    assert!(
        text_result.output.contains("Max test arity: 1"),
        "capabilities text output should include max arity"
    );
    assert!(
        text_result.output.contains("Solver profiles:")
            && text_result.output.contains("strict-cert")
            && text_result.output.contains("LeanCertified support"),
        "capabilities text output should surface solver/certification sections"
    );

    let json_result = run_capabilities_command(CapabilitiesArgs { json: true })
        .expect("capabilities JSON output should render");
    assert_eq!(json_result.exit_code, 0);
    let value: serde_json::Value =
        serde_json::from_str(&json_result.output).expect("capabilities JSON should parse");
    assert_eq!(value["version"], verify::VERIFICATION_CAPABILITIES_VERSION);
    assert_eq!(value["max_test_arity"], 1);
    assert_eq!(value["supported"][0], "property");
    assert!(value["solver_profiles"].as_array().is_some());
    assert_eq!(value["solver_profiles"][7], "strict-cert");
    assert_eq!(value["trust_profiles"][1], "production");
    assert_eq!(
        value["certification"]["solver_validated"]["available"],
        true
    );
    assert_eq!(value["certification"]["lean_certified"]["available"], false);
    assert_eq!(
        value["backends"]["cardano_ledger_api_blaster"]["available"],
        false
    );
}

fn sample_local_doctor_report() -> verify::DoctorReport {
    let mut report = verify::run_doctor("blaster-rev", "configured-plutus-rev");
    report.plutus_core.found = true;
    report.plutus_core.path = "/tmp/plutus-core".to_string();
    report.plutus_core.has_lakefile = true;
    report.plutus_core.error = None;
    report.plutus_core_rev = "configured-plutus-rev".to_string();
    report.all_ok = true;
    report.backends.plutus_core_blaster.available = true;
    report.backends.plutus_core_blaster.note = Some(
        "PlutusCoreBlaster dependency is configured via /tmp/plutus-core (rev configured-plutus-rev)."
            .to_string(),
    );
    report.certification.solver_validated.available = true;
    report.certification.lean_certified.available = false;
    report.certification.proof_reconstruction.available = false;
    report.certification.strict_cert_profile.available = false;
    report
}

#[test]
fn doctor_local_plutus_core_does_not_claim_configured_rev() {
    let mut report = sample_local_doctor_report();

    normalize_doctor_report_provenance(&mut report);

    assert_eq!(
        report.plutus_core_rev,
        "<local checkout; git revision not inspected>"
    );
    assert!(
        report
            .backends
            .plutus_core_blaster
            .note
            .as_deref()
            .expect("backend note")
            .contains("local checkout /tmp/plutus-core")
    );
}

#[test]
fn doctor_readiness_includes_certification_capabilities() {
    let mut report = sample_local_doctor_report();

    normalize_doctor_report_provenance(&mut report);

    assert!(!report.all_ok);
    assert_eq!(doctor_exit_code(&report), 1);
}

#[test]
fn verify_summary_json_contract_snapshot() {
    let theorem_results = vec![
        verify::TheoremResult::new_with_domain(
            "example.test_proved".to_string(),
            "Example.test_proved".to_string(),
            verify::ProofStatus::Proved,
            0,
            verify::TrustProfile::Production,
            trusted_domain(),
            Some(trusted_input_type()),
            Some(verify::ManifestTestMode::Normal),
            Some(aiken_project::export::TestReturnMode::Bool),
        ),
        verify::TheoremResult::new_with_domain(
            "example.test_witness".to_string(),
            "Example.test_witness".to_string(),
            verify::ProofStatus::WitnessProved {
                instances: 2,
                witnesses: vec!["00".to_string(), "01".to_string()],
                note: "witness-only proof".to_string(),
            },
            0,
            verify::TrustProfile::Production,
            witness_domain(&["00", "01"]),
            Some(trusted_input_type()),
            Some(verify::ManifestTestMode::FailOnce),
            Some(aiken_project::export::TestReturnMode::Bool),
        ),
        verify::TheoremResult::new_with_domain(
            "example.test_partial".to_string(),
            "Example.test_partial".to_string(),
            verify::ProofStatus::Partial {
                note: "partial proof".to_string(),
            },
            2,
            verify::TrustProfile::Production,
            partial_domain("partial proof"),
            Some(trusted_input_type()),
            Some(verify::ManifestTestMode::Normal),
            Some(aiken_project::export::TestReturnMode::Bool),
        ),
        verify::TheoremResult::new_with_domain(
            "example.test_failed".to_string(),
            "Example.test_failed".to_string(),
            verify::ProofStatus::Failed {
                category: verify::FailureCategory::BuildError,
                reason: "build failed".to_string(),
            },
            0,
            verify::TrustProfile::Production,
            trusted_domain(),
            Some(trusted_input_type()),
            Some(verify::ManifestTestMode::Normal),
            Some(aiken_project::export::TestReturnMode::Bool),
        ),
        verify::TheoremResult::new_with_domain(
            "example.test_timed_out".to_string(),
            "Example.test_timed_out".to_string(),
            verify::ProofStatus::TimedOut {
                reason: "timed out".to_string(),
            },
            0,
            verify::TrustProfile::Production,
            trusted_domain(),
            Some(trusted_input_type()),
            Some(verify::ManifestTestMode::Normal),
            Some(aiken_project::export::TestReturnMode::Bool),
        ),
        verify::TheoremResult::new_with_domain(
            "example.test_unknown".to_string(),
            "Example.test_unknown".to_string(),
            verify::ProofStatus::Unknown,
            0,
            verify::TrustProfile::Production,
            trusted_domain(),
            Some(trusted_input_type()),
            Some(verify::ManifestTestMode::Normal),
            Some(aiken_project::export::TestReturnMode::Bool),
        ),
    ];
    let mut artifacts = verify::VerificationArtifacts::default();
    artifacts.manifest = Some(PathBuf::from("/workspace/build/verify/manifest.json"));
    artifacts.lean_root = Some(PathBuf::from("/workspace/build/verify/AikenVerify"));
    artifacts.logs = Some(PathBuf::from("/workspace/build/verify/logs"));
    artifacts.smt2 = vec![PathBuf::from(
        "/workspace/build/verify/Artifacts/query.smt2",
    )];
    let mut summary = verify::VerifySummary::new(
        6,
        1,
        1,
        1,
        1,
        1,
        1,
        vec![verify::SkippedTest::new(
            "example.test_skip".to_string(),
            "example".to_string(),
            "unsupported shape".to_string(),
        )],
        theorem_results.clone(),
        verify::VerifyResult::new(
            false,
            verify::CapturedOutput::new(
                "stdout tail".to_string(),
                2048,
                true,
                Some(PathBuf::from(
                    "/workspace/build/verify/logs/lake_build.stdout.log",
                )),
            ),
            verify::CapturedOutput::new(
                "stderr tail".to_string(),
                1024,
                true,
                Some(PathBuf::from(
                    "/workspace/build/verify/logs/lake_build.stderr.log",
                )),
            ),
            Some(1),
            Some(theorem_results),
        ),
        artifacts,
        Some(3210),
        None,
        None,
        false,
        "abc123".to_string(),
        "pc456".to_string(),
        true,
        true,
    );
    summary.run_settings = Some(verify::VerificationRunSettings::new(
        verify::SolverProfile::Symbolic,
        verify::TrustProfile::Production,
        300,
        200_000,
    ));
    let mut cache = verify::WorkspaceCacheReport::new(true);
    cache.generated_lean = verify::WorkspaceCacheCounters::new(
        4,
        1,
        Some("generated Lean files reused across reruns".to_string()),
    );
    cache.compiled_uplc = verify::WorkspaceCacheCounters::new(
        2,
        0,
        Some("compiled UPLC payloads reused across reruns".to_string()),
    );
    cache.dependency_builds_preserved = true;
    cache.dependency_builds_note =
        Some("Lake dependency builds were preserved under .lake/build.".to_string());
    cache.solver_artifacts_preserved = true;
    cache.solver_artifacts_note =
        Some("Existing SMT/log artifacts were preserved for reuse and inspection.".to_string());
    summary.cache = Some(cache);

    normalize_summary_artifact_paths(&mut summary, Path::new("/workspace"), true);

    let json = serde_json::to_value(&summary).expect("summary should serialize");
    assert!(json.get("allow_vacuous_subgenerators").is_some());
    assert!(json.get("two_phase_disabled").is_some());
    assert_eq!(json["artifacts"]["manifest"], "build/verify/manifest.json");
    assert_eq!(json["artifacts"]["lean_root"], "build/verify/AikenVerify");
    assert_eq!(json["artifacts"]["logs"], "build/verify/logs");
    assert_eq!(
        json["artifacts"]["smt2"],
        serde_json::json!(["build/verify/Artifacts/query.smt2"])
    );
    assert_eq!(
        json["raw_output"]["stdout"]["log_path"],
        "build/verify/logs/lake_build.stdout.log"
    );
    assert_eq!(
        json["raw_output"]["stderr"]["log_path"],
        "build/verify/logs/lake_build.stderr.log"
    );
    assert_eq!(json["run_settings"]["solver_profile"], "symbolic");
    assert_eq!(json["run_settings"]["trust_profile"], "production");
    assert_eq!(json["run_settings"]["timeout_secs"], 300);
    assert_eq!(json["run_settings"]["cek_fuel"], 200000);
    assert_eq!(json["cache"]["generated_lean"]["reused_files"], 4);
    assert_eq!(json["cache"]["compiled_uplc"]["reused_files"], 2);
    insta::assert_json_snapshot!("verify_summary_json_contract", &summary);
}

#[test]
fn normalize_summary_artifact_paths_clears_deleted_artifact_paths() {
    let mut artifacts = verify::VerificationArtifacts::default();
    artifacts.manifest = Some(PathBuf::from("/workspace/build/verify/manifest.json"));
    artifacts.lean_root = Some(PathBuf::from("/workspace/build/verify/AikenVerify"));
    artifacts.logs = Some(PathBuf::from("/workspace/build/verify/logs"));
    artifacts.smt2 = vec![PathBuf::from(
        "/workspace/build/verify/Artifacts/query.smt2",
    )];
    let mut summary = verify::VerifySummary::new(
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        vec![],
        vec![],
        verify::VerifyResult::new(
            true,
            verify::CapturedOutput::new(
                String::new(),
                0,
                true,
                Some(PathBuf::from(
                    "/workspace/build/verify/logs/lake_build.stdout.log",
                )),
            ),
            verify::CapturedOutput::new(
                String::new(),
                0,
                true,
                Some(PathBuf::from(
                    "/workspace/build/verify/logs/lake_build.stderr.log",
                )),
            ),
            Some(0),
            Some(vec![]),
        ),
        artifacts,
        None,
        None,
        None,
        true,
        String::new(),
        String::new(),
        false,
        false,
    );

    normalize_summary_artifact_paths(&mut summary, Path::new("/workspace"), false);

    assert!(summary.raw_output.stdout.log_path.is_none());
    assert!(summary.raw_output.stderr.log_path.is_none());
    assert!(summary.artifacts.is_empty());
}

#[test]
fn failure_artifact_advice_points_at_logs_when_artifacts_are_retained() {
    let advice = failure_artifact_advice(Path::new("/workspace/build/verify"), true);

    assert_eq!(
        advice,
        vec![
            "Logs available at /workspace/build/verify/logs/".to_string(),
            "To reproduce: cd /workspace/build/verify && lake build".to_string(),
        ]
    );
}

#[test]
fn failure_artifact_advice_suggests_rerun_when_cleanup_removed_workspace() {
    let advice = failure_artifact_advice(Path::new("/workspace/build/verify"), false);

    assert_eq!(
        advice,
        vec![
            "Artifacts were cleaned up after this run. Re-run with --artifacts always to keep the generated workspace and logs.".to_string(),
        ]
    );
}

#[test]
fn generated_manifest_json_contract_snapshot() {
    let manifest = sample_generated_manifest();
    insta::assert_json_snapshot!("generated_manifest_json_contract", &manifest);
}

#[test]
fn generated_manifest_debug_renderers_surface_mode_and_execution_metadata() {
    let manifest = sample_generated_manifest();
    assert_eq!(
        render_manifest_debug_header(&manifest),
        "Manifest schema v2; plutus_version=v3; cek_fuel=200000; decode_policy=decode_error_is_test_failure"
    );
    assert_eq!(
        render_manifest_entry_debug(&manifest.tests[0]),
        "example -> Example.TestOk [return_mode=bool, test_mode=normal, on_test_failure=fail_immediately] domain=OverApprox/TrustedVersionedModel sampler=seeded_generation/seeded"
    );
}

#[test]
fn verify_cli_error_json_contract_snapshot() {
    let payload = verify_cli_error_payload("project load failed");
    insta::assert_json_snapshot!("verify_cli_error_json_contract", &payload);
}

#[test]
fn verify_cli_error_message_uses_fallback_for_empty_reports() {
    let message = verify_cli_error_message(&ExitFailure::into_report());
    assert_eq!(message, VERIFY_CLI_ERROR_FALLBACK_MESSAGE);
}

#[test]
fn verify_cli_error_message_preserves_non_empty_reports() {
    let message = verify_cli_error_message(&ExitFailure::with_message("unknown module"));
    assert_eq!(message, "unknown module");
}

fn denied_warning_report() -> miette::Report {
    ExitFailure::with_message(
        "Warnings were denied by --deny.\n\nI came across a validator in a lib/ module which means I'm going to ignore it.",
    )
}

#[test]
fn warning_only_deny_failure_surfaces_in_json_mode() {
    let payload = verify_cli_error_payload(verify_cli_error_message(&denied_warning_report()));
    assert_eq!(payload["kind"], "verify-cli-error");
    let message = payload["message"]
        .as_str()
        .expect("verify CLI error payload should carry a string message");
    assert!(message.contains("Warnings were denied by --deny"));
    assert!(message.contains("validator"));
}

#[test]
fn doctor_report_json_contract_snapshot() {
    let report = sample_doctor_report(true, "abc123", "pc456");
    insta::assert_json_snapshot!("doctor_report_json_contract", &report);
}

#[test]
fn capabilities_json_contract_snapshot() {
    let caps = verify::capabilities();
    insta::assert_json_snapshot!("capabilities_json_contract", &caps);
}

#[test]
fn no_proofs_summary_json_includes_flags_and_version_fields() {
    let summary = no_proofs_summary(
        &sample_generated_manifest(),
        true,
        "abc123",
        "pc456",
        true,
        true,
    );
    let json = serde_json::to_value(&summary).expect("summary should serialize");
    assert_eq!(
        json["verify_summary_version"],
        verify::VERIFY_SUMMARY_VERSION
    );
    assert_eq!(json["allow_vacuous_subgenerators"], true);
    assert_eq!(json["two_phase_disabled"], true);
}

#[test]
fn extract_counterexample_display_multiline_bytestring() {
    let reason = "\
error: AikenVerify/Proofs/Foo.lean:15:5: Counterexample:
error: AikenVerify/Proofs/Foo.lean:15:5: - x: (PlutusCore.ByteString.PlutusCore.ByteStringInternal.ByteString.mk
  \"Hello World.\")
error: AikenVerify/Proofs/Foo.lean:15:5: Tactic `blaster` failed: Goal was falsified";

    assert_eq!(
        extract_counterexample_display(reason),
        Some("\"Hello World.\"".to_string())
    );
}

#[test]
fn extract_counterexample_display_inline_assignment() {
    let reason = "error: Foo.lean:1:1: Counterexample: x = 42";
    assert_eq!(
        extract_counterexample_display(reason),
        Some("42".to_string())
    );
}

#[test]
fn extract_counterexample_display_multiple_inputs() {
    let reason = "\
error: Foo.lean:1:1: Counterexample:
error: Foo.lean:1:1: - x: 1
error: Foo.lean:1:1: - y: True
error: Foo.lean:1:1: Tactic `blaster` failed";

    assert_eq!(
        extract_counterexample_display(reason),
        Some("x = 1, y = True".to_string())
    );
}

#[test]
fn solver_counterexample_label_prefers_replay_confirmed_input() {
    let reason = "error: Foo.lean:1:1: Counterexample: x = 42";
    let mut theorem = verify::TheoremResult::new(
        "example.test".to_string(),
        "example_theorem".to_string(),
        verify::ProofStatus::Failed {
            category: verify::FailureCategory::Counterexample,
            reason: reason.to_string(),
        },
        0,
    );
    theorem.counterexample = Some(
        serde_json::from_value(serde_json::json!({
            "classification": "confirmed_by_replay",
            "replay_status": "confirmed",
            "input_source_value": "41",
            "raw_model_text": reason,
            "property_outcome": "returns_false",
            "replay_note": "confirmed replay"
        }))
        .unwrap(),
    );
    assert_eq!(
        solver_counterexample_label(&theorem, reason),
        "SOLVER FALSIFIED [smt_counterexample]: 41"
    );
}

#[test]
fn solver_counterexample_label_marks_potential_models() {
    let reason = "error: Foo.lean:1:1: Counterexample: x = 42";
    let mut theorem = verify::TheoremResult::new(
        "example.test".to_string(),
        "example_theorem".to_string(),
        verify::ProofStatus::Failed {
            category: verify::FailureCategory::Counterexample,
            reason: reason.to_string(),
        },
        0,
    );
    theorem.counterexample = Some(
        serde_json::from_value(serde_json::json!({
            "classification": "potential",
            "replay_status": "not_attempted",
            "input_source_value": "41",
            "raw_model_text": reason,
            "replay_note": "potential replay"
        }))
        .unwrap(),
    );
    assert_eq!(
        solver_counterexample_label(&theorem, reason),
        "SOLVER FALSIFIED [smt_counterexample]: potential counterexample"
    );
}

#[test]
fn sanitize_stderr_for_display_removes_generic_build_failed_line() {
    let stderr = "\
error: Foo.lean:15:5: Counterexample:
error: build failed
error: Foo.lean:15:5: Tactic `blaster` failed";

    assert_eq!(
        sanitize_stderr_for_display(stderr),
        "error: Foo.lean:15:5: Counterexample:\nerror: Foo.lean:15:5: Tactic `blaster` failed"
    );
}

#[test]
fn generate_only_preflight_helper_does_not_enforce_explicit_bounds() {
    let tests = vec![dummy_property_test(
        "example.test_any_int",
        FuzzerOutputType::Int,
        FuzzerConstraint::Any,
    )];

    let unsupported = collect_generate_only_preflight_errors_with(&tests, |_t| Ok(()));
    assert!(
        unsupported.is_empty(),
        "helper must only report validator failures"
    );
}

#[test]
fn generate_only_preflight_helper_reports_validator_errors() {
    let tests = vec![
        dummy_property_test(
            "example.test_ok",
            FuzzerOutputType::Int,
            FuzzerConstraint::Any,
        ),
        dummy_property_test(
            "example.test_bad",
            FuzzerOutputType::Int,
            FuzzerConstraint::Any,
        ),
    ];

    let unsupported = collect_generate_only_preflight_errors_with(&tests, |t| {
        if t.name.ends_with("bad") {
            Err(miette::miette!("unsupported shape"))
        } else {
            Ok(())
        }
    });

    assert_eq!(unsupported, vec!["example.test_bad: unsupported shape"]);
}

#[test]
fn no_property_tests_output_is_json_in_json_mode() {
    let output =
        format_no_property_tests_output(OutputMode::Json, "custom-blaster", "custom-plutus", true)
            .expect("json mode output should serialize successfully");
    let value: serde_json::Value =
        serde_json::from_str(&output).expect("json mode output should be valid JSON");

    assert_eq!(
        value["verify_summary_version"],
        verify::VERIFY_SUMMARY_VERSION
    );
    assert_eq!(value["blaster_rev"], "custom-blaster");
    assert_eq!(value["plutus_core_rev"], "custom-plutus");
    assert_eq!(value["allow_vacuous_subgenerators"], true);
    assert_eq!(value["total"], 0);
    assert!(value["theorems"].as_array().unwrap().is_empty());
}

#[test]
fn no_property_tests_early_output_handles_empty_and_non_empty_slices() {
    let no_tests: Vec<ExportedPropertyTest> = vec![];
    let output = no_property_tests_early_output(
        &no_tests,
        OutputMode::Json,
        "custom-blaster",
        "custom-plutus",
        true,
    )
    .expect("empty slice should produce serializable JSON output")
    .expect("empty slice should return Some(output)");
    let value: serde_json::Value =
        serde_json::from_str(&output).expect("empty-slice output should be valid JSON");
    assert_eq!(
        value["verify_summary_version"],
        verify::VERIFY_SUMMARY_VERSION
    );
    assert_eq!(value["blaster_rev"], "custom-blaster");
    assert_eq!(value["plutus_core_rev"], "custom-plutus");
    assert_eq!(value["allow_vacuous_subgenerators"], true);
    assert_eq!(value["total"], 0);
    assert!(value["theorems"].as_array().unwrap().is_empty());

    let one_test = vec![dummy_property_test(
        "example.test_ok",
        FuzzerOutputType::Int,
        FuzzerConstraint::Any,
    )];
    let output = no_property_tests_early_output(
        &one_test,
        OutputMode::Json,
        "custom-blaster",
        "custom-plutus",
        true,
    )
    .expect("non-empty slice should not fail");
    assert!(
        output.is_none(),
        "non-empty slice should not trigger early no-tests output"
    );
}

#[test]
fn run_proofs_start_output_is_suppressed_in_json_mode() {
    assert_eq!(run_proofs_start_output(OutputMode::Json), None);
    assert_eq!(run_proofs_start_output(OutputMode::Silent), None);
}

#[test]
fn run_proofs_start_output_is_present_in_text_mode() {
    assert_eq!(
        run_proofs_start_output(OutputMode::Text),
        Some("Running proofs via lake build...")
    );
}

#[test]
fn no_proofs_summary_reports_skipped_metadata_and_failure_status() {
    let manifest = skipped_only_manifest();

    let summary = no_proofs_summary(
        &manifest,
        true,
        verify::DEFAULT_BLASTER_REV,
        verify::DEFAULT_PLUTUS_CORE_REV,
        false,
        false,
    );

    assert_eq!(summary.total, 0);
    assert_eq!(summary.skipped.len(), 1);
    assert_eq!(summary.skipped[0].name, "example.test_unsupported");
    assert!(!summary.raw_output.success);
    assert_eq!(summary.raw_output.exit_code, Some(1));

    let json = serde_json::to_value(summary).expect("summary should serialize");
    assert_eq!(
        json["skipped"][0]["name"],
        serde_json::Value::String("example.test_unsupported".to_string())
    );
}

#[test]
fn no_proofs_summary_marks_success_when_skips_are_allowed() {
    let manifest = skipped_only_manifest();

    let summary = no_proofs_summary(
        &manifest,
        false,
        verify::DEFAULT_BLASTER_REV,
        verify::DEFAULT_PLUTUS_CORE_REV,
        false,
        false,
    );

    assert!(summary.raw_output.success);
    assert_eq!(summary.raw_output.exit_code, Some(0));
}

#[test]
fn no_proofs_summary_command_success_false_when_skips_disallowed() {
    let manifest = skipped_only_manifest();

    let summary = no_proofs_summary(
        &manifest,
        true,
        verify::DEFAULT_BLASTER_REV,
        verify::DEFAULT_PLUTUS_CORE_REV,
        false,
        false,
    );
    assert!(!summary.command_success);

    let json = serde_json::to_value(&summary).expect("summary should serialize");
    assert_eq!(json["command_success"], serde_json::Value::Bool(false));
}

#[test]
fn no_proofs_summary_command_success_true_when_skips_allowed() {
    let manifest = skipped_only_manifest();

    let summary = no_proofs_summary(
        &manifest,
        false,
        verify::DEFAULT_BLASTER_REV,
        verify::DEFAULT_PLUTUS_CORE_REV,
        false,
        false,
    );
    assert!(summary.command_success);

    let json = serde_json::to_value(&summary).expect("summary should serialize");
    assert_eq!(json["command_success"], serde_json::Value::Bool(true));
}

#[test]
fn no_proofs_summary_stamps_blaster_and_plutus_core_revs() {
    // Provenance: even on the early-exit "no property tests remain"
    // branch, the JSON summary MUST surface the resolved Blaster /
    // PlutusCore revs so consumers can audit which dependency snapshot
    // the (zero) proofs would have run against. Covers both the default
    // and a non-default override path.
    let manifest = verify::GeneratedManifest::empty(verify::GENERATE_ONLY_VERSION.to_string());

    let default_summary = no_proofs_summary(
        &manifest,
        false,
        verify::DEFAULT_BLASTER_REV,
        verify::DEFAULT_PLUTUS_CORE_REV,
        false,
        false,
    );
    assert_eq!(default_summary.blaster_rev, verify::DEFAULT_BLASTER_REV);
    assert_eq!(
        default_summary.plutus_core_rev,
        verify::DEFAULT_PLUTUS_CORE_REV
    );

    let json = serde_json::to_value(&default_summary).expect("summary should serialize");
    assert_eq!(
        json["blaster_rev"],
        serde_json::Value::String(verify::DEFAULT_BLASTER_REV.to_string())
    );
    assert_eq!(
        json["plutus_core_rev"],
        serde_json::Value::String(verify::DEFAULT_PLUTUS_CORE_REV.to_string())
    );

    // Override path: confirm an arbitrary `--blaster-rev` /
    // `--plutus-core-rev` value flows into the summary unchanged.
    let override_summary = no_proofs_summary(
        &manifest,
        false,
        "abc1234",
        "feature/experimental",
        false,
        false,
    );
    assert_eq!(override_summary.blaster_rev, "abc1234");
    assert_eq!(override_summary.plutus_core_rev, "feature/experimental");
}

#[test]
fn parsed_summary_stamps_blaster_and_plutus_core_revs_via_run_options() {
    // Provenance: on the standard parse-then-stamp codepath (lines
    // ~1040 of cmd/verify.rs), the CLI mutates the summary to carry
    // the resolved revs from RunCommandOptions. Reproduce that pattern
    // here and verify both fields land in the JSON output.
    let manifest = single_test_manifest(false);

    let raw = successful_raw_result();

    let run_options = fixture_run_options(
        PathBuf::from("build/verify-blaster-rev-stamp"),
        VerificationTargetKind::default(),
    );

    let mut summary =
        verify::parse_verify_results(raw, &manifest, verify::VerifyParseContext::for_tests());
    // Pre-stamp: library default is empty.
    assert_eq!(summary.blaster_rev, "");
    assert_eq!(summary.plutus_core_rev, "");

    // Mirror the production stamping in cmd/verify.rs.
    summary.blaster_rev.clone_from(&run_options.blaster_rev);
    summary
        .plutus_core_rev
        .clone_from(&run_options.plutus_core_rev);

    assert_eq!(summary.blaster_rev, DEFAULT_BLASTER_REV);
    assert_eq!(summary.plutus_core_rev, DEFAULT_PLUTUS_CORE_REV);

    let json = serde_json::to_value(&summary).expect("summary should serialize");
    assert_eq!(
        json["blaster_rev"],
        serde_json::Value::String(DEFAULT_BLASTER_REV.to_string())
    );
    assert_eq!(
        json["plutus_core_rev"],
        serde_json::Value::String(DEFAULT_PLUTUS_CORE_REV.to_string())
    );
}

#[test]
fn command_success_in_json_reflects_skip_failure() {
    // Simulate a summary where proofs passed but skips cause failure.
    let manifest = single_test_manifest(true);

    let raw = successful_raw_result();

    let mut summary =
        verify::parse_verify_results(raw, &manifest, verify::VerifyParseContext::for_tests());
    // Before skip adjustment, proofs passed so command_success is true.
    assert!(summary.command_success);

    // Now apply skip-induced failure (mirrors the cmd layer logic).
    let skipped_without_allow = skips_require_failure(manifest.skipped.len(), false);
    if skipped_without_allow {
        summary.command_success = false;
    }

    assert!(!summary.command_success);
    let json = serde_json::to_value(&summary).expect("summary should serialize");
    assert_eq!(json["command_success"], serde_json::Value::Bool(false));
}

#[test]
fn verify_run_all_skipped_bypasses_plutus_core_check_when_skips_allowed() {
    let fixture_root = unique_temp_dir("aiken-verify-skip-only");
    if fixture_root.exists() {
        fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
    }
    fs::create_dir_all(&fixture_root).expect("fixture root should be creatable");
    write_verify_skip_only_fixture(&fixture_root);

    let mut run_options = fixture_run_options(
        PathBuf::from("build/verify-skip-only"),
        VerificationTargetKind::default(),
    );
    run_options.generate_only = false;
    run_options.skip_policy = SkipPolicy::All;
    run_options.allow_skips = true;
    run_options.target = VerificationTargetKind::ValidatorHandler;

    let mut project = Project::new(fixture_root.clone(), EventTarget::default())
        .expect("fixture project should load");
    let result = exec_run_with_project(&mut project, &run_options);
    assert!(
        result.is_ok(),
        "skip-only runs should not require PlutusCore when there are no runnable proofs: {result:?}"
    );

    fs::remove_dir_all(fixture_root).expect("fixture root should be removable");
}

#[test]
fn verify_run_generate_only_supports_non_default_targets() {
    let fixture_root = unique_temp_dir("aiken-verify-target-modes");
    if fixture_root.exists() {
        fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
    }
    fs::create_dir_all(&fixture_root).expect("fixture root should be creatable");
    write_verify_target_fixture(&fixture_root);

    for target in [
        VerificationTargetKind::ValidatorHandler,
        VerificationTargetKind::Equivalence,
    ] {
        let out_dir = PathBuf::from(format!("build/verify-{target}"));
        let mut run_options = fixture_run_options(out_dir.clone(), target.clone());
        run_options.artifact_policy = ArtifactRetention::Always;

        let mut project = Project::new(fixture_root.clone(), EventTarget::default())
            .expect("fixture project should load");
        if let Err(errors) = exec_run_with_project(&mut project, &run_options) {
            panic!("verify run should support --target {target}; got errors: {errors:#?}");
        }

        let resolved_out_dir = fixture_root.join(&out_dir);
        let manifest_path = resolved_out_dir.join("manifest.json");
        assert!(
            manifest_path.exists(),
            "--target {target} should generate manifest.json"
        );
        let manifest_json =
            fs::read_to_string(&manifest_path).expect("generated manifest should be readable");
        let manifest_value: serde_json::Value =
            serde_json::from_str(&manifest_json).expect("manifest should be valid JSON");

        let tests = manifest_value["tests"]
            .as_array()
            .map(|t| t.len())
            .unwrap_or(0);
        let skipped = manifest_value["skipped"]
            .as_array()
            .map(|s| s.len())
            .unwrap_or(0);
        assert!(tests > 0, "--target {target} should emit runnable tests");
        assert_eq!(
            skipped, 0,
            "--target {target} should not succeed with only skipped tests"
        );
    }

    fs::remove_dir_all(fixture_root).expect("fixture root should be removable");
}

#[test]
fn verify_run_unit_only_projects_return_no_proofs_without_crashing() {
    let fixture_root = unique_temp_dir("aiken-verify-unit-only");
    if fixture_root.exists() {
        fs::remove_dir_all(&fixture_root).expect("stale fixture root should be removable");
    }
    fs::create_dir_all(&fixture_root).expect("fixture root should be creatable");
    write_verify_unit_only_fixture(&fixture_root);

    let run_options = fixture_run_options(
        PathBuf::from("build/verify-unit-only"),
        VerificationTargetKind::default(),
    );
    let mut project = Project::new(fixture_root.clone(), EventTarget::default())
        .expect("fixture project should load");
    let result = exec_run_with_project(&mut project, &run_options);
    assert!(
        result.is_ok(),
        "unit-only verify runs should return the no-proofs summary instead of crashing: {result:?}"
    );

    fs::remove_dir_all(fixture_root).expect("fixture root should be removable");
}

#[test]
fn run_compile_options_forwards_env() {
    let options = run_compile_options(Some("staging".to_string()));
    assert_eq!(options.env.as_deref(), Some("staging"));
}

#[test]
fn resolve_verify_out_dir_uses_project_root_for_relative_paths() {
    let project_root = PathBuf::from("workspace/member-a");
    let out_dir = PathBuf::from("build/verify");

    assert_eq!(
        resolve_verify_out_dir(&out_dir, &project_root)
            .expect("relative out_dir without traversal should resolve"),
        project_root.join("build/verify")
    );
}

#[test]
fn resolve_verify_out_dir_accepts_absolute_paths_inside_project_root() {
    let cwd = std::env::current_dir().unwrap();
    let project_root = cwd.join("member-a");
    let out_dir = project_root.join("build/verify");

    assert_eq!(
        resolve_verify_out_dir(&out_dir, &project_root)
            .expect("absolute out_dir without traversal should be accepted"),
        out_dir
    );
}

#[test]
fn resolve_verify_out_dir_rejects_absolute_paths_outside_project_root() {
    let cwd = std::env::current_dir().unwrap();
    let project_root = cwd.join("member-a");
    let outside_out_dir = cwd.join("verify-output");

    let err = resolve_verify_out_dir(&outside_out_dir, &project_root)
        .expect_err("absolute out_dir outside project root should be rejected");

    assert!(
        err.to_string().contains("inside project root"),
        "unexpected outside-project error message: {err}"
    );
}

#[test]
fn resolve_verify_out_dir_rejects_parent_traversal_segments() {
    let project_root = PathBuf::from("workspace/member-a");
    let err = resolve_verify_out_dir(Path::new("../outside"), &project_root)
        .expect_err("parent traversal should be rejected");

    assert!(
        err.to_string().contains("parent directory segments"),
        "unexpected traversal error message: {err}"
    );
}

#[cfg(unix)]
#[test]
fn resolve_verify_out_dir_rejects_symlink_escapes() {
    let fixture_root = unique_temp_dir("aiken-verify-out-dir-symlink");
    let project_root = fixture_root.join("project");
    let outside_dir = fixture_root.join("outside");
    fs::create_dir_all(&project_root).expect("project root should be creatable");
    fs::create_dir_all(&outside_dir).expect("outside directory should be creatable");
    symlink(&outside_dir, project_root.join("escaped"))
        .expect("escape symlink should be creatable");

    let err = resolve_verify_out_dir(Path::new("escaped/generated"), &project_root)
        .expect_err("symlink escape should be rejected");
    assert!(
        err.to_string().contains("inside project root"),
        "unexpected symlink-escape error message: {err}"
    );

    fs::remove_dir_all(fixture_root).expect("fixture root should be removable");
}

#[test]
fn run_args_parse_env_flag() {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    let parsed =
        VerifyCli::try_parse_from(["aiken-verify", "run", "--env", "staging", "--generate-only"])
            .expect("`verify run --env` should parse");

    let Cmd::Run(args) = parsed.cmd else {
        panic!("expected `run` subcommand")
    };
    assert_eq!(args.env.as_deref(), Some("staging"));
}

#[test]
fn run_args_directory_and_relative_out_dir_resolve_under_project_root() {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    let cwd = std::env::current_dir().expect("test process must be able to determine current dir");
    let project_root = cwd.join("example-project");
    let project_root_arg = project_root.to_string_lossy().into_owned();

    let parsed = VerifyCli::try_parse_from([
        "aiken-verify",
        "run",
        project_root_arg.as_str(),
        "--out-dir",
        "build/verify",
        "--generate-only",
    ])
    .expect("`verify run <project-dir> --out-dir build/verify` should parse");

    let Cmd::Run(args) = parsed.cmd else {
        panic!("expected `run` subcommand")
    };

    assert_eq!(
        resolve_verify_out_dir(&args.out_dir, &project_root)
            .expect("run args out_dir should resolve under project root"),
        project_root.join("build/verify")
    );
}

#[test]
fn doctor_args_reject_project_directory_positionals() {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    let cwd = std::env::current_dir().expect("test process must be able to determine current dir");
    let project_root = cwd.join("example-project");
    let project_root_arg = project_root.to_string_lossy().into_owned();

    assert!(
        VerifyCli::try_parse_from(["aiken-verify", "doctor", project_root_arg.as_str()]).is_err(),
        "`verify doctor` should reject positional project directories because the command is global",
    );
}

#[test]
fn doctor_can_run_without_an_aiken_project() {
    let result = run_doctor_command_with(
        true,
        "abc123".to_string(),
        "pc-rev".to_string(),
        |blaster_rev, plutus_core_rev| {
            let mut report = sample_doctor_report(true, blaster_rev, plutus_core_rev);
            report.certification.lean_certified.available = true;
            report.certification.proof_reconstruction.available = true;
            report.certification.strict_cert_profile.available = true;
            report
        },
    )
    .expect("doctor should run without requiring aiken.toml");

    assert_eq!(result.exit_code, 0);
    let value: serde_json::Value =
        serde_json::from_str(&result.output).expect("doctor JSON output should parse");
    assert_eq!(value["blaster_rev"], "abc123");
    assert_eq!(value["plutus_core_rev"], "pc-rev");
}

#[test]
fn clean_args_directory_and_relative_out_dir_resolve_under_project_root() {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    let cwd = std::env::current_dir().expect("test process must be able to determine current dir");
    let project_root = cwd.join("example-project");
    let project_root_arg = project_root.to_string_lossy().into_owned();

    let parsed = VerifyCli::try_parse_from([
        "aiken-verify",
        "clean",
        project_root_arg.as_str(),
        "--out-dir",
        "build/verify",
    ])
    .expect("`verify clean <project-dir> --out-dir build/verify` should parse");

    let Cmd::Clean(args) = parsed.cmd else {
        panic!("expected `clean` subcommand")
    };

    assert_eq!(args.directory, Some(project_root.clone()));
    assert_eq!(
        resolve_verify_out_dir(&args.out_dir, &project_root)
            .expect("clean args out_dir should resolve under project root"),
        project_root.join("build/verify")
    );
}

fn render_verify_subcommand_long_help(name: &str) -> String {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    let mut command = VerifyCli::command();
    let subcommand = command
        .find_subcommand_mut(name)
        .unwrap_or_else(|| panic!("expected `{name}` subcommand"));
    let mut output = Vec::new();
    subcommand
        .write_long_help(&mut output)
        .expect("subcommand help should render");
    String::from_utf8(output).expect("help output should be UTF-8")
}

#[test]
fn doctor_help_includes_about_and_examples() {
    let help = render_verify_subcommand_long_help("doctor");
    assert!(help.contains("Check toolchain, dependencies, and configuration"));
    assert!(help.contains("aiken verify doctor --json"));
}

#[test]
fn clean_help_includes_about_and_examples() {
    let help = render_verify_subcommand_long_help("clean");
    assert!(help.contains("Remove generated verification artifacts and logs"));
    assert!(help.contains("aiken verify clean --out-dir build/verify-ci"));
    assert!(help.contains("Use `-h` instead of `--help`"));
}

#[test]
fn capabilities_help_includes_about_and_examples() {
    let help = render_verify_subcommand_long_help("capabilities");
    assert!(help.contains("Show supported verification capabilities"));
    assert!(help.contains("aiken verify capabilities --json"));
    assert!(help.contains("Use `-h` instead of `--help`"));
}

#[test]
fn skips_require_failure_only_when_skips_are_disallowed() {
    assert!(skips_require_failure(1, false));
    assert!(!skips_require_failure(0, false));
    assert!(!skips_require_failure(1, true));
}

#[test]
fn proofs_succeeded_respects_command_success_false() {
    let mut summary = fixture_summary(1, 0, 0, 0, 0, 0);
    summary.command_success = false;
    assert!(!proofs_succeeded(&summary, false, false));
}

#[test]
fn silent_cli_error_message_is_only_emitted_in_silent_mode() {
    let report = ExitFailure::with_message("unknown module");
    assert_eq!(
        silent_cli_error_message(OutputMode::Silent, &report),
        Some("unknown module".to_string())
    );
    assert_eq!(silent_cli_error_message(OutputMode::Text, &report), None);
    assert_eq!(silent_cli_error_message(OutputMode::Json, &report), None);
}

#[test]
fn warning_only_deny_failure_surfaces_in_silent_mode() {
    let message = silent_cli_error_message(OutputMode::Silent, &denied_warning_report())
        .expect("silent mode should surface the denied warning message");
    assert!(message.contains("Warnings were denied by --deny"));
    assert!(message.contains("validator"));
}

fn fixture_summary(
    proved: usize,
    partial: usize,
    witness: usize,
    failed: usize,
    timed_out: usize,
    unknown: usize,
) -> verify::VerifySummary {
    let total = proved + partial + witness + failed + timed_out + unknown;
    verify::VerifySummary::new(
        total,
        proved,
        partial,
        witness,
        failed,
        timed_out,
        unknown,
        Vec::new(),
        Vec::new(),
        verify::VerifyResult::new(
            failed == 0 && timed_out == 0 && unknown == 0,
            verify::CapturedOutput::small(""),
            verify::CapturedOutput::small(""),
            Some(0),
            None,
        ),
        verify::VerificationArtifacts::default(),
        None,
        None,
        None,
        true,
        verify::DEFAULT_BLASTER_REV.to_string(),
        verify::DEFAULT_PLUTUS_CORE_REV.to_string(),
        false,
        false,
    )
}

#[test]
fn strict_cert_empty_failure_reason_is_made_explicit() {
    let theorem = verify::TheoremResult::new_with_domain(
        "example.test".to_string(),
        "Example.test".to_string(),
        verify::ProofStatus::Failed {
            category: verify::FailureCategory::Unknown,
            reason: String::new(),
        },
        0,
        verify::TrustProfile::Production,
        trusted_domain(),
        Some(trusted_input_type()),
        Some(verify::ManifestTestMode::Normal),
        Some(aiken_project::export::TestReturnMode::Bool),
    );
    let mut summary = fixture_summary(0, 0, 0, 1, 0, 0);
    summary.theorems.push(theorem);

    ensure_strict_cert_failure_reasons(&mut summary);

    let verify::ProofStatus::Failed { reason, .. } = &summary.theorems[0].proof_status else {
        panic!("expected failed status");
    };
    assert!(reason.contains("strict-cert requires LeanCertified results"));
    assert_eq!(
        summary.theorems[0].explanation.as_deref(),
        Some(reason.as_str())
    );
}

#[test]
fn proofs_succeeded_partial_no_accept_returns_false() {
    let summary = fixture_summary(1, 1, 0, 0, 0, 0);
    assert!(!proofs_succeeded(&summary, false, false));
}

#[test]
fn proofs_succeeded_partial_with_accept_returns_true() {
    let summary = fixture_summary(1, 1, 0, 0, 0, 0);
    assert!(proofs_succeeded(&summary, true, false));
}

#[test]
fn proofs_succeeded_witness_no_accept_returns_false() {
    let summary = fixture_summary(0, 0, 1, 0, 0, 0);
    assert!(!proofs_succeeded(&summary, false, false));
}

#[test]
fn proofs_succeeded_witness_with_accept_returns_true() {
    let summary = fixture_summary(0, 0, 1, 0, 0, 0);
    assert!(proofs_succeeded(&summary, false, true));
}

#[test]
fn proofs_succeeded_failed_overrides_accept_flags() {
    let summary = fixture_summary(0, 1, 1, 1, 0, 0);
    assert!(!proofs_succeeded(&summary, true, true));
}

#[test]
fn proofs_succeeded_proved_only_returns_true() {
    let summary = fixture_summary(5, 0, 0, 0, 0, 0);
    assert!(proofs_succeeded(&summary, false, false));
}

#[test]
fn proofs_succeeded_timed_out_overrides_accept_flags() {
    let summary = fixture_summary(0, 0, 0, 0, 1, 0);
    assert!(!proofs_succeeded(&summary, true, true));
}

#[test]
fn proofs_succeeded_unknown_overrides_accept_flags() {
    let summary = fixture_summary(0, 0, 0, 0, 0, 1);
    assert!(!proofs_succeeded(&summary, true, true));
}

#[test]
fn proofs_succeeded_partial_does_not_silence_witness() {
    // --accept-partial alone must NOT pass when there are witness-only
    // proofs; the user must opt in to each axis separately.
    let summary = fixture_summary(0, 1, 1, 0, 0, 0);
    assert!(!proofs_succeeded(&summary, true, false));
    assert!(!proofs_succeeded(&summary, false, true));
    assert!(proofs_succeeded(&summary, true, true));
}

#[test]
fn command_success_override_reflects_partial_gate() {
    // Mirror the exec_run_with_project gate: a summary with partial > 0
    // and no --accept-partial must flip command_success to false.
    let mut summary = fixture_summary(1, 1, 0, 0, 0, 0);
    let accept_partial = false;
    let accept_witness = false;
    if summary.command_success
        && ((summary.partial > 0 && !accept_partial) || (summary.witness > 0 && !accept_witness))
    {
        summary.command_success = false;
    }
    assert!(!summary.command_success);

    let json = serde_json::to_value(&summary).expect("summary should serialize");
    assert_eq!(json["command_success"], serde_json::Value::Bool(false));
}

#[test]
fn command_success_override_passes_through_when_accept_flags_set() {
    let mut summary = fixture_summary(1, 1, 1, 0, 0, 0);
    let accept_partial = true;
    let accept_witness = true;
    if summary.command_success
        && ((summary.partial > 0 && !accept_partial) || (summary.witness > 0 && !accept_witness))
    {
        summary.command_success = false;
    }
    assert!(summary.command_success);
}

#[test]
fn classify_partial_code_returns_s0004_for_phase2_halt_note() {
    assert_eq!(
        classify_partial_code("two-phase halt Phase 2 sorry-closed"),
        "S0004"
    );
    assert_eq!(classify_partial_code(""), "S0004");
}

#[test]
fn run_args_accept_partial_and_accept_witness_flags_parse() {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    let parsed = VerifyCli::try_parse_from([
        "aiken-verify",
        "run",
        "--accept-partial",
        "--accept-witness",
        "--generate-only",
    ])
    .expect("`verify run --accept-partial --accept-witness` should parse");

    let Cmd::Run(args) = parsed.cmd else {
        panic!("expected `run` subcommand")
    };
    assert!(args.accept_partial);
    assert!(args.accept_witness);

    // Defaults: both flags should default to false when omitted.
    let parsed_defaults = VerifyCli::try_parse_from(["aiken-verify", "run", "--generate-only"])
        .expect("`verify run` should parse without accept flags");
    let Cmd::Run(default_args) = parsed_defaults.cmd else {
        panic!("expected `run` subcommand")
    };
    assert!(!default_args.accept_partial);
    assert!(!default_args.accept_witness);
}

#[test]
fn run_args_solver_and_trust_profiles_parse() {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    let parsed = VerifyCli::try_parse_from([
        "aiken-verify",
        "run",
        "--solver-profile",
        "strict-cert",
        "--trust-profile",
        "experimental",
        "--generate-only",
    ])
    .expect("`verify run --solver-profile strict-cert --trust-profile experimental` should parse");

    let Cmd::Run(args) = parsed.cmd else {
        panic!("expected `run` subcommand")
    };
    assert_eq!(args.solver_profile, verify::SolverProfile::StrictCert);
    assert_eq!(args.trust_profile, verify::TrustProfile::Experimental);
}

#[test]
fn run_args_allow_vacuous_subgenerators_hidden_flag_parses() {
    // This debug flag is hidden from help output, but explicit argv users must
    // still be able to pass it.
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    let parsed = VerifyCli::try_parse_from([
        "aiken-verify",
        "run",
        "--allow-vacuous-subgenerators",
        "--generate-only",
    ])
    .expect("`verify run --allow-vacuous-subgenerators` should parse even though hidden");
    let Cmd::Run(args) = parsed.cmd else {
        panic!("expected `run` subcommand")
    };
    assert!(
        args.allow_vacuous_subgenerators,
        "hidden --allow-vacuous-subgenerators flag must round-trip to RunArgs"
    );

    // Default (omitted): production semantics — must be false.
    let parsed_defaults = VerifyCli::try_parse_from(["aiken-verify", "run", "--generate-only"])
        .expect("`verify run` without the hidden flag must parse");
    let Cmd::Run(default_args) = parsed_defaults.cmd else {
        panic!("expected `run` subcommand")
    };
    assert!(
        !default_args.allow_vacuous_subgenerators,
        "without the flag, allow_vacuous_subgenerators MUST default to false"
    );
}

#[test]
fn artifact_retention_on_failure_keeps_artifacts_when_command_fails() {
    assert!(
        !should_cleanup_artifacts(ArtifactRetention::OnFailure, false),
        "on-failure policy should retain artifacts whenever the final command exits non-zero",
    );
}

#[test]
fn artifact_retention_on_failure_cleans_after_full_success() {
    assert!(
        should_cleanup_artifacts(ArtifactRetention::OnFailure, true),
        "on-failure policy should clean artifacts after a fully successful run",
    );
}

#[test]
fn artifact_retention_on_success_requires_full_success() {
    assert!(
        !should_cleanup_artifacts(ArtifactRetention::OnSuccess, true),
        "on-success policy should retain artifacts only when run fully succeeds",
    );
    assert!(
        should_cleanup_artifacts(ArtifactRetention::OnSuccess, false),
        "on-success policy should clean artifacts when run exits non-zero",
    );
}

#[test]
fn cek_budget_zero_is_rejected() {
    let args = RunArgs {
        directory: None,
        deny: false,
        silent: false,
        match_tests: None,
        exact_match: false,
        env: None,
        generate_only: true,
        out_dir: PathBuf::from("build/verify"),
        keep_artifacts: false,
        artifacts: ArtifactRetention::OnFailure,
        timeout: 300,
        cek_budget: 0,
        jobs: 0,
        json: false,
        skip_unsupported: None,
        strict_unsupported: false,
        allow_skips: false,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        solver_profile: verify::SolverProfile::default(),
        trust_profile: verify::TrustProfile::default(),
        target: VerificationTargetKind::PropertyWrapper,
        plutus_core_dir: None,
        raw_output_bytes: 65536,
        accept_partial: false,
        accept_witness: false,
        allow_vacuous_subgenerators: false,
    };
    let err = exec_run(args).expect_err("cek_budget 0 should be rejected");
    assert!(
        err.to_string().contains("must be greater than 0"),
        "error should mention the constraint: {err}"
    );
}

#[test]
fn raw_output_bytes_validates_max_limit() {
    // `--raw-output-bytes 17000000` exceeds the 16 MiB cap and must be
    // rejected before any compile/lake work runs. Validation happens at
    // the top of `exec_run` so we can assert it without a fixture
    // project.
    let args = RunArgs {
        directory: None,
        deny: false,
        silent: false,
        match_tests: None,
        exact_match: false,
        env: None,
        generate_only: true,
        out_dir: PathBuf::from("build/verify"),
        keep_artifacts: false,
        artifacts: ArtifactRetention::OnFailure,
        timeout: 300,
        cek_budget: 200_000,
        jobs: 0,
        json: false,
        skip_unsupported: None,
        strict_unsupported: false,
        allow_skips: false,
        blaster_rev: DEFAULT_BLASTER_REV.to_string(),
        plutus_core_rev: DEFAULT_PLUTUS_CORE_REV.to_string(),
        existential_mode: ExistentialMode::default(),
        solver_profile: verify::SolverProfile::default(),
        trust_profile: verify::TrustProfile::default(),
        target: VerificationTargetKind::PropertyWrapper,
        plutus_core_dir: None,
        raw_output_bytes: 17_000_000,
        accept_partial: false,
        accept_witness: false,
        allow_vacuous_subgenerators: false,
    };
    let err = exec_run(args).expect_err("--raw-output-bytes above 16 MiB should be rejected");
    let msg = err.to_string();
    assert!(
        msg.contains("--raw-output-bytes must be <="),
        "error should mention the validation message: {msg}"
    );
    assert!(
        msg.contains(&MAX_RAW_OUTPUT_TAIL_BYTES.to_string()),
        "error should mention the cap value ({}): {msg}",
        MAX_RAW_OUTPUT_TAIL_BYTES
    );
    assert!(
        msg.contains("17000000"),
        "error should report the offending value: {msg}"
    );
}

/// Clap-level regression test for the comma-delimited `--skip-unsupported`
/// code filter. Behavioural filtering is covered in `aiken-project`; this
/// test only pins the command-line shape.
#[test]
fn skip_unsupported_with_codes_parses_codes_filter_correctly() {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    let parsed = VerifyCli::try_parse_from([
        "aiken-verify",
        "run",
        "--skip-unsupported=E0011,E0013",
        "--generate-only",
    ])
    .expect("`verify run --skip-unsupported=E0011,E0013` should parse");

    let Cmd::Run(args) = parsed.cmd else {
        panic!("expected `run` subcommand")
    };
    assert_eq!(
        args.skip_unsupported,
        Some(vec!["E0011".to_string(), "E0013".to_string()]),
        "clap should capture the codes filter as Option<Vec<String>>"
    );
    assert!(
        !args.strict_unsupported,
        "--strict-unsupported should default to false when --skip-unsupported is set"
    );

    // The CLI plumbing converts the raw clap shape into a `SkipPolicy` via
    // `SkipPolicy::from_cli`; assert the resolved policy is `Codes(set)`.
    let normalized = normalize_skip_unsupported(args.skip_unsupported);
    let policy = SkipPolicy::from_cli(normalized);
    let expected: std::collections::BTreeSet<String> = ["E0011".to_string(), "E0013".to_string()]
        .into_iter()
        .collect();
    assert_eq!(policy, SkipPolicy::Codes(expected));

    // Construct a representative report via the same `unsupported(...)`

    // Construct a representative report via the same `unsupported(...)`
    // constructor that the verify pipeline uses, downcasting to confirm
    // the policy is applied at the catalogue-code level.
    // E0011: in the codes list → skippable.
    // E0015: NOT in the codes list → hard error.
    // S0002: UnsoundFallback → never skippable regardless.
    //
    // We can't call the project crate's `is_skippable_generation_error`
    // directly (it's `pub(crate)` to its own module). Instead, exercise
    // the visible contract via `generate_lean_workspace` in the project
    // crate's own tests; here we pin the clap surface shape that the CLI
    // exposes.
    //
    // The behavioural pin is in `verify/tests.rs`
    // (`skip_policy_codes_filters_to_listed_codes_only`); this test owns
    // the clap → SkipPolicy conversion contract.
}

#[test]
fn bare_skip_unsupported_preserves_positional_directory_and_resolves_to_all() {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    let cwd = std::env::current_dir().expect("test process must be able to determine current dir");
    let project_root = cwd.join("example-project");
    let project_root_arg = project_root.to_string_lossy().into_owned();

    let parsed = VerifyCli::try_parse_from([
        "aiken-verify",
        "run",
        "--skip-unsupported",
        project_root_arg.as_str(),
        "--generate-only",
    ])
    .expect("bare --skip-unsupported should not consume the positional directory");

    let Cmd::Run(args) = parsed.cmd else {
        panic!("expected `run` subcommand")
    };

    assert_eq!(args.directory, Some(project_root));
    let normalized = normalize_skip_unsupported(args.skip_unsupported);
    assert_eq!(normalized, Some(Vec::new()));
    assert_eq!(SkipPolicy::from_cli(normalized), SkipPolicy::All);
}

#[test]
fn skip_unsupported_invalid_code_is_rejected_during_setup_validation() {
    let codes = ["E9999".to_string(), "E0001".to_string()];
    let err = validate_skip_unsupported_codes(Some(&codes))
        .expect_err("unknown or dormant catalogue codes should be rejected before project setup");
    let msg = err.to_string();
    assert!(msg.contains("Invalid --skip-unsupported code(s): E9999, E0001"));
    assert!(msg.contains("aiken verify capabilities"));
}

#[test]
fn skip_unsupported_rejects_non_skippable_codes_during_setup_validation() {
    let codes = ["S0001".to_string()];
    let err = validate_skip_unsupported_codes(Some(&codes))
        .expect_err("non-skippable catalogue codes must be rejected before project setup");
    let msg = err.to_string();
    assert!(msg.contains("Invalid --skip-unsupported code(s): S0001"));
    assert!(msg.contains("Only skippable catalogue codes are accepted here"));
}

/// `--strict-unsupported` is an explicit alias for the default strict policy
/// and must conflict with `--skip-unsupported`.
#[test]
fn strict_unsupported_alias_matches_default() {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    // `--strict-unsupported` parses cleanly and resolves to `None`
    // (= `SkipPolicy::None` via `SkipPolicy::from_cli`).
    let parsed = VerifyCli::try_parse_from([
        "aiken-verify",
        "run",
        "--strict-unsupported",
        "--generate-only",
    ])
    .expect("`verify run --strict-unsupported` should parse");

    let Cmd::Run(args) = parsed.cmd else {
        panic!("expected `run` subcommand")
    };
    assert!(
        args.strict_unsupported,
        "--strict-unsupported should set the bool flag"
    );
    assert!(
        args.skip_unsupported.is_none(),
        "--strict-unsupported must not populate --skip-unsupported"
    );
    assert_eq!(
        SkipPolicy::from_cli(args.skip_unsupported),
        SkipPolicy::None,
        "the alias must resolve to SkipPolicy::None (no skipping)"
    );

    // Default (no flag) ALSO resolves to SkipPolicy::None — `--strict`
    // is purely an explicit way to say "no skipping" in CI argv.
    let defaults = VerifyCli::try_parse_from(["aiken-verify", "run", "--generate-only"])
        .expect("`verify run` (no skip flags) should parse");
    let Cmd::Run(default_args) = defaults.cmd else {
        panic!("expected `run` subcommand")
    };
    assert!(default_args.skip_unsupported.is_none());
    assert!(!default_args.strict_unsupported);
    assert_eq!(
        SkipPolicy::from_cli(default_args.skip_unsupported),
        SkipPolicy::None,
    );

    // `--strict-unsupported --skip-unsupported` MUST be rejected by
    // clap's `conflicts_with` constraint.
    let conflict = VerifyCli::try_parse_from([
        "aiken-verify",
        "run",
        "--strict-unsupported",
        "--skip-unsupported",
        "--generate-only",
    ]);
    let err = match conflict {
        Ok(_) => {
            panic!("--strict-unsupported and --skip-unsupported should conflict at the clap layer")
        }
        Err(e) => e,
    };
    let err_msg = err.to_string();
    assert!(
        err_msg.contains("cannot be used with")
            || err_msg.contains("conflicts")
            || err_msg.contains("'--skip-unsupported'"),
        "conflict error should mention the conflicting flag, got: {err_msg}"
    );
}

/// Pin the existing `--allow-skips` clap contract: it requires
/// `--skip-unsupported`, so passing it without should fail. Now that
/// `--skip-unsupported` is `Option<Vec<String>>`, "presence" means
/// `Some(_)` (including the empty list), which still satisfies clap's
/// `requires` constraint.
#[test]
fn allow_skips_requires_skip_unsupported_after_signature_change() {
    #[derive(Parser)]
    struct VerifyCli {
        #[command(subcommand)]
        cmd: Cmd,
    }

    // `--allow-skips` alone is rejected by clap.
    let bad =
        VerifyCli::try_parse_from(["aiken-verify", "run", "--allow-skips", "--generate-only"]);
    assert!(
        bad.is_err(),
        "--allow-skips without --skip-unsupported should fail (requires constraint)"
    );

    // `--skip-unsupported --allow-skips` is accepted (presence of the
    // Option<Vec<_>> with Some(empty) is sufficient after normalization).
    let ok = VerifyCli::try_parse_from([
        "aiken-verify",
        "run",
        "--skip-unsupported",
        "--allow-skips",
        "--generate-only",
    ])
    .expect("`--skip-unsupported --allow-skips` should parse");
    let Cmd::Run(args) = ok.cmd else {
        panic!("expected `run` subcommand")
    };
    assert!(args.allow_skips);
    let normalized = normalize_skip_unsupported(args.skip_unsupported);
    assert_eq!(normalized, Some(Vec::new()));
    assert_eq!(SkipPolicy::from_cli(normalized), SkipPolicy::All);

    // `--skip-unsupported=E0011 --allow-skips` is also accepted.
    let codes_with_allow = VerifyCli::try_parse_from([
        "aiken-verify",
        "run",
        "--skip-unsupported=E0011",
        "--allow-skips",
        "--generate-only",
    ])
    .expect("`--skip-unsupported=E0011 --allow-skips` should parse");
    let Cmd::Run(args) = codes_with_allow.cmd else {
        panic!("expected `run` subcommand")
    };
    assert!(args.allow_skips);
    assert_eq!(args.skip_unsupported, Some(vec!["E0011".to_string()]));
}

/// `aiken verify capabilities` must render the active skip catalogue in both
/// text and JSON forms, including skippable E-codes and non-skippable S-codes.
#[test]
fn capabilities_output_includes_catalogue_table_in_text_and_json() {
    let caps = verify::capabilities();

    // JSON mode: the new `supported` and `unsupported` fields must round-
    // trip through `format_capabilities_output --json` per the plan's
    // user-facing contract.
    let json_output = format_capabilities_output(&caps, true).expect("json render");
    let json: serde_json::Value =
        serde_json::from_str(json_output.trim()).expect("output should be valid JSON");
    assert_eq!(
        json.get("supported")
            .and_then(|v| v.as_array())
            .map(|a| a.iter().filter_map(|v| v.as_str()).collect::<Vec<_>>()),
        Some(vec!["property"]),
        "JSON should include `supported: [\"property\"]`"
    );
    let unsupported_arr = json
        .get("unsupported")
        .and_then(|v| v.as_array())
        .expect("JSON should include `unsupported` array");
    assert!(
        unsupported_arr.iter().any(|v| {
            v.get("code").and_then(|c| c.as_str()) == Some("E0011")
                && v.get("skippable").and_then(|s| s.as_bool()) == Some(true)
        }),
        "JSON `unsupported` array must contain an active E-code with skippable=true"
    );
    assert!(
        unsupported_arr
            .iter()
            .all(|v| v.get("code").and_then(|c| c.as_str()) != Some("E0001")),
        "JSON `unsupported` array must not advertise dormant codes such as E0001"
    );
    assert!(
        unsupported_arr.iter().any(|v| {
            v.get("code")
                .and_then(|c| c.as_str())
                .map(|s| s.starts_with('S'))
                .unwrap_or(false)
                && v.get("skippable").and_then(|s| s.as_bool()) == Some(false)
        }),
        "JSON `unsupported` array must contain at least one S-code with skippable=false"
    );

    // Text mode: the table header, separator, and at least one active
    // E-code and one S-code row should be present.
    let text_output = format_capabilities_output(&caps, false).expect("text render");
    assert!(
        text_output.contains("Error catalogue:"),
        "text output should include the catalogue section header"
    );
    assert!(
        text_output.contains("CODE")
            && text_output.contains("FEATURE")
            && text_output.contains("SKIPPABLE"),
        "text output should include the catalogue table headers"
    );
    assert!(
        text_output.contains("E0011"),
        "text output should list active E-codes such as E0011"
    );
    assert!(
        !text_output.contains("E0001"),
        "text output must not list dormant codes such as E0001"
    );
    assert!(
        text_output.contains("S0001"),
        "text output should list S-codes such as S0001"
    );
    // E-codes in skippable categories should render as `yes`; S-codes
    // (UnsoundFallback) should render as `no`.
    let lines_with_e0011: Vec<_> = text_output
        .lines()
        .filter(|l| l.contains("E0011"))
        .collect();
    assert!(
        lines_with_e0011.iter().any(|l| l.contains("yes")),
        "E0011 row should report skippable=yes; got: {lines_with_e0011:?}"
    );
    let lines_with_s0001: Vec<_> = text_output
        .lines()
        .filter(|l| l.contains("S0001"))
        .collect();
    assert!(
        lines_with_s0001.iter().any(|l| l.contains("no")),
        "S0001 row should report skippable=no; got: {lines_with_s0001:?}"
    );
}

/// Snapshot the canonical text table emitted by `aiken verify capabilities`.
/// Per-line `contains()` assertions cover the important rows above; this
/// snapshot keeps column widths, section order, and table layout reviewable.
#[test]
fn capabilities_text_output_canonical_shape() {
    let caps = verify::capabilities();
    let text_output = format_capabilities_output(&caps, false).expect("text render");
    insta::assert_snapshot!("capabilities_text_output_canonical_shape", text_output);
}
