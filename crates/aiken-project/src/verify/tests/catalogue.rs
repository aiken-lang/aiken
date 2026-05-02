//! Regression tests for the static error-code catalogue.
//!
//! These tests pin the invariants that downstream commits depend on:
//!   - Every variant of `UnsupportedReason` has a matching catalogue entry.
//!   - Catalogue codes are unique.
//!   - `UnsoundFallback`-categorised codes are never silenced by
//!     `--skip-unsupported`.
//!   - Every `S`-prefixed code has a non-empty summary (placeholder for the
//!     richer remediation field that S-codes will gain in a later commit).

use super::super::error_catalogue::{CATALOGUE, unsupported};
use super::super::{GenerationError, GenerationErrorCategory, SkipPolicy, UnsupportedReason};

/// Manual exhaustive map from every `UnsupportedReason` variant to the
/// stable code it lives under in the catalogue. Adding a new variant to
/// `UnsupportedReason` without updating this table fails to compile (the
/// match arm is missing) — that is intentional; the table is the
/// drift sentinel between the typed payload and the wire-stable code set.
fn code_for_reason(reason: &UnsupportedReason) -> &'static str {
    match reason {
        UnsupportedReason::UnitTest { .. } => "E0001",
        UnsupportedReason::BenchmarkTest { .. } => "E0002",
        UnsupportedReason::TestArityGtOne { .. } => "E0003",
        UnsupportedReason::PlutusV1V2Validator { .. } => "E0004",
        UnsupportedReason::RecursiveAdtNoMutual { .. } => "E0010",
        UnsupportedReason::UnboundedBytearray { .. } => "E0011",
        UnsupportedReason::StringFuzzer { .. } => "E0012",
        UnsupportedReason::ListOfBool { .. } => "E0013",
        UnsupportedReason::ListOfBytearray { .. } => "E0014",
        UnsupportedReason::QualifiedAdtNoSchema { .. } => "E0015",
        UnsupportedReason::OpaqueTopLevelFuzzer { .. } => "E0016",
        UnsupportedReason::OpaqueListElementSemantics { .. } => "E0017",
        UnsupportedReason::OpaqueSubgeneratorStub { .. } => "E0018",
        UnsupportedReason::TransactionShapedTest { .. } => "E0019",
        UnsupportedReason::NestedStateMachine { .. } => "E0020",
        UnsupportedReason::VacuousTransitionPredicate { .. } => "E0021",
        UnsupportedReason::StepIrNoConstraints { .. } => "E0022",
        UnsupportedReason::ExistentialStateMachineTrace { .. } => "E0023",
        UnsupportedReason::StateMachineNoWitnessSynthesizable { .. } => "E0024",
        UnsupportedReason::MissingFuzzerSchema { .. } => "E0025",
        UnsupportedReason::NonDataFuzzerSchema { .. } => "E0026",
        UnsupportedReason::EmptyConstructorDomain { .. } => "E0027",
        UnsupportedReason::UnsupportedFuzzerOutputType { .. } => "E0028",
        UnsupportedReason::OverlappingTransitionIndexes { .. } => "E0029",
        UnsupportedReason::TupleElementNoLeanMapping { .. } => "E0030",
        UnsupportedReason::ListElementNoLeanMapping { .. } => "E0031",
        UnsupportedReason::NonFirstOrderSuchThatHelper { .. } => "E0032",
        UnsupportedReason::WhenPatternConstructorVarDropped { .. } => "E0033",
        UnsupportedReason::FiniteDomainTooLarge { .. } => "E0034",
        UnsupportedReason::FiniteDomainTargetModeUnsupported { .. } => "E0035",
        UnsupportedReason::SemanticsOutputTypeMismatch { .. } => "E0044",
        UnsupportedReason::ValidatorTargetMissing { .. } => "E0050",
        UnsupportedReason::StepFnSoundAxiomEmitted { .. } => "S0001",
        UnsupportedReason::ConstructorTagUnresolved { .. } => "S0002",
        UnsupportedReason::ExistentialWitnessUnsoundForDomain { .. } => "S0003",
    }
}

/// Concrete-value witnesses for every `UnsupportedReason` variant. Used by
/// `every_unsupported_reason_has_catalogue_entry` and
/// `unsound_fallback_codes_are_never_skippable` to exercise each variant
/// at least once. Order matches the declaration order of the enum so that
/// adding a new variant forces this table to grow as well.
fn all_reasons() -> Vec<UnsupportedReason> {
    let test_name = "module.test".to_string();
    vec![
        UnsupportedReason::UnitTest {
            test_name: test_name.clone(),
        },
        UnsupportedReason::BenchmarkTest {
            test_name: test_name.clone(),
        },
        UnsupportedReason::TestArityGtOne {
            test_name: test_name.clone(),
            arity: 2,
        },
        UnsupportedReason::PlutusV1V2Validator {
            test_name: test_name.clone(),
            version: 2,
        },
        UnsupportedReason::RecursiveAdtNoMutual {
            test_name: test_name.clone(),
            type_name: "Tree".to_string(),
        },
        UnsupportedReason::UnboundedBytearray {
            test_name: test_name.clone(),
        },
        UnsupportedReason::StringFuzzer {
            test_name: test_name.clone(),
        },
        UnsupportedReason::ListOfBool {
            test_name: test_name.clone(),
        },
        UnsupportedReason::ListOfBytearray {
            test_name: test_name.clone(),
        },
        UnsupportedReason::QualifiedAdtNoSchema {
            test_name: test_name.clone(),
            type_name: "Foo.Bar".to_string(),
        },
        UnsupportedReason::OpaqueTopLevelFuzzer {
            test_name: test_name.clone(),
            reason: "opaque body".to_string(),
        },
        UnsupportedReason::OpaqueListElementSemantics {
            test_name: test_name.clone(),
            reason: "opaque element".to_string(),
        },
        UnsupportedReason::OpaqueSubgeneratorStub {
            test_name: test_name.clone(),
            sub_generator: "module::subgen".to_string(),
        },
        UnsupportedReason::TransactionShapedTest {
            test_name: test_name.clone(),
        },
        UnsupportedReason::NestedStateMachine {
            test_name: test_name.clone(),
        },
        UnsupportedReason::VacuousTransitionPredicate {
            test_name: test_name.clone(),
        },
        UnsupportedReason::StepIrNoConstraints {
            test_name: test_name.clone(),
        },
        UnsupportedReason::ExistentialStateMachineTrace {
            test_name: test_name.clone(),
        },
        UnsupportedReason::StateMachineNoWitnessSynthesizable {
            test_name: test_name.clone(),
        },
        UnsupportedReason::MissingFuzzerSchema {
            test_name: test_name.clone(),
        },
        UnsupportedReason::NonDataFuzzerSchema {
            test_name: test_name.clone(),
        },
        UnsupportedReason::EmptyConstructorDomain {
            test_name: test_name.clone(),
        },
        UnsupportedReason::UnsupportedFuzzerOutputType {
            test_name: test_name.clone(),
            type_repr: "Foo".to_string(),
        },
        UnsupportedReason::OverlappingTransitionIndexes {
            test_name: test_name.clone(),
        },
        UnsupportedReason::TupleElementNoLeanMapping {
            test_name: test_name.clone(),
            element_type: "Foo".to_string(),
        },
        UnsupportedReason::ListElementNoLeanMapping {
            test_name: test_name.clone(),
            element_type: "Foo".to_string(),
        },
        UnsupportedReason::NonFirstOrderSuchThatHelper {
            test_name: test_name.clone(),
            helper: "helper_fn".to_string(),
        },
        UnsupportedReason::WhenPatternConstructorVarDropped {
            test_name: test_name.clone(),
            pattern: "Some(x)".to_string(),
            binders: vec!["x".to_string()],
        },
        UnsupportedReason::FiniteDomainTooLarge {
            test_name: test_name.clone(),
            cases: "65".to_string(),
            cap: 64,
            cap_constant: "MAX_FINITE_THEOREM_INSTANCES_PER_TEST",
        },
        UnsupportedReason::FiniteDomainTargetModeUnsupported {
            test_name: test_name.clone(),
            target: "validator".to_string(),
        },
        UnsupportedReason::SemanticsOutputTypeMismatch {
            test_name: test_name.clone(),
        },
        UnsupportedReason::ValidatorTargetMissing {
            test_name: test_name.clone(),
        },
        UnsupportedReason::StepFnSoundAxiomEmitted {
            test_name: test_name.clone(),
            shape: "Opaque".to_string(),
        },
        UnsupportedReason::ConstructorTagUnresolved {
            test_name: test_name.clone(),
            ctor: "Variant".to_string(),
            type_name: "Type".to_string(),
        },
        UnsupportedReason::ExistentialWitnessUnsoundForDomain {
            test_name,
            domain: "Int".to_string(),
        },
    ]
}

#[test]
fn every_unsupported_reason_has_catalogue_entry() {
    for reason in all_reasons() {
        let code = code_for_reason(&reason);
        let entry = CATALOGUE.iter().find(|e| e.code == code);
        assert!(
            entry.is_some(),
            "UnsupportedReason variant maps to code `{code}` but no \
             matching CatalogueEntry exists in CATALOGUE. Add it before \
             shipping; the catalogue is the single source of truth.",
        );
    }
}

#[test]
fn every_catalogue_code_is_unique() {
    use std::collections::HashSet;
    let mut seen: HashSet<&'static str> = HashSet::new();
    for entry in CATALOGUE {
        assert!(
            seen.insert(entry.code),
            "duplicate catalogue code `{}` — every code must be unique",
            entry.code
        );
    }
}

#[test]
fn unsound_fallback_codes_are_never_skippable() {
    // Build a quick lookup from code to a representative reason so we can
    // construct a fully-formed GenerationError for every UnsoundFallback
    // entry in the catalogue.
    let reasons = all_reasons();
    for entry in CATALOGUE {
        if !matches!(entry.category, GenerationErrorCategory::UnsoundFallback) {
            continue;
        }
        let reason = reasons
            .iter()
            .find(|r| code_for_reason(r) == entry.code)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "UnsoundFallback catalogue entry `{}` has no matching \
                     UnsupportedReason variant in `all_reasons()`",
                    entry.code
                )
            });

        let err = unsupported(entry.code, reason);
        let report = miette::Report::new(err);

        // Confirm under all three policies — `None`, `All`, and a `Codes`
        // filter that explicitly lists this code — that an `UnsoundFallback`
        // entry is NEVER silenced. This is the load-bearing invariant that
        // distinguishes S-codes from skippable E-codes.
        for policy in [
            SkipPolicy::None,
            SkipPolicy::All,
            SkipPolicy::Codes(std::iter::once(entry.code.to_string()).collect()),
        ] {
            assert!(
                !super::super::is_skippable_generation_error(&report, &policy),
                "UnsoundFallback code `{}` is reported as skippable under policy {policy:?} — \
                 --skip-unsupported must NOT silence S-codes.",
                entry.code,
            );
        }

        // Sanity: the GenerationError carries the catalogue code on its
        // `code` field (downcasting via miette).
        let downcast = report
            .downcast_ref::<GenerationError>()
            .expect("downcast to GenerationError");
        assert_eq!(downcast.code, Some(entry.code));
        assert!(downcast.reason.is_some());
        assert!(matches!(
            downcast.category,
            GenerationErrorCategory::UnsoundFallback
        ));
    }
}

#[test]
fn s_codes_have_remediation_field_set() {
    // Placeholder for the richer remediation field that S-codes will gain
    // in a later commit. For now we assert that every S-prefixed entry has
    // a non-empty `summary` string — that is the user-facing one-liner.
    let mut s_count = 0;
    for entry in CATALOGUE {
        if !entry.code.starts_with('S') {
            continue;
        }
        s_count += 1;
        assert!(
            !entry.summary.is_empty(),
            "catalogue entry `{}` has empty summary; \
             every S-code must carry remediation copy",
            entry.code
        );
    }
    assert!(
        s_count > 0,
        "expected at least one S-prefixed entry in CATALOGUE; \
         did the catalogue lose its UnsoundFallback rows?"
    );
}

#[test]
fn catalogue_generation_errors_expose_miette_metadata_without_duplicate_codes() {
    let err = unsupported(
        "E0011",
        UnsupportedReason::UnboundedBytearray {
            test_name: "module.test".to_string(),
        },
    );

    let code = miette::Diagnostic::code(&err).map(|code| code.to_string());
    assert_eq!(code.as_deref(), Some("E0011"));

    let help = miette::Diagnostic::help(&err)
        .expect("catalogue-backed error should expose help")
        .to_string();
    assert!(help.contains("--skip-unsupported=E0011"));
    assert!(miette::Diagnostic::url(&err).is_none());

    assert_eq!(
        err.message,
        "Test 'module.test': Unbounded or list-nested ByteArray/String fuzzers cannot be soundly enumerated; add a `bytearray_between(min, max)` constraint or rewrite the fuzzer to a bounded shape."
    );
    assert!(
        !err.message.contains("[E0011]"),
        "miette renders Diagnostic::code(); the message itself must not embed the code"
    );
}

#[test]
fn project_error_preserves_verify_generation_diagnostic_metadata() {
    let report = miette::Report::new(unsupported(
        "S0003",
        UnsupportedReason::ExistentialWitnessUnsoundForDomain {
            test_name: "module.test".to_string(),
            domain: "Int".to_string(),
        },
    ));
    let (message, code, help, url) = crate::verify::generation_error_metadata(&report);
    let wrapped = crate::error::Error::verify_generation(message, code, help, url);

    let code = miette::Diagnostic::code(&wrapped).map(|code| code.to_string());
    assert_eq!(code.as_deref(), Some("S0003"));
    let help = miette::Diagnostic::help(&wrapped)
        .expect("wrapped verify generation error should preserve help")
        .to_string();
    assert!(help.contains("unsound"));
    assert!(miette::Diagnostic::url(&wrapped).is_none());
}

#[test]
fn every_catalogue_entry_has_metadata_populated() {
    // Pin the invariant that all metadata fields on a CatalogueEntry are
    // populated. `feature`, `doc_path`, and the matching code-prefix are
    // contracts consumed by `aiken verify capabilities` and the JSON output
    // in subsequent commits. Asserting them here today doubles as a drift
    // sentinel and as a test-time read of every field on the struct, which
    // keeps `dead_code` clean.
    for entry in CATALOGUE {
        assert!(
            !entry.feature.is_empty(),
            "catalogue entry `{}` has empty feature slug",
            entry.code,
        );
        assert!(
            !entry.summary.is_empty(),
            "catalogue entry `{}` has empty summary",
            entry.code,
        );
        assert!(
            !entry.doc_path.is_empty(),
            "catalogue entry `{}` has empty doc_path",
            entry.code,
        );
        // doc_path conventionally mirrors the code today.
        assert_eq!(
            entry.doc_path, entry.code,
            "catalogue entry `{}` has a mismatched doc_path `{}` \
             (the convention is doc_path == code)",
            entry.code, entry.doc_path,
        );
        // `future` is a bool flag; just read it so the field does not go
        // dead.
        let _ = entry.future;
    }
}

#[test]
fn iter_catalogue_projects_the_currently_surfaced_codes_with_correct_skippable_flag() {
    // The public `iter_catalogue` is what `aiken verify capabilities --json`
    // consumes. Pin the invariants:
    //   1. Only codes with a real runtime/verdict surface are exposed.
    //   2. `skippable` is true iff the category is UnsupportedShape /
    //      FallbackRequired AND the entry is not informational.
    //   3. Public docs are not advertised yet, so `doc` stays absent.
    use super::super::error_catalogue::{EXPOSED_CATALOGUE_CODES, iter_catalogue};
    let projected: Vec<_> = iter_catalogue().collect();
    let expected: Vec<_> = CATALOGUE
        .iter()
        .filter(|entry| EXPOSED_CATALOGUE_CODES.contains(&entry.code))
        .collect();
    assert_eq!(
        projected.len(),
        expected.len(),
        "iter_catalogue must project the currently surfaced codes only"
    );

    for (entry, projection) in expected.iter().zip(projected.iter()) {
        assert_eq!(projection.code, entry.code);
        assert_eq!(projection.feature, entry.feature);
        assert!(projection.doc.is_none());

        let category_is_skippable = matches!(
            entry.category,
            GenerationErrorCategory::UnsupportedShape | GenerationErrorCategory::FallbackRequired,
        );
        let expected_skippable = category_is_skippable && !entry.informational;
        assert_eq!(
            projection.skippable, expected_skippable,
            "skippable flag for `{}` should be {expected_skippable} \
             (category_is_skippable={category_is_skippable}, \
             informational={}), got {}",
            entry.code, entry.informational, projection.skippable,
        );
    }

    assert!(
        projected.iter().all(|entry| entry.code != "E0001"),
        "dormant codes like E0001 must not be advertised until the verifier can emit them"
    );
    assert!(
        projected.iter().all(|entry| entry.code != "S0004"),
        "informational-only codes like S0004 must stay out of the public catalogue until runtime reports emit them with a code field",
    );
}

#[test]
fn capabilities_json_shape_includes_supported_and_unsupported_arrays() {
    // The public JSON surface must include the high-level supported kinds and
    // the currently surfaced catalogue entries. Spot-check one skippable
    // active E-code and one non-skippable S-code.
    let caps = super::super::capabilities();

    assert_eq!(caps.supported, vec!["property".to_string()]);

    let e0011 = caps
        .unsupported
        .iter()
        .find(|c| c.code == "E0011")
        .expect("E0011 must be present in capabilities catalogue");
    assert_eq!(e0011.feature, "unbounded_bytearray");
    assert!(
        e0011.skippable,
        "E0011 (FallbackRequired) must serialise as skippable=true"
    );
    assert!(e0011.doc.is_none());

    let s0001 = caps
        .unsupported
        .iter()
        .find(|c| c.code == "S0001")
        .expect("S0001 must be present in capabilities catalogue");
    assert_eq!(s0001.feature, "step_fn_sound_axiom_emitted");
    assert!(
        !s0001.skippable,
        "S0001 (UnsoundFallback) must serialise as skippable=false"
    );
    assert!(s0001.doc.is_none());
    assert!(
        caps.unsupported.iter().all(|c| c.code != "E0001"),
        "dormant codes like E0001 must be absent from the public catalogue"
    );
    assert!(
        caps.unsupported.iter().all(|c| c.code != "S0004"),
        "informational-only codes like S0004 must be absent from the public capabilities payload",
    );

    let json = serde_json::to_value(&caps).expect("capabilities should serialise");
    let supported = json
        .get("supported")
        .and_then(|v| v.as_array())
        .expect("`supported` must be a JSON array");
    assert!(supported.iter().any(|v| v == "property"));

    let unsupported = json
        .get("unsupported")
        .and_then(|v| v.as_array())
        .expect("`unsupported` must be a JSON array");
    assert!(
        !unsupported.is_empty(),
        "`unsupported` array should not be empty",
    );

    let e0011_json = unsupported
        .iter()
        .find(|v| v.get("code").and_then(|c| c.as_str()) == Some("E0011"))
        .expect("E0011 must be present in JSON `unsupported` array");
    assert_eq!(
        e0011_json.get("feature").and_then(|v| v.as_str()),
        Some("unbounded_bytearray")
    );
    assert_eq!(
        e0011_json.get("skippable").and_then(|v| v.as_bool()),
        Some(true)
    );
    assert!(e0011_json.get("doc").is_none());

    let s0001_json = unsupported
        .iter()
        .find(|v| v.get("code").and_then(|c| c.as_str()) == Some("S0001"))
        .expect("S0001 must be present in JSON `unsupported` array");
    assert_eq!(
        s0001_json.get("skippable").and_then(|v| v.as_bool()),
        Some(false),
        "S0001 must serialise as skippable=false"
    );
    assert!(
        unsupported
            .iter()
            .all(|v| v.get("code").and_then(|c| c.as_str()) != Some("E0001")),
        "dormant codes like E0001 must be absent from the JSON `unsupported` array"
    );
}
