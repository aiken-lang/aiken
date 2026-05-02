// The catalogue and its constructor are foundational infrastructure: this
// table is the single source of truth for stable verify error codes. The
// publicly exposed `iter_catalogue` / `CatalogueCode` surface is consumed by
// `aiken verify capabilities`; `lookup` and `unsupported` remain internal
// constructors used by call sites that emit `GenerationError`s.
#![allow(dead_code)]

//! Static catalogue of stable error codes that `aiken verify` can produce
//! when a test cannot be lowered to a sound Lean theorem.
//!
//! The catalogue is the single source of truth for:
//!   - The stable wire-level identity of an error (`code`, e.g. "E0023" or
//!     "S0001"). Codes never change once shipped.
//!   - The category mapping that drives `is_skippable_generation_error`
//!     (skippable `UnsupportedShape` / `FallbackRequired` vs hard
//!     `InvalidConstraint` / `UnsoundFallback`).
//!   - The short user-facing summary surfaced by `aiken verify capabilities`
//!     and the JSON output.
//!
//! Error codes fall into four classes:
//!
//!   * **E0001–E0033, E0050** — *skippable* unsupported features. Mapped to
//!     `UnsupportedShape` (we cannot lower this test shape) or
//!     `FallbackRequired` (we would need a fallback we cannot soundly emit).
//!     These respect `--skip-unsupported`.
//!   * **E0040–E0045** — *non-skippable* user errors. Mapped to
//!     `InvalidConstraint`. Surfacing these means the test's metadata is
//!     internally inconsistent (e.g. `min > max`).
//!   * **S0001–S0003** — *non-skippable* unsound-fallback errors. Mapped to
//!     `UnsoundFallback`. These previously caused silent "PROVED" verdicts
//!     and are now surfaced as hard errors regardless of `--skip-unsupported`.
//!   * **S0004** — *internal informational* caveat embedded in a
//!     `ProofCaveat::Partial`. Not an error per se; kept in the internal
//!     catalogue for stable metadata, but not projected to the public
//!     capabilities surface until the runtime emits it as a code-bearing field.
//!
//! Adding a new code is a three-step change:
//!   1. Add a `CatalogueEntry` to `CATALOGUE` below.
//!   2. Add a matching variant to `UnsupportedReason` in `verify.rs`.
//!   3. Have `every_unsupported_reason_has_catalogue_entry` cover the variant.

use super::{GenerationError, GenerationErrorCategory, UnsupportedReason};

/// Static metadata describing one stable error code in the verify catalogue.
pub(crate) struct CatalogueEntry {
    /// Stable wire-level identifier, e.g. "E0023" or "S0001". Never changes
    /// once shipped.
    pub code: &'static str,
    /// Snake-case feature slug. Matches the JSON `code` discriminator emitted
    /// by `serde` for `UnsupportedReason`.
    pub feature: &'static str,
    /// Category that drives skip/hard-error routing.
    pub category: GenerationErrorCategory,
    /// One-line human-readable description used by `aiken verify capabilities`
    /// and as the default `GenerationError.message` when the catalogue
    /// `unsupported(...)` constructor is used.
    pub summary: &'static str,
    /// When true, the generated user-facing message includes a "future
    /// version" line indicating the feature is planned.
    pub future: bool,
    /// Reserved documentation fragment. Not surfaced until public docs exist.
    pub doc_path: &'static str,
    /// When true, the entry is *informational only* — it never emerges
    /// as a runtime [`GenerationError`]. Informational entries remain in the
    /// internal catalogue for stable metadata, but are not projected through
    /// the public `iter_catalogue` capabilities surface until the runtime has
    /// a code-bearing way to emit them.
    pub informational: bool,
}

/// Source of truth for every stable error code recognised by `aiken verify`.
/// Every variant of `UnsupportedReason` MUST have a matching entry here; this
/// invariant is enforced by `every_unsupported_reason_has_catalogue_entry`.
pub(crate) const CATALOGUE: &[CatalogueEntry] = &[
    // ---------------------------------------------------------------
    // Skippable shape gates — UnsupportedShape
    // ---------------------------------------------------------------
    CatalogueEntry {
        code: "E0001",
        feature: "unit_test",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "Unit tests are not yet supported by `aiken verify`; only property tests can be discharged as Lean theorems.",
        future: true,
        doc_path: "E0001",
        informational: false,
    },
    CatalogueEntry {
        code: "E0002",
        feature: "benchmark_test",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "Benchmark tests are not supported by `aiken verify`; only property tests can be discharged as Lean theorems.",
        future: false,
        doc_path: "E0002",
        informational: false,
    },
    CatalogueEntry {
        code: "E0003",
        feature: "test_arity_gt_one",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "Property tests with more than one fuzzer argument are not yet supported.",
        future: true,
        doc_path: "E0003",
        informational: false,
    },
    CatalogueEntry {
        code: "E0004",
        feature: "plutus_v1_v2_validator",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "Validators targeting Plutus v1 or v2 are not supported by `aiken verify`; only Plutus v3 validators can be discharged.",
        future: false,
        doc_path: "E0004",
        informational: false,
    },
    // ---------------------------------------------------------------
    // Skippable fallback-required shapes — FallbackRequired
    // ---------------------------------------------------------------
    CatalogueEntry {
        code: "E0010",
        feature: "recursive_adt_no_mutual",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "Recursive ADTs without `mutual`-block lowering cannot yet be expressed as Lean inductive types.",
        future: true,
        doc_path: "E0010",
        informational: false,
    },
    CatalogueEntry {
        code: "E0011",
        feature: "unbounded_bytearray",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "Unbounded or list-nested ByteArray/String fuzzers cannot be soundly enumerated; add a `bytearray_between(min, max)` constraint or rewrite the fuzzer to a bounded shape.",
        future: false,
        doc_path: "E0011",
        informational: false,
    },
    CatalogueEntry {
        code: "E0012",
        feature: "string_fuzzer",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "String-domain fuzzers are not supported; convert the value to a bounded ByteArray.",
        future: true,
        doc_path: "E0012",
        informational: false,
    },
    CatalogueEntry {
        code: "E0013",
        feature: "list_of_bool",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "List<Bool> fuzzers cannot be lowered to a finite Lean enumeration today.",
        future: true,
        doc_path: "E0013",
        informational: false,
    },
    CatalogueEntry {
        code: "E0014",
        feature: "list_of_bytearray",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "List<ByteArray> fuzzers cannot be lowered without bounded element schemas.",
        future: true,
        doc_path: "E0014",
        informational: false,
    },
    CatalogueEntry {
        code: "E0015",
        feature: "qualified_adt_no_schema",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "Qualified ADT fuzzer has no exported data schema; cannot derive the Lean shape predicate.",
        future: true,
        doc_path: "E0015",
        informational: false,
    },
    CatalogueEntry {
        code: "E0016",
        feature: "opaque_top_level_fuzzer",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "The top-level fuzzer is opaque (cannot be statically inlined); rewrite using first-order combinators.",
        future: true,
        doc_path: "E0016",
        informational: false,
    },
    CatalogueEntry {
        code: "E0017",
        feature: "opaque_list_element_semantics",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "List element semantics are opaque; the per-element predicate cannot be lowered to Lean.",
        future: true,
        doc_path: "E0017",
        informational: false,
    },
    CatalogueEntry {
        code: "E0018",
        feature: "opaque_subgenerator_stub",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "Fuzzer references a sub-generator whose body cannot be statically inlined. Stubbing it as `opaque ... : Prop` lets Lean's `Inhabited Prop` instance fill the body with `fun _ _ => True`, making every constraint on the sub-generator domain trivially provable. Inline the sub-generator into the main fuzzer chain, or rewrite using only first-order combinators (and_then, fork*, int_between, bytearray_between). Sub-generator predicate extraction will be supported in a future release.",
        future: true,
        doc_path: "E0018",
        informational: false,
    },
    CatalogueEntry {
        code: "E0019",
        feature: "transaction_shaped_test",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "Transaction-shaped property tests (those quantifying over `Transaction` directly) are not yet supported.",
        future: true,
        doc_path: "E0019",
        informational: false,
    },
    CatalogueEntry {
        code: "E0020",
        feature: "nested_state_machine",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "Nested state-machine tests (state machines inside other fuzzer combinators) are not supported.",
        future: true,
        doc_path: "E0020",
        informational: false,
    },
    CatalogueEntry {
        code: "E0021",
        feature: "vacuous_transition_predicate",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "State-machine transition predicate is structurally vacuous (would prove `∀ x, True`); refusing to emit a trivial theorem.",
        future: false,
        doc_path: "E0021",
        informational: false,
    },
    CatalogueEntry {
        code: "E0022",
        feature: "step_ir_no_constraints",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "Step IR carries no surviving constraints after lowering; the resulting theorem would be vacuous.",
        future: false,
        doc_path: "E0022",
        informational: false,
    },
    CatalogueEntry {
        code: "E0023",
        feature: "existential_state_machine_trace",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "Existential state-machine traces are not yet expressible as Lean theorems.",
        future: true,
        doc_path: "E0023",
        informational: false,
    },
    CatalogueEntry {
        code: "E0024",
        feature: "state_machine_no_witness_synthesizable",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "State-machine test has no synthesisable concrete witness; cannot emit a witness-only proof.",
        future: true,
        doc_path: "E0024",
        informational: false,
    },
    CatalogueEntry {
        code: "E0025",
        feature: "missing_fuzzer_schema",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "Fuzzer has no exported data schema; cannot derive the Lean precondition.",
        future: false,
        doc_path: "E0025",
        informational: false,
    },
    CatalogueEntry {
        code: "E0026",
        feature: "non_data_fuzzer_schema",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "Fuzzer schema is not a Plutus Data shape; only Data-typed fuzzers can be lowered.",
        future: true,
        doc_path: "E0026",
        informational: false,
    },
    CatalogueEntry {
        code: "E0027",
        feature: "empty_constructor_domain",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "Constructor domain is empty after lowering; the resulting theorem would quantify over no inputs.",
        future: false,
        doc_path: "E0027",
        informational: false,
    },
    CatalogueEntry {
        code: "E0028",
        feature: "unsupported_fuzzer_output_type",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "Fuzzer output type is not yet mapped to a Lean representation.",
        future: true,
        doc_path: "E0028",
        informational: false,
    },
    CatalogueEntry {
        code: "E0029",
        feature: "overlapping_transition_indexes",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "State-machine transition indexes overlap; cannot derive a deterministic `step_tag` mapping.",
        future: true,
        doc_path: "E0029",
        informational: false,
    },
    CatalogueEntry {
        code: "E0030",
        feature: "tuple_element_no_lean_mapping",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "A tuple element has no Lean mapping; cannot construct the tuple-shaped precondition.",
        future: true,
        doc_path: "E0030",
        informational: false,
    },
    CatalogueEntry {
        code: "E0031",
        feature: "list_element_no_lean_mapping",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "A list element has no Lean mapping; cannot construct the list-shaped precondition.",
        future: true,
        doc_path: "E0031",
        informational: false,
    },
    CatalogueEntry {
        code: "E0032",
        feature: "non_first_order_such_that_helper",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "`such_that` helper is not first-order; cannot inline the predicate body into Lean.",
        future: true,
        doc_path: "E0032",
        informational: false,
    },
    CatalogueEntry {
        code: "E0033",
        feature: "when_pattern_constructor_var_dropped",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "`when` clause uses a constructor pattern whose binders are dropped during lowering; emitted as a widened `Or` branch.",
        future: true,
        doc_path: "E0033",
        informational: false,
    },
    CatalogueEntry {
        code: "E0034",
        feature: "finite_domain_too_large",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "A finite fuzzer domain exceeds the verifier's generated-theorem cap.",
        future: false,
        doc_path: "E0034",
        informational: false,
    },
    CatalogueEntry {
        code: "E0035",
        feature: "finite_domain_target_mode_unsupported",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "Finite-domain theorem generation is currently supported only for property-wrapper verification targets.",
        future: false,
        doc_path: "E0035",
        informational: false,
    },
    CatalogueEntry {
        code: "E0050",
        feature: "validator_target_missing",
        category: GenerationErrorCategory::UnsupportedShape,
        summary: "Selected verification target requires a validator handler, but the test has no validator-target metadata.",
        future: false,
        doc_path: "E0050",
        informational: false,
    },
    // ---------------------------------------------------------------
    // Non-skippable user errors — InvalidConstraint
    // ---------------------------------------------------------------
    CatalogueEntry {
        code: "E0040",
        feature: "inconsistent_list_length_bounds",
        category: GenerationErrorCategory::InvalidConstraint,
        summary: "List length bounds are internally inconsistent (e.g. `min > max`); fix the fuzzer constraint.",
        future: false,
        doc_path: "E0040",
        informational: false,
    },
    CatalogueEntry {
        code: "E0041",
        feature: "inconsistent_bytestring_length_bounds",
        category: GenerationErrorCategory::InvalidConstraint,
        summary: "ByteArray length bounds are internally inconsistent (e.g. `min > max`); fix the fuzzer constraint.",
        future: false,
        doc_path: "E0041",
        informational: false,
    },
    CatalogueEntry {
        code: "E0042",
        feature: "inconsistent_int_bounds",
        category: GenerationErrorCategory::InvalidConstraint,
        summary: "Integer bounds are internally inconsistent (e.g. `min > max`); fix the fuzzer constraint.",
        future: false,
        doc_path: "E0042",
        informational: false,
    },
    CatalogueEntry {
        code: "E0043",
        feature: "invalid_int_bound_literal",
        category: GenerationErrorCategory::InvalidConstraint,
        summary: "Integer bound is not a valid literal; expected a decimal integer constant.",
        future: false,
        doc_path: "E0043",
        informational: false,
    },
    CatalogueEntry {
        code: "E0044",
        feature: "semantics_output_type_mismatch",
        category: GenerationErrorCategory::InvalidConstraint,
        summary: "Fuzzer semantics tree disagrees with the declared output type; metadata is internally inconsistent.",
        future: false,
        doc_path: "E0044",
        informational: false,
    },
    CatalogueEntry {
        code: "E0045",
        feature: "invalid_concrete_witness_hex",
        category: GenerationErrorCategory::InvalidConstraint,
        summary: "Concrete witness payload is not valid hex; expected a CBOR-hex encoded Plutus Data value.",
        future: false,
        doc_path: "E0045",
        informational: false,
    },
    // ---------------------------------------------------------------
    // Hard errors — UnsoundFallback (never skippable)
    // ---------------------------------------------------------------
    CatalogueEntry {
        code: "S0001",
        feature: "step_fn_sound_axiom_emitted",
        category: GenerationErrorCategory::UnsoundFallback,
        summary: "Step IR is not a single `Construct`; previously emitted a trusted `step_fn_sound` axiom that made every theorem trivially provable.",
        future: true,
        doc_path: "S0001",
        informational: false,
    },
    CatalogueEntry {
        code: "S0002",
        feature: "constructor_tag_unresolved",
        category: GenerationErrorCategory::UnsoundFallback,
        summary: "Could not resolve the UPLC constructor tag; previously fell back to tag 0, making non-first-constructor preconditions vacuously satisfiable.",
        future: false,
        doc_path: "S0002",
        informational: false,
    },
    CatalogueEntry {
        code: "S0003",
        feature: "existential_witness_unsound_for_domain",
        category: GenerationErrorCategory::UnsoundFallback,
        summary: "`--existential-mode witness` synthesises a trivial witness that is unsound for non-Bool domains; only Bool-domain fail-once tests are accepted.",
        future: false,
        doc_path: "S0003",
        informational: false,
    },
    // ---------------------------------------------------------------
    // Informational caveat — surfaced via ProofCaveat::Partial,
    // accepted with `--accept-partial`. Categorised as `FallbackRequired`
    // so `is_skippable_generation_error` would skip if it were ever
    // promoted to a generation error (it currently is not).
    // ---------------------------------------------------------------
    CatalogueEntry {
        code: "S0004",
        feature: "phase2_halting_sorry_unconditional",
        category: GenerationErrorCategory::FallbackRequired,
        summary: "Phase-2 halting proof is closed with `sorry`; the Lean build succeeded but the obligation is not formally discharged. Accepted via `--accept-partial`.",
        future: true,
        doc_path: "S0004",
        informational: true,
    },
];

/// Look up the catalogue entry for a given stable code. Used by callers that
/// need to consult metadata (category, summary, help text) at runtime — the
/// expected pattern is to call `unsupported(code, reason)` instead, which
/// performs the lookup internally.
pub(crate) fn lookup(code: &str) -> Option<&'static CatalogueEntry> {
    CATALOGUE.iter().find(|e| e.code == code)
}

impl CatalogueEntry {
    pub(crate) fn diagnostic_help(&self) -> String {
        match self.category {
            GenerationErrorCategory::InvalidConstraint => {
                "Fix the invalid fuzzer constraint or compiler metadata, then rerun `aiken verify`.".to_string()
            }
            GenerationErrorCategory::UnsoundFallback => {
                "This proof shape would be unsound if accepted automatically. Rewrite the test to use a supported, sound domain before rerunning `aiken verify`.".to_string()
            }
            GenerationErrorCategory::UnsupportedShape | GenerationErrorCategory::FallbackRequired => {
                let skip_hint = if self.informational {
                    "".to_string()
                } else {
                    format!(" If you only want to continue verifying other tests, pass `--skip-unsupported={}`.", self.code)
                };
                let future_hint = if self.future {
                    " This feature is reserved for a future verifier release."
                } else {
                    ""
                };
                format!(
                    "Rewrite the test or fuzzer to use currently supported verifier features.{skip_hint}{future_hint}"
                )
            }
        }
    }
}

/// Build a `GenerationError` from a stable catalogue code and its typed
/// payload. Debug-asserts that the code exists in `CATALOGUE`. The error's
/// `category` is derived from the catalogue entry, ensuring that
/// `is_skippable_generation_error` is consistent with the catalogue table.
pub(crate) fn unsupported(code: &'static str, reason: UnsupportedReason) -> GenerationError {
    let entry = lookup(code);
    debug_assert!(
        entry.is_some(),
        "code {code} not in CATALOGUE — add it before shipping",
    );

    let (category, message) = match entry {
        Some(e) => (e.category, reason.user_message(e.summary)),
        // Production fallback: should never trigger in debug builds because
        // of the assertion above, but if it does we route the error as an
        // unsupported shape rather than panicking on the user.
        None => (
            GenerationErrorCategory::UnsupportedShape,
            format!(
                "Test '{}': unknown error code (catalogue lookup failed)",
                reason.test_name()
            ),
        ),
    };

    GenerationError {
        category,
        message,
        code: Some(code),
        reason: Some(reason),
    }
}

/// Public projection of a single [`CatalogueEntry`] suitable for serialisation
/// in `aiken verify capabilities --json`.
///
/// Mirrors the shape published in the user-facing JSON contract:
///
/// ```json
/// { "code": "E0011", "feature": "unbounded_bytearray", "skippable": true }
/// ```
///
/// `skippable` is derived from the entry's category — `true` iff the category
/// is `UnsupportedShape` or `FallbackRequired` (the same predicate used by
/// `is_skippable_generation_error` for `SkipPolicy::All`). `UnsoundFallback`
/// (S0001/S0002/S0003) and `InvalidConstraint` codes always serialise as
/// `false` because they are never silenced by `--skip-unsupported`.
#[non_exhaustive]
#[derive(Debug, Clone, serde::Serialize)]
pub struct CatalogueCode {
    /// Stable wire-level identifier (e.g. "E0023" or "S0001").
    pub code: &'static str,
    /// Snake-case feature slug.
    pub feature: &'static str,
    /// True when `--skip-unsupported` (or `--skip-unsupported=<code>`) is able
    /// to silence this code; false for hard errors (`InvalidConstraint`,
    /// `UnsoundFallback`).
    pub skippable: bool,
    /// Reserved for future public documentation links. Omitted until docs exist.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub doc: Option<String>,
}

/// True when the category is silenced by `--skip-unsupported` under
/// `SkipPolicy::All`. Single source of truth for the catalogue
/// projection (`iter_catalogue`), the CLI capabilities renderer
/// (`format_capabilities_output`), and the runtime dispatcher
/// (`is_skippable_generation_error` in `verify.rs`).
///
/// Commit 18 (folds C15 #1): `is_skippable_generation_error` previously
/// open-coded the same `matches!` literal; the two could silently drift.
/// Routing both through this helper makes drift structurally impossible.
pub(crate) fn category_is_skippable(category: GenerationErrorCategory) -> bool {
    matches!(
        category,
        GenerationErrorCategory::UnsupportedShape | GenerationErrorCategory::FallbackRequired
    )
}

/// User-facing catalogue codes that the current verifier can actually surface
/// today as code-bearing runtime diagnostics.
///
/// Keep this list honest: add a code here only once a real runtime path exists.
pub(crate) const EXPOSED_CATALOGUE_CODES: &[&str] = &[
    "E0011", "E0013", "E0015", "E0016", "E0017", "E0018", "E0020", "E0023", "E0024", "E0025",
    "E0026", "E0027", "E0028", "E0029", "E0034", "E0035", "E0044", "E0050", "S0001", "S0002",
    "S0003",
];

/// Iterator over the currently surfaced catalogue entries, projected to the
/// public [`CatalogueCode`] shape. Used by `aiken verify capabilities` and by
/// CLI validation of `--skip-unsupported=<CODE>`. Dormant and informational-only
/// catalogue entries stay internal until the verifier can actually emit them as
/// code-bearing diagnostics, so the public surface does not advertise codes
/// that cannot occur today.
pub fn iter_catalogue() -> impl Iterator<Item = CatalogueCode> + 'static {
    CATALOGUE
        .iter()
        .filter(|entry| EXPOSED_CATALOGUE_CODES.contains(&entry.code))
        .map(|entry| CatalogueCode {
            code: entry.code,
            feature: entry.feature,
            skippable: !entry.informational && category_is_skippable(entry.category),
            doc: None,
        })
}
