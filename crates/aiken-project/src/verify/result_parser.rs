use super::*;
use std::collections::HashSet;

/// Translate a `ManifestEntry`'s caveat metadata into a `ProofStatus` that
/// represents a successful build. `WitnessProofNote` precedence over
/// `partial_proof_note` mirrors `manifest_entry_caveat` in `verify.rs`.
fn manifest_entry_success_status(entry: &ManifestEntry) -> ProofStatus {
    if let Some(note) = entry.witness_proof_note.clone() {
        ProofStatus::WitnessProved {
            instances: note.instances,
            witnesses: note.witnesses,
            note: note.note,
        }
    } else if let Some(note) = entry.partial_proof_note.clone() {
        ProofStatus::Partial { note }
    } else {
        ProofStatus::Proved
    }
}

/// Schema version for the `aiken verify --json` output.
///
/// Version 2 adds provenance for debug-mode widening and the two-phase proof
/// opt-out environment flag.
pub const VERIFY_SUMMARY_VERSION: &str = "2";

/// Parse lake build output into per-theorem results.
/// Uses the manifest to know which theorems were expected.
///
/// # Caller responsibility (commit 18, folds C14 #2)
///
/// The returned [`VerifySummary`] leaves the provenance fields
/// `blaster_rev`, `plutus_core_rev`, and `elapsed_ms` at their
/// default values (empty strings / `0`). The `aiken verify` CLI
/// populates them after parsing from the resolved `RunCommandOptions`
/// (see `crates/aiken/src/cmd/verify.rs`); programmatic library
/// callers (SDKs, MCP integrations, custom tooling) MUST do the
/// same before serialising the summary, otherwise downstream JSON
/// consumers will see empty-string revs and zero elapsed time. The
/// fields are intentionally serialised even when empty so that
/// missing-vs-empty is observable on the wire.
pub fn parse_verify_results(raw: VerifyResult, manifest: &GeneratedManifest) -> VerifySummary {
    if let Some(mut theorems) = raw.theorem_results.clone() {
        let proved = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::Proved))
            .count();
        let partial = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::Partial { .. }))
            .count();
        let witness = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::WitnessProved { .. }))
            .count();
        let failed = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::Failed { .. }))
            .count();
        let timed_out = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::TimedOut { .. }))
            .count();
        let unknown = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::Unknown))
            .count();

        theorems.sort_by(|a, b| {
            a.test_name
                .cmp(&b.test_name)
                .then_with(|| a.theorem_name.cmp(&b.theorem_name))
        });

        // Partial / WitnessProved do NOT contribute to command failure: the
        // build succeeded and the open / witness-only obligation is reported
        // transparently. `--accept-partial` / `--accept-witness` (commit 6)
        // gate whether the CLI exits zero in those cases.
        let proofs_ok = failed == 0 && timed_out == 0 && unknown == 0;
        return VerifySummary {
            total: theorems.len(),
            proved,
            partial,
            witness,
            failed,
            timed_out,
            unknown,
            skipped: manifest.skipped.clone(),
            theorems,
            raw_output: raw,
            elapsed_ms: None,
            command_success: proofs_ok,
            // Provenance fields are populated by the CLI layer after parsing
            // (it owns the `RunCommandOptions`/`VerifyConfig` that carry the
            // resolved revs). Default to empty so library-level callers and
            // unit tests get a self-consistent struct.
            blaster_rev: String::new(),
            plutus_core_rev: String::new(),
            allow_vacuous_subgenerators: false,
            two_phase_disabled: false,
            verify_summary_version: VERIFY_SUMMARY_VERSION,
        };
    }

    let mut theorems = Vec::new();
    let combined_output = format!("{}\n{}", raw.stdout.tail, raw.stderr.tail);

    if raw.success {
        for entry in &manifest.tests {
            // When the manifest flagged this entry's proof as partial (e.g.
            // two-phase halt with a `sorry`-closed Phase 2) or witness-only
            // (e.g. native_decide halt witnesses), surface the appropriate
            // status rather than the misleading `Proved`. The termination
            // companion, if present, is independently proved and stays
            // `Proved`.
            let correctness_status = manifest_entry_success_status(entry);
            theorems.push(TheoremResult {
                test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                theorem_name: entry.lean_theorem.clone(),
                status: correctness_status,
                over_approximations: entry.over_approximations,
            });
            if entry.has_termination_theorem {
                theorems.push(TheoremResult {
                    test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                    theorem_name: format!("{}_alwaysTerminating", entry.lean_theorem),
                    status: ProofStatus::Proved,
                    over_approximations: 0,
                });
            }
            if entry.has_equivalence_theorem {
                theorems.push(TheoremResult {
                    test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                    theorem_name: format!("{}_equivalence", entry.lean_theorem),
                    status: ProofStatus::Proved,
                    over_approximations: 0,
                });
            }
        }
    } else {
        let timed_out_run = combined_output.contains("[verify-timeout]");
        let succeeded_modules = collect_succeeded_proof_modules(&combined_output);
        let failed_modules = collect_failed_proof_modules(&combined_output, &succeeded_modules);

        for entry in &manifest.tests {
            let module_failed = failed_modules.contains(&entry.lean_module);
            let module_succeeded = succeeded_modules.contains(&entry.lean_module);
            let theorem_failed =
                theorem_has_explicit_failure(&entry.lean_theorem, &combined_output);
            let term_name = entry
                .has_termination_theorem
                .then(|| format!("{}_alwaysTerminating", entry.lean_theorem));
            let term_failed = term_name
                .as_ref()
                .is_some_and(|name| theorem_has_explicit_failure(name, &combined_output));
            let equivalence_name = entry
                .has_equivalence_theorem
                .then(|| format!("{}_equivalence", entry.lean_theorem));
            let equivalence_failed = equivalence_name
                .as_ref()
                .is_some_and(|name| theorem_has_explicit_failure(name, &combined_output));
            // Mirror run_proofs fallback behavior: keep correctness conservative
            // when a sibling termination failure is explicit, but still let the
            // termination theorem inherit module-level failure when needed.
            let correctness_fallback_to_module_failure =
                module_failed && !theorem_failed && !term_failed && !equivalence_failed;
            let termination_fallback_to_module_failure =
                module_failed && !term_failed && !equivalence_failed;
            let equivalence_fallback_to_module_failure =
                module_failed && !equivalence_failed && !term_failed;

            let correctness_status = if timed_out_run {
                ProofStatus::TimedOut {
                    reason: "Proof execution timed out".to_string(),
                }
            } else if theorem_failed || correctness_fallback_to_module_failure {
                let reason = if theorem_failed {
                    extract_error_for_theorem(&entry.lean_theorem, &combined_output)
                } else {
                    extract_error_for_module(&entry.lean_module, &combined_output)
                };
                let category = match classify_failure(&reason) {
                    FailureCategory::Unknown => FailureCategory::BuildError,
                    known => known,
                };
                ProofStatus::Failed { category, reason }
            } else if module_succeeded {
                // Sibling module(s) failed but this entry's module built
                // cleanly. If the proof was flagged partial / witness-only
                // at generation time, surface the appropriate status rather
                // than `Proved`.
                manifest_entry_success_status(entry)
            } else {
                ProofStatus::Unknown
            };

            let correctness_over_approx = if matches!(
                correctness_status,
                ProofStatus::Proved
                    | ProofStatus::Partial { .. }
                    | ProofStatus::WitnessProved { .. }
            ) {
                entry.over_approximations
            } else {
                0
            };

            theorems.push(TheoremResult {
                test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                theorem_name: entry.lean_theorem.clone(),
                status: correctness_status,
                over_approximations: correctness_over_approx,
            });

            if let Some(term_name) = term_name {
                let term_status = if timed_out_run {
                    ProofStatus::TimedOut {
                        reason: "Proof execution timed out".to_string(),
                    }
                } else if term_failed || termination_fallback_to_module_failure {
                    let reason = if term_failed {
                        extract_error_for_theorem(&term_name, &combined_output)
                    } else {
                        extract_error_for_module(&entry.lean_module, &combined_output)
                    };
                    let category = if !term_failed && theorem_failed {
                        FailureCategory::BuildError
                    } else {
                        match classify_failure(&reason) {
                            FailureCategory::Unknown => FailureCategory::BuildError,
                            known => known,
                        }
                    };
                    ProofStatus::Failed { category, reason }
                } else if module_succeeded {
                    ProofStatus::Proved
                } else {
                    ProofStatus::Unknown
                };

                theorems.push(TheoremResult {
                    test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                    theorem_name: term_name,
                    status: term_status,
                    over_approximations: 0, // termination theorem is not a two-phase proof
                });
            }

            if let Some(equivalence_name) = equivalence_name {
                let equivalence_status = if timed_out_run {
                    ProofStatus::TimedOut {
                        reason: "Proof execution timed out".to_string(),
                    }
                } else if equivalence_failed || equivalence_fallback_to_module_failure {
                    let reason = if equivalence_failed {
                        extract_error_for_theorem(&equivalence_name, &combined_output)
                    } else {
                        extract_error_for_module(&entry.lean_module, &combined_output)
                    };
                    let category = if !equivalence_failed && (theorem_failed || term_failed) {
                        FailureCategory::BuildError
                    } else {
                        match classify_failure(&reason) {
                            FailureCategory::Unknown => FailureCategory::BuildError,
                            known => known,
                        }
                    };
                    ProofStatus::Failed { category, reason }
                } else if module_succeeded {
                    ProofStatus::Proved
                } else {
                    ProofStatus::Unknown
                };

                theorems.push(TheoremResult {
                    test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                    theorem_name: equivalence_name,
                    status: equivalence_status,
                    over_approximations: 0,
                });
            }
        }
    }

    // If the run failed and no theorem received a concrete status signal
    // (proved/failed/timed out), classify the run-level failure for all theorems.
    // This avoids surfacing opaque UNKNOWN statuses for generic lake failures.
    if !raw.success
        && theorems
            .iter()
            .all(|t| matches!(t.status, ProofStatus::Unknown))
    {
        let category = classify_failure(&combined_output);
        if category != FailureCategory::Unknown {
            let reason = extract_global_failure_reason(&combined_output);
            for theorem in &mut theorems {
                theorem.status = ProofStatus::Failed {
                    category: category.clone(),
                    reason: reason.clone(),
                };
            }
        }
    }

    let proved = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::Proved))
        .count();
    let partial = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::Partial { .. }))
        .count();
    let witness = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::WitnessProved { .. }))
        .count();
    let failed = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::Failed { .. }))
        .count();
    let timed_out = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::TimedOut { .. }))
        .count();
    let unknown = theorems
        .iter()
        .filter(|t| matches!(t.status, ProofStatus::Unknown))
        .count();

    // Sort for deterministic output (important for CI diffs).
    theorems.sort_by(|a, b| {
        a.test_name
            .cmp(&b.test_name)
            .then_with(|| a.theorem_name.cmp(&b.theorem_name))
    });

    // `Partial` and `WitnessProved` are intentionally NOT in this list: the
    // build succeeded and the open / witness-only obligation is reported
    // transparently; treating them as failures would regress every two-phase
    // halt scenario without surfacing useful info. CLI-level acceptance is
    // gated by `--accept-partial` / `--accept-witness` (commit 6).
    let proofs_ok = failed == 0 && timed_out == 0 && unknown == 0;
    VerifySummary {
        total: theorems.len(),
        proved,
        partial,
        witness,
        failed,
        timed_out,
        unknown,
        skipped: manifest.skipped.clone(),
        theorems,
        raw_output: raw,
        elapsed_ms: None,
        command_success: proofs_ok,
        // Provenance fields are populated by the CLI layer after parsing
        // (it owns the `RunCommandOptions`/`VerifyConfig` that carry the
        // resolved revs). Default to empty so library-level callers and
        // unit tests get a self-consistent struct.
        blaster_rev: String::new(),
        plutus_core_rev: String::new(),
        allow_vacuous_subgenerators: false,
        two_phase_disabled: false,
        verify_summary_version: VERIFY_SUMMARY_VERSION,
    }
}

fn parse_proof_module_token(token: &str) -> Option<String> {
    let cleaned = token
        .split_whitespace()
        .next()
        .unwrap_or(token)
        .split(':')
        .next()
        .unwrap_or(token)
        .trim_end_matches([',', ';', '.'])
        .trim();

    if cleaned.starts_with("AikenVerify.Proofs.") {
        Some(cleaned.to_string())
    } else {
        None
    }
}

fn parse_proof_module_path_token(token: &str) -> Option<String> {
    let cleaned = token
        .split_whitespace()
        .next()
        .unwrap_or(token)
        .trim_matches(|c: char| c == '"' || c == '\'' || c == '`')
        .trim_end_matches([',', ';', '.'])
        .split(':')
        .next()
        .unwrap_or(token)
        .trim();

    let normalized = cleaned.replace('\\', "/");
    let stripped = normalized.strip_prefix("AikenVerify/Proofs/")?;
    let stem = stripped.strip_suffix(".lean")?;

    if stem.is_empty() {
        return None;
    }

    let module_segments: Vec<&str> = stem
        .split('/')
        .filter(|segment| !segment.is_empty())
        .collect();
    if module_segments.is_empty() {
        return None;
    }

    Some(format!("AikenVerify.Proofs.{}", module_segments.join(".")))
}

fn parse_proof_module_from_line(line: &str) -> Option<String> {
    if let Some(idx) = line.find("AikenVerify.Proofs.")
        && let Some(module) = parse_proof_module_token(&line[idx..])
    {
        return Some(module);
    }

    if let Some(idx) = line.find("AikenVerify/Proofs/")
        && let Some(module) = parse_proof_module_path_token(&line[idx..])
    {
        return Some(module);
    }

    None
}

fn collect_succeeded_proof_modules(output: &str) -> HashSet<String> {
    let mut modules = HashSet::new();

    for line in output.lines() {
        if let Some(idx) = line.find("Built ") {
            let token = &line[idx + "Built ".len()..];
            if let Some(module) = parse_proof_module_token(token) {
                modules.insert(module);
            }
        }

        if let Some(idx) = line.find("Replayed ") {
            let token = &line[idx + "Replayed ".len()..];
            if let Some(module) = parse_proof_module_token(token) {
                modules.insert(module);
            }
        }
    }

    modules
}

fn collect_failed_proof_modules(
    output: &str,
    succeeded_modules: &HashSet<String>,
) -> HashSet<String> {
    let mut modules = HashSet::new();
    let mut building_candidates = Vec::new();
    let mut last_building_module = None;
    let mut saw_error_line = false;

    for line in output.lines() {
        if let Some(idx) = line.find("Building ") {
            let token = &line[idx + "Building ".len()..];
            if let Some(module) = parse_proof_module_token(token) {
                let failure_marker = line.contains('✖');
                last_building_module = Some(module.clone());
                building_candidates.push(module.clone());

                if failure_marker {
                    modules.insert(module);
                }
            }
        }

        if let Some(token) = line.trim().strip_prefix("- ") {
            let module =
                parse_proof_module_token(token).or_else(|| parse_proof_module_path_token(token));
            if let Some(module) = module {
                modules.insert(module);
            }
            continue;
        }

        if line.to_ascii_lowercase().contains("error:") {
            saw_error_line = true;
            if let Some(module) = parse_proof_module_from_line(line) {
                modules.insert(module);
            } else if let Some(module) = last_building_module.clone()
                && !succeeded_modules.contains(&module)
            {
                modules.insert(module);
            }
        }
    }

    if modules.is_empty()
        && saw_error_line
        && let Some(module) = building_candidates
            .iter()
            .rev()
            .find(|module| !succeeded_modules.contains(*module))
    {
        modules.insert(module.clone());
    }

    modules
}

fn is_lean_identifier_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

fn contains_token_with_boundaries<F>(line: &str, token: &str, mut is_token_char: F) -> bool
where
    F: FnMut(char) -> bool,
{
    if token.is_empty() {
        return false;
    }

    let mut search_start = 0;
    while let Some(offset) = line[search_start..].find(token) {
        let start = search_start + offset;
        let end = start + token.len();

        let before = line[..start].chars().next_back();
        let after = line[end..].chars().next();
        let before_ok = before.map(|c| !is_token_char(c)).unwrap_or(true);
        let after_ok = after.map(|c| !is_token_char(c)).unwrap_or(true);

        if before_ok && after_ok {
            return true;
        }

        search_start = end;
    }

    false
}

fn contains_identifier_token(line: &str, token: &str) -> bool {
    contains_token_with_boundaries(line, token, is_lean_identifier_char)
}

fn line_has_theorem_reference(line: &str, theorem: &str) -> bool {
    line.contains(&format!("'{theorem}'")) || contains_identifier_token(line, theorem)
}

fn line_has_length_indexed_variant(line: &str, base: &str) -> bool {
    if base.is_empty() {
        return false;
    }
    // Match `{base}_l<digits>` and `{base}_l<digits>_alwaysTerminating`.
    // The base name must have a non-identifier char (or start-of-string) before it,
    // then `_l` followed by one or more ASCII digits, then either a non-identifier
    // char or the suffix `_alwaysTerminating`.
    let mut start = 0;
    while let Some(off) = line[start..].find(base) {
        let s = start + off;
        let e = s + base.len();
        let before_ok = line[..s]
            .chars()
            .next_back()
            .map(|c| !is_lean_identifier_char(c))
            .unwrap_or(true);
        let rest = &line[e..];
        if before_ok && rest.starts_with("_l") {
            let digits_end = 2 + rest[2..].chars().take_while(|c| c.is_ascii_digit()).count();
            if digits_end > 2 {
                // There's at least one digit
                let after = rest[digits_end..].chars().next();
                let after_ok = after.map(|c| !is_lean_identifier_char(c)).unwrap_or(true);
                const AT_SUFFIX: &str = "_alwaysTerminating";
                let at_rest = &rest[digits_end..];
                let at_ok = if let Some(stripped) = at_rest.strip_prefix(AT_SUFFIX) {
                    let after_at = stripped.chars().next();
                    after_at
                        .map(|c| !is_lean_identifier_char(c))
                        .unwrap_or(true)
                } else {
                    false
                };
                if after_ok || at_ok {
                    return true;
                }
            }
        }
        start = e;
    }
    false
}

fn is_lean_module_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == '.'
}

fn is_lean_module_path_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == '/'
}

fn line_has_module_reference(line: &str, module: &str, module_path: &str) -> bool {
    contains_token_with_boundaries(line, module, is_lean_module_char)
        || contains_token_with_boundaries(line, module_path, is_lean_module_path_char)
}

fn line_has_error_marker(line: &str) -> bool {
    line.to_ascii_lowercase().contains("error:")
}

pub(super) fn theorem_has_explicit_failure(theorem: &str, output: &str) -> bool {
    output
        .lines()
        .filter(|line| line_has_error_marker(line))
        .any(|line| {
            line_has_theorem_reference(line, theorem)
                || line_has_length_indexed_variant(line, theorem)
        })
}

/// Classify a failure based on build output content.
pub(super) fn classify_failure(output: &str) -> FailureCategory {
    if output.contains("❌ Falsified") || output.contains("Counterexample:") {
        FailureCategory::Counterexample
    } else if output.contains("Tactic `blaster` failed")
        || output.contains("unsolved goals")
        || output.contains("tactic 'blaster' failed")
    {
        FailureCategory::UnsatGoal
    } else if output.contains("deterministic timeout") || output.contains("tactic timed out") {
        FailureCategory::Timeout
    } else if output.contains("unknown package")
        || output.contains("could not resolve")
        || output.contains("lake fetch")
        || output.contains("lake update")
    {
        FailureCategory::DependencyError
    } else if output.contains("translateApp: Inductive predicate not yet supported")
        || (output.contains("translateApp:") && output.contains("not yet supported"))
        || output.contains("Lean.Expr.const `List.Mem")
    {
        FailureCategory::BlasterUnsupported
    } else if output.contains("error:") {
        FailureCategory::BuildError
    } else {
        FailureCategory::Unknown
    }
}

fn redact_absolute_paths(text: &str) -> String {
    text.lines()
        .map(redact_absolute_paths_in_line)
        .collect::<Vec<_>>()
        .join("\n")
}

fn redact_absolute_paths_in_line(line: &str) -> String {
    let mut redacted = String::with_capacity(line.len());
    let mut cursor = 0;

    while let Some(start) = next_absolute_path_start(line, cursor) {
        redacted.push_str(&line[cursor..start]);

        let end = absolute_path_token_end(line, start);
        let token = &line[start..end];
        redacted.push_str(&redact_absolute_path_token(token));
        cursor = end;
    }

    redacted.push_str(&line[cursor..]);
    redacted
}

fn next_absolute_path_start(line: &str, cursor: usize) -> Option<usize> {
    line[cursor..].char_indices().find_map(|(offset, _)| {
        let start = cursor + offset;
        (is_absolute_boundary(line, start) && is_absolute_path_start(line, start)).then_some(start)
    })
}

fn is_absolute_boundary(line: &str, start: usize) -> bool {
    start == 0
        || line[..start]
            .chars()
            .next_back()
            .is_some_and(|ch| ch.is_whitespace() || matches!(ch, '(' | '[' | '{' | '"' | '\''))
}

fn is_absolute_path_start(line: &str, start: usize) -> bool {
    let bytes = line.as_bytes();

    bytes.get(start) == Some(&b'/')
        || (bytes.get(start) == Some(&b'\\') && bytes.get(start + 1) == Some(&b'\\'))
        || bytes.get(start..start + 3).is_some_and(|segment| {
            segment[0].is_ascii_alphabetic()
                && segment[1] == b':'
                && matches!(segment[2], b'/' | b'\\')
        })
}

fn absolute_path_token_end(line: &str, start: usize) -> usize {
    if let Some(end) = quoted_absolute_path_end(line, start) {
        return end;
    }

    if let Some(end) = diagnostic_absolute_path_end(&line[start..]) {
        return start + end;
    }

    line[start..]
        .char_indices()
        .skip(1)
        .find_map(|(offset, ch)| matches!(ch, ',' | ')' | ']' | '}' | '"').then_some(start + offset))
        .unwrap_or(line.len())
}

fn is_escaped_quote(line: &str, quote_index: usize) -> bool {
    let bytes = line.as_bytes();
    let mut backslashes = 0usize;
    let mut cursor = quote_index;

    while cursor > 0 && bytes[cursor - 1] == b'\\' {
        backslashes += 1;
        cursor -= 1;
    }

    backslashes % 2 == 1
}

fn quoted_absolute_path_end(line: &str, start: usize) -> Option<usize> {
    let quote = line[..start]
        .chars()
        .next_back()
        .filter(|ch| matches!(ch, '"' | '\''))?;

    line[start..].char_indices().skip(1).find_map(|(offset, ch)| {
        if ch != quote {
            return None;
        }

        let end = start + offset;
        if is_escaped_quote(line, end) {
            return None;
        }

        let next = line[end + ch.len_utf8()..].chars().next();
        next.is_none_or(|next| {
            next.is_whitespace() || matches!(next, ':' | ',' | ')' | ']' | '}' | '"' | '\'')
        })
        .then_some(end)
    })
}

fn diagnostic_absolute_path_end(segment: &str) -> Option<usize> {
    let bytes = segment.as_bytes();
    let mut index = 0;

    while index < bytes.len() {
        if bytes[index] != b':' {
            index += 1;
            continue;
        }

        let mut line_digits_end = index + 1;
        while line_digits_end < bytes.len() && bytes[line_digits_end].is_ascii_digit() {
            line_digits_end += 1;
        }
        if line_digits_end == index + 1
            || line_digits_end >= bytes.len()
            || bytes[line_digits_end] != b':'
        {
            index += 1;
            continue;
        }

        let mut column_digits_end = line_digits_end + 1;
        while column_digits_end < bytes.len() && bytes[column_digits_end].is_ascii_digit() {
            column_digits_end += 1;
        }
        if column_digits_end == line_digits_end + 1 {
            index += 1;
            continue;
        }

        if column_digits_end < bytes.len() && bytes[column_digits_end] == b':' {
            column_digits_end += 1;
        }

        return Some(column_digits_end);
    }

    None
}

fn split_diagnostic_suffix(path: &str) -> (&str, &str) {
    let trimmed = path.strip_suffix(':').unwrap_or(path);
    let mut parts = trimmed.rsplitn(3, ':');
    let Some(column) = parts.next() else {
        return (path, "");
    };
    let Some(line) = parts.next() else {
        return (path, "");
    };
    let Some(prefix) = parts.next() else {
        return (path, "");
    };

    if column.is_empty()
        || line.is_empty()
        || !column.chars().all(|ch| ch.is_ascii_digit())
        || !line.chars().all(|ch| ch.is_ascii_digit())
    {
        return (path, "");
    }

    (&path[..prefix.len()], &path[prefix.len()..])
}

fn minimum_redacted_path_components(components: &[&str]) -> usize {
    match components {
        // Home-directory roots need one extra elision so shallow checkout
        // paths like `/Users/alice/project` do not leak the username.
        [first, _, ..]
            if first.eq_ignore_ascii_case("Users") || first.eq_ignore_ascii_case("home") =>
        {
            2
        }
        _ => 1,
    }
}

fn redact_to_bare_ellipsis(path: &str, components: &[&str]) -> bool {
    if path.starts_with("\\\\") {
        return components.is_empty();
    }

    components.first().is_some_and(|first| {
        (first.eq_ignore_ascii_case("Users") || first.eq_ignore_ascii_case("home"))
            && components.len() <= 2
    })
}

fn redact_absolute_path_token(token: &str) -> String {
    let core_end = token.trim_end_matches([',', ')', ']', '}']).len();
    let (core, suffix) = token.split_at(core_end);
    let (path, diagnostic_suffix) = split_diagnostic_suffix(core);

    if !is_absolute_path_start(path, 0) {
        return token.to_string();
    }

    let is_windows_drive = path.as_bytes().get(1) == Some(&b':');
    let is_unc = path.starts_with("\\\\");
    let separator = if is_windows_drive || is_unc { '\\' } else { '/' };
    let escaped_quote_sentinel = '\0';
    let normalized_path = if separator == '\\' {
        path.replace(r#"\""#, &escaped_quote_sentinel.to_string())
    } else {
        path.to_string()
    };
    let mut components = if separator == '\\' {
        normalized_path
            .split(['\\', '/'])
            .filter(|component| !component.is_empty())
            .map(ToString::to_string)
            .collect::<Vec<_>>()
    } else {
        normalized_path
            .split('/')
            .filter(|component| !component.is_empty())
            .map(ToString::to_string)
            .collect::<Vec<_>>()
    };

    if is_windows_drive {
        components = components.into_iter().skip(1).collect();
    } else if is_unc {
        components = components.into_iter().skip(2).collect();
    }

    let component_refs = components.iter().map(String::as_str).collect::<Vec<_>>();
    if components.is_empty() || redact_to_bare_ellipsis(path, &component_refs) {
        return format!("…{}{}", diagnostic_suffix, suffix);
    }

    let keep_from = components.len().saturating_sub(3).max(
        minimum_redacted_path_components(&component_refs).min(components.len().saturating_sub(1)),
    );
    let separator = separator.to_string();
    let visible = components[keep_from..].join(&separator);
    let visible = if separator == "\\" {
        visible.replace(escaped_quote_sentinel, r#"\""#)
    } else {
        visible
    };

    format!("…{}{}{}{}", separator, visible, diagnostic_suffix, suffix)
}

/// Extract error context for a theorem from build output.
/// Includes the matching line plus surrounding context lines (up to `CONTEXT_LINES`
/// before and after each match) for more useful diagnostics.
fn extract_error_for_pattern(pattern: &str, output: &str) -> String {
    extract_error_for_match(output, |line| line.contains(pattern))
}

fn extract_error_for_match<F>(output: &str, mut matches: F) -> String
where
    F: FnMut(&str) -> bool,
{
    const CONTEXT_LINES: usize = 3;
    let lines: Vec<&str> = output.lines().collect();
    let mut included = vec![false; lines.len()];
    let mut matched = false;

    for (i, line) in lines.iter().enumerate() {
        if matches(line) {
            matched = true;
            let start = i.saturating_sub(CONTEXT_LINES);
            let end = (i + CONTEXT_LINES + 1).min(lines.len());
            for included_flag in included.iter_mut().take(end).skip(start) {
                *included_flag = true;
            }
        }
    }

    if !matched {
        return String::new();
    }

    let mut result = Vec::new();
    let mut last_included = false;
    for (i, line) in lines.iter().enumerate() {
        if included[i] {
            if !last_included && !result.is_empty() {
                result.push("  ...");
            }
            result.push(*line);
            last_included = true;
        } else {
            last_included = false;
        }
    }

    redact_absolute_paths(&result.join("\n"))
}

pub(super) fn extract_error_for_module(module: &str, output: &str) -> String {
    let module_path = module.replace('.', "/");
    extract_error_for_match(output, |line| {
        line_has_module_reference(line, module, &module_path)
    })
}

pub(super) fn extract_error_for_theorem(theorem: &str, output: &str) -> String {
    extract_error_for_match(output, |line| {
        line_has_theorem_reference(line, theorem) || line_has_length_indexed_variant(line, theorem)
    })
}

pub(super) fn extract_global_failure_reason(output: &str) -> String {
    let extracted = extract_error_for_pattern("error:", output);
    if !extracted.trim().is_empty() {
        return extracted;
    }

    let fallback = output
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .take(20)
        .collect::<Vec<_>>()
        .join("\n");

    redact_absolute_paths(&fallback)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn redact_absolute_paths_collapses_prefixes() {
        let line =
            "error: /Users/alice/work/build/verify/AikenVerify/Proofs/Foo.lean:15:5: build failed";
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains("/Users/alice/work"));
        assert_eq!(
            redacted,
            "error: …/AikenVerify/Proofs/Foo.lean:15:5: build failed"
        );
    }

    #[test]
    fn redact_absolute_paths_leaves_relative_proof_paths_untouched() {
        let line = "error: AikenVerify/Proofs/Foo.lean:15:5: build failed";
        assert_eq!(redact_absolute_paths_in_line(line), line);
    }

    #[test]
    fn redact_absolute_paths_keeps_diagnostic_paths_with_spaces_intact() {
        let line =
            "error: /Users/alice/work/Aiken Verify/Proofs/Foo Bar.lean:15:5: build failed";
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains("/Users/alice/work"));
        assert_eq!(
            redacted,
            "error: …/Aiken Verify/Proofs/Foo Bar.lean:15:5: build failed"
        );
    }

    #[test]
    fn redact_absolute_paths_handles_quoted_paths_with_spaces() {
        let line =
            "error: \"/Users/alice/work/Aiken Verify/Proofs/Foo Bar.lean\" failed";
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains("/Users/alice/work"));
        assert_eq!(
            redacted,
            "error: \"…/Aiken Verify/Proofs/Foo Bar.lean\" failed"
        );
    }

    #[test]
    fn redact_absolute_paths_handles_quoted_posix_paths_with_escaped_quotes() {
        let line = r#"error: "/Users/alice/work/Foo\"":15:5: build failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains("/Users/alice/work"));
        assert_eq!(redacted, r#"error: "…/work/Foo\"":15:5: build failed"#);
    }

    #[test]
    fn redact_absolute_paths_handles_unquoted_posix_paths_with_spaces() {
        let line = "error: /Users/alice/work/Aiken Verify failed";
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains("/Users/alice/work"));
        assert_eq!(redacted, "error: …/work/Aiken Verify failed");
    }

    #[test]
    fn redact_absolute_paths_redacts_windows_drive_diagnostics_with_spaces() {
        let line =
            r#"error: C:\Users\alice\Aiken Verify\Proofs\Foo Bar.lean:15:5: build failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains(r#"C:\Users\alice"#));
        assert_eq!(
            redacted,
            r#"error: …\Aiken Verify\Proofs\Foo Bar.lean:15:5: build failed"#
        );
    }

    #[test]
    fn redact_absolute_paths_handles_quoted_windows_drive_paths() {
        let line =
            r#"error: "C:\Users\alice\Aiken Verify\Proofs\Foo Bar.lean" failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains(r#"C:\Users\alice"#));
        assert_eq!(
            redacted,
            r#"error: "…\Aiken Verify\Proofs\Foo Bar.lean" failed"#
        );
    }

    #[test]
    fn redact_absolute_paths_handles_quoted_windows_drive_paths_with_escaped_quotes() {
        let line =
            r#"error: "C:\Users\alice\Aiken Verify\Proofs\Foo\"":15:5: build failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains(r#"C:\Users\alice"#));
        assert_eq!(
            redacted,
            r#"error: "…\Aiken Verify\Proofs\Foo\"":15:5: build failed"#
        );
    }

    #[test]
    fn redact_absolute_paths_handles_unquoted_windows_drive_paths_with_spaces() {
        let line = r#"error: C:\Users\Jane Doe\Aiken Verify failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains(r#"C:\Users\Jane Doe"#));
        assert_eq!(redacted, r#"error: …\Aiken Verify failed"#);
    }

    #[test]
    fn redact_absolute_paths_redacts_unc_diagnostics_with_spaces() {
        let line =
            r#"error: \\server\share\Aiken Verify\Proofs\Foo Bar.lean:7:9: build failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains(r#"\\server\share"#));
        assert_eq!(
            redacted,
            r#"error: …\Proofs\Foo Bar.lean:7:9: build failed"#
        );
    }

    #[test]
    fn redact_absolute_paths_handles_quoted_unc_paths() {
        let line =
            r#"error: "\\server\share\Aiken Verify\Proofs\Foo Bar.lean" failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains(r#"\\server\share"#));
        assert_eq!(
            redacted,
            r#"error: "…\Proofs\Foo Bar.lean" failed"#
        );
    }

    #[test]
    fn redact_absolute_paths_handles_quoted_unc_paths_with_escaped_quotes() {
        let line =
            r#"error: "\\server\share\Aiken Verify\Proofs\Foo\"":7:9: build failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains(r#"\\server\share"#));
        assert_eq!(redacted, r#"error: "…\Proofs\Foo\"":7:9: build failed"#);
    }

    #[test]
    fn redact_absolute_paths_handles_unquoted_unc_paths_with_spaces() {
        let line = r#"error: \\server\share\Team Folder\Proofs Failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains(r#"\\server\share"#));
        assert_eq!(redacted, r#"error: …\Proofs Failed"#);
    }


    #[test]
    fn redact_absolute_paths_redacts_shallow_posix_home_paths() {
        let line = "error: /Users/alice/aiken/Foo.lean:12:3: build failed";
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains("/Users/alice"));
        assert_eq!(redacted, "error: …/aiken/Foo.lean:12:3: build failed");

        let directory_line = "error: /Users/alice/aiken failed";
        assert_eq!(
            redact_absolute_paths_in_line(directory_line),
            "error: …/aiken failed"
        );
    }

    #[test]
    fn redact_absolute_paths_redacts_home_root_only_posix_paths() {
        let line = "error: /Users/alice";
        assert_eq!(redact_absolute_paths_in_line(line), "error: …");
    }

    #[test]
    fn redact_absolute_paths_redacts_shallow_windows_drive_paths() {
        let line = r#"error: C:\src\proj\Foo.lean:12:3: build failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains(r#"C:\src"#));
        assert_eq!(redacted, r#"error: …\proj\Foo.lean:12:3: build failed"#);
    }

    #[test]
    fn redact_absolute_paths_redacts_windows_home_root_only_paths() {
        let line = r#"error: C:\Users\alice"#;
        assert_eq!(redact_absolute_paths_in_line(line), r#"error: …"#);
    }

    #[test]
    fn redact_absolute_paths_redacts_shallow_unc_paths() {
        let line = r#"error: \\server\share\proj\Foo.lean:7:9: build failed"#;
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains(r#"\\server\share"#));
        assert_eq!(redacted, r#"error: …\Foo.lean:7:9: build failed"#);
    }

    #[test]
    fn redact_absolute_paths_redacts_unc_share_roots() {
        let line = r#"error: \\server\share"#;
        assert_eq!(redact_absolute_paths_in_line(line), r#"error: …"#);
    }

    #[test]
    fn extract_global_failure_reason_redacts_absolute_paths() {
        let output = "error: /tmp/project/validators/fixture.ak:12:3: parse failure";
        let reason = extract_global_failure_reason(output);

        assert!(!reason.contains("/tmp/project"));
        assert_eq!(
            reason,
            "error: …/project/validators/fixture.ak:12:3: parse failure"
        );
    }

    #[test]
    fn redact_absolute_paths_handles_single_quoted_paths_with_apostrophes() {
        let line = "error: '/Users/alice/team's/Foo.lean' failed";
        let redacted = redact_absolute_paths_in_line(line);

        assert!(!redacted.contains("/Users/alice"));
        assert_eq!(redacted, "error: '…/team's/Foo.lean' failed");
    }

    #[test]
    fn length_indexed_variant_matches_ln_suffix() {
        // base `foo` should match `foo_l3` but NOT `foo` bare or `foo_long_thing`
        assert!(line_has_length_indexed_variant(
            "error: foo_l3 failed",
            "foo"
        ));
        assert!(line_has_length_indexed_variant(
            "error: foo_l0 failed",
            "foo"
        ));
        assert!(line_has_length_indexed_variant(
            "error: foo_l10 failed",
            "foo"
        ));
        assert!(line_has_length_indexed_variant(
            "error: foo_l3_alwaysTerminating failed",
            "foo"
        ));
        // should NOT match plain foo
        assert!(!line_has_length_indexed_variant("error: foo failed", "foo"));
        // should NOT match foo_long_thing (not digits after _l)
        assert!(!line_has_length_indexed_variant(
            "error: foo_long_thing failed",
            "foo"
        ));
        // should NOT match if foo is a suffix of another identifier (bad prefix)
        assert!(!line_has_length_indexed_variant(
            "error: bar_foo_l3 failed",
            "foo"
        ));
        // SHOULD match when at start of string
        assert!(line_has_length_indexed_variant("foo_l5 failed", "foo"));
    }

    #[test]
    fn line_has_theorem_reference_does_not_match_ln_suffix() {
        // Confirm the existing function does NOT match _lN — that's the bug this fixes
        assert!(!line_has_theorem_reference("error: foo_l3 failed", "foo"));
    }

    #[test]
    fn length_indexed_variant_rejects_empty_base() {
        // Must return false immediately without hanging.
        assert!(!line_has_length_indexed_variant("error: _l3 failed", ""));
        assert!(!line_has_length_indexed_variant("", ""));
        assert!(!line_has_length_indexed_variant("nothing here", ""));
    }

    #[test]
    fn length_indexed_variant_rejects_missing_or_non_digit_after_l() {
        // `_l` with no digits
        assert!(!line_has_length_indexed_variant(
            "error: foo_l failed",
            "foo"
        ));
        assert!(!line_has_length_indexed_variant(
            "error: foo_l_bar failed",
            "foo"
        ));
        // `_l` followed by a letter
        assert!(!line_has_length_indexed_variant(
            "error: foo_lx failed",
            "foo"
        ));
    }

    #[test]
    fn length_indexed_variant_rejects_identifier_char_after_digits() {
        // digits immediately followed by identifier char (not _alwaysTerminating)
        assert!(!line_has_length_indexed_variant(
            "error: foo_l3abc failed",
            "foo"
        ));
        assert!(!line_has_length_indexed_variant(
            "error: foo_l3x failed",
            "foo"
        ));
        // `_` followed by something other than alwaysTerminating
        assert!(!line_has_length_indexed_variant(
            "error: foo_l3_other failed",
            "foo"
        ));
    }

    #[test]
    fn length_indexed_variant_rejects_always_terminating_without_boundary() {
        // `_alwaysTerminatingExtra` must NOT match (no boundary after the suffix)
        assert!(!line_has_length_indexed_variant(
            "error: foo_l3_alwaysTerminatingExtra failed",
            "foo"
        ));
    }

    #[test]
    fn length_indexed_variant_accepts_always_terminating_with_valid_boundary() {
        // `_alwaysTerminating` followed by space — valid
        assert!(line_has_length_indexed_variant(
            "error: foo_l3_alwaysTerminating failed",
            "foo"
        ));
        // at end of string — valid
        assert!(line_has_length_indexed_variant(
            "foo_l3_alwaysTerminating",
            "foo"
        ));
    }

    #[test]
    fn length_indexed_variant_continues_past_bad_prefix_match() {
        // First occurrence has bad prefix (x before foo); second is valid
        assert!(line_has_length_indexed_variant(
            "xfoo and foo_l3 failed",
            "foo"
        ));
        // `foofoo_l3` — the valid `foo` occurrence has `o` as preceding char → rejects
        assert!(!line_has_length_indexed_variant(
            "error: foofoo_l3 failed",
            "foo"
        ));
    }

    #[test]
    fn length_indexed_variant_matches_multi_digit_lengths() {
        assert!(line_has_length_indexed_variant(
            "error: foo_l10 failed",
            "foo"
        ));
        assert!(line_has_length_indexed_variant(
            "error: foo_l100 failed",
            "foo"
        ));
    }
}
