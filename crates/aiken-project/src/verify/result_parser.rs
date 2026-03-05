use super::*;
use std::collections::HashSet;

/// Parse lake build output into per-theorem results.
/// Uses the manifest to know which theorems were expected.
pub fn parse_verify_results(raw: VerifyResult, manifest: &GeneratedManifest) -> VerifySummary {
    if let Some(mut theorems) = raw.theorem_results.clone() {
        let proved = theorems
            .iter()
            .filter(|t| matches!(t.status, ProofStatus::Proved))
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

        return VerifySummary {
            total: theorems.len(),
            proved,
            failed,
            timed_out,
            unknown,
            skipped: manifest.skipped.clone(),
            fallbacks: manifest.fallbacks.clone(),
            theorems,
            raw_output: raw,
            elapsed_ms: None,
        };
    }

    let mut theorems = Vec::new();
    let combined_output = format!("{}\n{}", raw.stdout, raw.stderr);

    if raw.success {
        for entry in &manifest.tests {
            theorems.push(TheoremResult {
                test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                theorem_name: entry.lean_theorem.clone(),
                status: ProofStatus::Proved,
            });
            if entry.has_termination_theorem {
                theorems.push(TheoremResult {
                    test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                    theorem_name: format!("{}_alwaysTerminating", entry.lean_theorem),
                    status: ProofStatus::Proved,
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
            // Mirror run_proofs fallback behavior: keep correctness conservative
            // when a sibling termination failure is explicit, but still let the
            // termination theorem inherit module-level failure when needed.
            let correctness_fallback_to_module_failure =
                module_failed && !theorem_failed && !term_failed;
            let termination_fallback_to_module_failure = module_failed && !term_failed;

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
                ProofStatus::Proved
            } else {
                ProofStatus::Unknown
            };

            theorems.push(TheoremResult {
                test_name: format!("{}.{}", entry.aiken_module, entry.aiken_name),
                theorem_name: entry.lean_theorem.clone(),
                status: correctness_status,
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

    VerifySummary {
        total: theorems.len(),
        proved,
        failed,
        timed_out,
        unknown,
        skipped: manifest.skipped.clone(),
        fallbacks: manifest.fallbacks.clone(),
        theorems,
        raw_output: raw,
        elapsed_ms: None,
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
        .trim_end_matches(|c: char| c == ',' || c == ';' || c == '.')
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
        .trim_end_matches(|c: char| c == ',' || c == ';' || c == '.')
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
    if let Some(idx) = line.find("AikenVerify.Proofs.") {
        if let Some(module) = parse_proof_module_token(&line[idx..]) {
            return Some(module);
        }
    }

    if let Some(idx) = line.find("AikenVerify/Proofs/") {
        if let Some(module) = parse_proof_module_path_token(&line[idx..]) {
            return Some(module);
        }
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

    if modules.is_empty() && saw_error_line {
        if let Some(module) = building_candidates
            .iter()
            .rev()
            .find(|module| !succeeded_modules.contains(*module))
        {
            modules.insert(module.clone());
        }
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
        .any(|line| line_has_theorem_reference(line, theorem))
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

    result.join("\n")
}

pub(super) fn extract_error_for_module(module: &str, output: &str) -> String {
    let module_path = module.replace('.', "/");
    extract_error_for_match(output, |line| {
        line_has_module_reference(line, module, &module_path)
    })
}

pub(super) fn extract_error_for_theorem(theorem: &str, output: &str) -> String {
    extract_error_for_match(output, |line| line_has_theorem_reference(line, theorem))
}

pub(super) fn extract_global_failure_reason(output: &str) -> String {
    let extracted = extract_error_for_pattern("error:", output);
    if !extracted.trim().is_empty() {
        return extracted;
    }

    output
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .take(20)
        .collect::<Vec<_>>()
        .join("\n")
}
