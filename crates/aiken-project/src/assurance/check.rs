//! Fresh checking against the pinned consumer's text-only verdict protocol.

use super::{
    consumer::{ConsumerWorkspace, DRIVER_IMPORTS, TemporaryDirectory, lean_path_literal},
    generate::{Environment, civil_date, replace_file},
};
use crate::{
    Project,
    telemetry::{
        Event, EventListener, VerificationProperty, VerificationReport, VerificationTrust,
        VerificationVerdict,
    },
};
use aiken_lang::ast::Tracing;
use aiken_ual as ual;
use miette::{IntoDiagnostic, WrapErr};
use regex::Regex;
use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    io::{Cursor, Write},
    path::Path,
    process::{Child, Command, ExitStatus, Stdio},
    thread,
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

const METHOD: &str = "Blaster SMT; no reconstructed Lean proof";
const BUDGET: &str = "Results are relative to each checking context's declared CEK step budget, not unbounded execution or ledger acceptance.";

fn empty_report(title: String) -> VerificationReport {
    VerificationReport {
        title,
        trust: VerificationTrust {
            method: METHOD.to_owned(),
            z3: "unknown (consumer environment not inspected)".to_owned(),
            revisions: [
                "PlutusCoreBlaster",
                "Lean-blaster",
                "CardanoLedgerApiBlaster",
            ]
            .into_iter()
            .map(|name| (name.to_owned(), "unknown".to_owned()))
            .collect(),
            budget: BUDGET.to_owned(),
        },
        properties: Vec::new(),
        warnings: Vec::new(),
        errors: Vec::new(),
        success: false,
    }
}

/// Emit the same complete report even when CLI project discovery fails.
pub fn report_failure(listener: &impl EventListener, title: String, error: String) {
    let mut report = empty_report(title);
    report.errors.push(error);
    listener.handle_event(Event::StartingVerification {
        trust: report.trust.clone(),
    });
    listener.handle_event(Event::FinishedVerification { report });
}

/// Every attempt emits a finished report, including failures before compilation.
/// Return true only after validated, byte-bound evidence has been published.
pub fn check_assurance(
    root: &Path,
    environment: &Path,
    timeout: Duration,
    tracing: Tracing,
    env: Option<String>,
    listener: &impl EventListener,
) -> bool {
    let mut report = empty_report(root.display().to_string());
    let mut started = false;
    let result = (|| -> miette::Result<()> {
        let output = root.join("assurance");
        clear_previous_run(&output)?;
        if timeout.is_zero() {
            return Err(miette::miette!(
                "verification timeout must be greater than zero"
            ));
        }
        let workspace = ConsumerWorkspace::load(root)?;
        let (revisions, warnings) = workspace.verification_revisions()?;
        report.trust.revisions = revisions;
        report.warnings.extend(warnings);
        match solver_identity(&workspace, timeout.min(Duration::from_secs(10))) {
            Ok(identity) => report.trust.z3 = identity,
            Err(error) => {
                report
                    .warnings
                    .push(format!("cannot inspect actual Z3 binary: {error:?}"));
                return Err(error);
            }
        }
        listener.handle_event(Event::StartingVerification {
            trust: report.trust.clone(),
        });
        started = true;
        let environment_manifest = Environment::read(environment)?;
        // The listener reference implements the trait through this local adapter,
        // avoiding a requirement that callers or their event sinks be cloneable.
        let mut project =
            Project::new(root.to_path_buf(), ForwardEvents(listener)).map_err(|error| {
                error.report();
                miette::miette!("cannot load project configuration")
            })?;
        report.title = project.config.name.to_string();
        let generated =
            project.generate_assurance(Some(&environment_manifest), false, tracing, env);
        for warning in project.warnings() {
            warning.report();
        }
        generated?;
        report.warnings.extend(project.assurance_scope_warnings());
        let output = output.canonicalize().into_diagnostic()?;
        let assurance_bytes = fs::read(output.join("assurance.json")).into_diagnostic()?;
        let mut assurance: ual::AssuranceDocument =
            serde_json::from_slice(&assurance_bytes).into_diagnostic()?;
        report.title.clone_from(&assurance.preamble.title);
        report.properties = assurance
            .properties
            .iter()
            .map(|property| VerificationProperty {
                id: property.id.clone(),
                verdict: VerificationVerdict::Missing,
                counterexample: None,
            })
            .collect();
        if report.properties.is_empty() {
            return Err(miette::miette!("no assurance properties to verify"));
        }
        let driver = output.join("Check.lean");
        replace_file(
            &driver,
            format!(
                "{DRIVER_IMPORTS}open CardanoLedgerApi.V3 CardanoLedgerApi.V3.Contexts.ScriptInfo\n#verify_blueprint AikenAssurance {}\n",
                lean_path_literal(&output.join("assurance.json"))?
            )
            .as_bytes(),
        )?;
        let log_path = output.join("verification.log");
        let run = run_process(&mut workspace.lean_command(&driver), &log_path, timeout);
        // Read the log even when the process timed out or exited unsuccessfully.
        let log = fs::read_to_string(&log_path)
            .into_diagnostic()
            .wrap_err("cannot read consumer verification log")?;
        report.errors.extend(PinnedVerdictReader::read(
            &log,
            &report.title,
            &mut report.properties,
        ));
        if !log.is_empty() {
            eprint!("{log}");
            if !log.ends_with('\n') {
                eprintln!();
            }
        }
        if (!run.as_ref().is_ok_and(|status| status.success()) || !report.errors.is_empty())
            && !log.is_empty()
            && !report
                .properties
                .iter()
                .any(|property| property.counterexample.is_some())
        {
            report.errors.push(format!("Consumer diagnostics:\n{log}"));
        }
        let status = run?;
        if !status.success() {
            report.errors.push(format!(
                "Lean consumer exited unsuccessfully ({status}); see {}",
                log_path.display()
            ));
        }
        if report
            .properties
            .iter()
            .any(|property| !matches!(property.verdict, VerificationVerdict::Verified))
        {
            report
                .errors
                .push("fresh verification did not verify every property".to_owned());
        }
        if !report.errors.is_empty() {
            return Err(miette::miette!(
                "fresh verification failed; see {}",
                log_path.display()
            ));
        }
        publish_evidence(
            &output,
            environment,
            &assurance_bytes,
            &mut assurance,
            &report,
        )?;
        Ok(())
    })();
    if !started {
        listener.handle_event(Event::StartingVerification {
            trust: report.trust.clone(),
        });
    }
    match result {
        Ok(()) => report.success = true,
        Err(error) => report.errors.push(format!("{error:?}")),
    }
    let success = report.success;
    listener.handle_event(Event::FinishedVerification { report });
    success
}

struct ForwardEvents<'a, T>(&'a T);
impl<T: EventListener> EventListener for ForwardEvents<'_, T> {
    fn handle_event(&self, event: Event) {
        self.0.handle_event(event);
    }
}

fn clear_previous_run(output: &Path) -> miette::Result<()> {
    match fs::symlink_metadata(output) {
        Ok(metadata) if !metadata.file_type().is_dir() => {
            return Err(miette::miette!(
                "{} must be a real directory, not a symlink or file",
                output.display()
            ));
        }
        Ok(_) => (),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(error) => return Err(error).into_diagnostic(),
    }
    // Remove evidence first, before configuration, compilation or invocation can fail.
    // unlink does not follow an existing output-file symlink.
    for name in [
        "verified-assurance.json",
        "verification-artifact.zip",
        "run-report.json",
        "verification.log",
        "Check.lean",
    ] {
        match fs::remove_file(output.join(name)) {
            Ok(()) => (),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => (),
            Err(error) => {
                return Err(error)
                    .into_diagnostic()
                    .wrap_err_with(|| format!("cannot remove stale {name}"));
            }
        }
    }
    Ok(())
}

fn solver_identity(workspace: &ConsumerWorkspace, timeout: Duration) -> miette::Result<String> {
    let temporary = TemporaryDirectory::new()?;
    let log = temporary.0.join("solver-path.log");
    let mut command = workspace.lake_command();
    command.args(["env", "which", "z3"]);
    let status = run_process(&mut command, &log, timeout)?;
    if !status.success() {
        return Err(miette::miette!(
            "cannot resolve Z3 in the consumer Lake environment: {}",
            fs::read_to_string(&log).into_diagnostic()?
        ));
    }
    let text = fs::read_to_string(&log).into_diagnostic()?;
    let path = Path::new(text.trim());
    if !path.is_absolute() {
        return Err(miette::miette!(
            "consumer did not identify an absolute Z3 executable path"
        ));
    }
    let path = path.canonicalize().into_diagnostic()?;
    let hash = ual::sha256_digest(&fs::read(&path).into_diagnostic()?);
    Ok(format!("{} (sha256:{})", path.display(), hash.digest))
}

/// Both streams share an open file description: retain the complete diagnostic
/// order without pipe deadlocks or unbounded in-memory capture while Lean runs.
fn run_process(command: &mut Command, log: &Path, timeout: Duration) -> miette::Result<ExitStatus> {
    let file = fs::File::create_new(log).into_diagnostic()?;
    command
        .stdin(Stdio::null())
        .stdout(Stdio::from(file.try_clone().into_diagnostic()?))
        .stderr(Stdio::from(file));
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        command.process_group(0);
    }
    let mut child = command
        .spawn()
        .into_diagnostic()
        .wrap_err("cannot launch consumer command; provision Lake/Lean and Z3 on PATH")?;
    let start = Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(status)) => return Ok(status),
            Ok(None) if start.elapsed() < timeout => thread::sleep(
                Duration::from_millis(20).min(timeout.saturating_sub(start.elapsed())),
            ),
            Ok(None) => {
                terminate_tree(&mut child);
                return Err(miette::miette!(
                    "consumer timed out after {} seconds; process tree terminated; see {}",
                    timeout.as_secs(),
                    log.display()
                ));
            }
            Err(error) => {
                terminate_tree(&mut child);
                return Err(error).into_diagnostic();
            }
        }
    }
}

fn terminate_tree(child: &mut Child) {
    #[cfg(unix)]
    {
        // POSIX kill(2), directly declared to avoid adding a dependency. The child
        // is its own process-group leader; its Lean/Z3 descendants inherit it.
        unsafe extern "C" {
            fn kill(pid: i32, signal: i32) -> i32;
        }
        // SAFETY: no pointers; a negative child PID names only the dedicated group.
        unsafe {
            kill(-(child.id() as i32), 9);
        }
    }
    #[cfg(not(unix))]
    {
        let _ = child.kill();
    }
    let _ = child.wait();
}

/// The vendored Assurance.lean at SCHEMA_PIN provides no JSON report option.
/// Keep its exact text protocol isolated here rather than probing speculative flags.
struct PinnedVerdictReader;
impl PinnedVerdictReader {
    fn read(log: &str, title: &str, properties: &mut [VerificationProperty]) -> Vec<String> {
        // Capability selection is tied to the vendored source revision, never a
        // failed invocation with a speculative `(report := ...)` option. A pin
        // update must deliberately install its matching verdict reader/schema.
        let revision = ual::SCHEMA_PIN
            .lines()
            .find_map(|line| line.strip_prefix("commit: "));
        if revision != Some("1c23de8f35fdc5decf6f126c7b56ed6649e4a57f") {
            return vec![
                "no verdict reader registered for the pinned consumer revision".to_owned(),
            ];
        }
        let location =
            Regex::new(r"^.*:[0-9]+:[0-9]+: (?:info|warning|error): (.*)$").expect("static regex");
        let property_line = Regex::new(r"^Property '([^']+)': (.*)$").expect("static regex");
        let summary_line = Regex::new(r"^Assurance '(.*)': ([0-9]+) checked, ([0-9]+) not checked, ([0-9]+) failed/inconclusive$").expect("static regex");
        let unsupported =
            Regex::new(r"^not checked \(unsupported language '[^']+'\)$").expect("static regex");
        let mut seen = BTreeSet::new();
        let mut errors = Vec::new();
        let mut summary = None;
        let mut summary_count = 0;
        for raw in log.lines() {
            let line = location
                .captures(raw)
                .and_then(|capture| capture.get(1))
                .map_or(raw, |capture| capture.as_str());
            if let Some(capture) = property_line.captures(line) {
                let id = &capture[1];
                let text = &capture[2];
                let Some(property) = properties.iter_mut().find(|property| property.id == id)
                else {
                    errors.push(format!("consumer reported unknown property '{id}'"));
                    continue;
                };
                if text == "checking fresh proposition"
                    || text
                        == "historical artifact not checked (remote URI); fresh checking is independent"
                {
                    continue;
                }
                let verdict = match text {
                    "verified (Blaster SMT; no reconstructed Lean proof)" => {
                        VerificationVerdict::Verified
                    }
                    "falsified" => VerificationVerdict::Falsified,
                    "inconclusive" => VerificationVerdict::Inconclusive,
                    "not checked (natural-language only)"
                    | "not checked (formal source is only available by URI)" => {
                        VerificationVerdict::NotChecked
                    }
                    text if unsupported.is_match(text) => VerificationVerdict::NotChecked,
                    _ => {
                        errors.push(format!("malformed consumer verdict for '{id}': {text}"));
                        continue;
                    }
                };
                if !seen.insert(id.to_owned()) {
                    errors.push(format!("duplicate consumer verdict for '{id}'"));
                    continue;
                }
                if matches!(verdict, VerificationVerdict::Falsified) {
                    // Blaster's counterexample precedes the final property line and
                    // is not a separately delimited protocol. Preserve it verbatim.
                    property.counterexample = Some(log.to_owned());
                }
                property.verdict = verdict;
            } else if let Some(capture) = summary_line.captures(line) {
                summary_count += 1;
                if &capture[1] != title {
                    errors.push(format!(
                        "consumer summary title mismatch: expected '{title}', found '{}'",
                        &capture[1]
                    ));
                }
                summary = capture[2]
                    .parse::<usize>()
                    .ok()
                    .zip(capture[3].parse::<usize>().ok())
                    .zip(capture[4].parse::<usize>().ok())
                    .map(|((checked, skipped), failed)| (checked, skipped, failed));
                if summary.is_none() {
                    errors.push("consumer summary counts overflow".to_owned());
                }
            } else if line.starts_with("Property ") || line.starts_with("Assurance ") {
                errors.push(format!("malformed consumer verdict or summary: {line}"));
            }
        }
        for property in properties.iter() {
            if !seen.contains(&property.id) {
                errors.push(format!("missing consumer verdict for '{}'", property.id));
            }
        }
        if summary_count != 1 {
            errors.push(format!(
                "expected one consumer summary, found {summary_count}"
            ));
        }
        let checked = properties
            .iter()
            .filter(|property| {
                matches!(
                    property.verdict,
                    VerificationVerdict::Verified
                        | VerificationVerdict::Falsified
                        | VerificationVerdict::Inconclusive
                )
            })
            .count();
        let skipped = properties
            .iter()
            .filter(|property| matches!(property.verdict, VerificationVerdict::NotChecked))
            .count();
        let failed = properties
            .iter()
            .filter(|property| {
                matches!(
                    property.verdict,
                    VerificationVerdict::Falsified | VerificationVerdict::Inconclusive
                )
            })
            .count();
        if summary.is_some_and(|counts| counts != (checked, skipped, failed)) {
            errors.push("consumer summary counts do not match per-property verdicts".to_owned());
        }
        if properties.is_empty() {
            errors.push("no assurance properties to verify".to_owned());
        }
        errors
    }
}

fn publish_evidence(
    output: &Path,
    environment: &Path,
    assurance_bytes: &[u8],
    assurance: &mut ual::AssuranceDocument,
    report: &VerificationReport,
) -> miette::Result<()> {
    let blueprint_bytes = fs::read(output.join("plutus.json")).into_diagnostic()?;
    let blueprint: ual::CompiledInterfaceBlueprint =
        serde_json::from_slice(&blueprint_bytes).into_diagnostic()?;
    let environment_bytes = fs::read(environment).into_diagnostic()?;
    let mut contexts = BTreeMap::new();
    for (id, artifact) in assurance.checking_contexts.iter().flatten() {
        contexts.insert(
            id.clone(),
            fs::read(output.join(&artifact.uri)).into_diagnostic()?,
        );
    }
    let context_refs = contexts
        .iter()
        .map(|(id, bytes)| (id.clone(), bytes.as_slice()))
        .collect();
    ual::validate_bindings(assurance, &blueprint_bytes, &context_refs).into_diagnostic()?;
    let mut applied_artifacts = BTreeMap::new();
    for bytes in contexts.values() {
        let context: ual::CheckingContext = serde_json::from_slice(bytes).into_diagnostic()?;
        ual::validate_environment(&context, &environment_bytes).into_diagnostic()?;
        for target in context.targets {
            if let ual::ContextTarget::Validator(ual::ValidatorTarget {
                parameters:
                    ual::ParameterBinding::Applied {
                        values,
                        applied_script,
                        ..
                    },
                ..
            }) = target
            {
                for artifact in values
                    .into_iter()
                    .map(|value| value.term)
                    .chain(std::iter::once(applied_script))
                {
                    let bytes = fs::read(output.join(&artifact.uri)).into_diagnostic()?;
                    artifact.hash.verify(&bytes).into_diagnostic()?;
                    applied_artifacts.insert(artifact.uri, bytes);
                }
            }
        }
    }
    let date = civil_date(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .into_diagnostic()?
            .as_secs()
            / 86_400,
    )?;
    // The archive is the evidence artifact, not a reconstructed proof. It keeps
    // the exact inputs, complete log, driver and trust disclosure together.
    let mut successful_report = report.clone();
    successful_report.success = true;
    let report_bytes = ual::canonical_bytes(&successful_report).into_diagnostic()?;
    let mut archive = zip::ZipWriter::new(Cursor::new(Vec::new()));
    let options = zip::write::FileOptions::default();
    let driver_bytes = fs::read(output.join("Check.lean")).into_diagnostic()?;
    let log_bytes = fs::read(output.join("verification.log")).into_diagnostic()?;
    for (name, bytes) in [
        ("assurance.json", assurance_bytes),
        ("plutus.json", blueprint_bytes.as_slice()),
        ("environment.json", environment_bytes.as_slice()),
        ("Check.lean", driver_bytes.as_slice()),
        ("verification.log", log_bytes.as_slice()),
        ("run-report.json", report_bytes.as_slice()),
    ] {
        archive.start_file(name, options).into_diagnostic()?;
        archive.write_all(bytes).into_diagnostic()?;
    }
    for (id, bytes) in &contexts {
        let artifact = &assurance
            .checking_contexts
            .as_ref()
            .expect("contexts loaded")[id];
        archive
            .start_file(&artifact.uri, options)
            .into_diagnostic()?;
        archive.write_all(bytes).into_diagnostic()?;
    }
    for (uri, bytes) in &applied_artifacts {
        archive.start_file(uri, options).into_diagnostic()?;
        archive.write_all(bytes).into_diagnostic()?;
    }
    let artifact_bytes = archive.finish().into_diagnostic()?.into_inner();
    let artifact = ual::Artifact {
        uri: "verification-artifact.zip".to_owned(),
        hash: ual::sha256_digest(&artifact_bytes),
        media_type: Some("application/zip".to_owned()),
        extensions: BTreeMap::new(),
    };
    assurance.tools.get_or_insert_with(BTreeMap::new).insert(
        "blaster".to_owned(),
        ual::RegistryEntry {
            name: "Blaster".to_owned(),
            version: report.trust.revisions["Lean-blaster"].clone(),
            uri: Some("https://github.com/input-output-hk/Lean-blaster".to_owned()),
            description: Some(METHOD.to_owned()),
            extensions: BTreeMap::new(),
        },
    );
    for property in &mut assurance.properties {
        let context_hash = property
            .checking_context
            .as_ref()
            .and_then(|id| assurance.checking_contexts.as_ref()?.get(id))
            .map(|reference| reference.hash.clone())
            .ok_or_else(|| {
                miette::miette!("property '{}' has no checking context binding", property.id)
            })?;
        let mut evidence = ual::EvidenceRecord::smt_check(
            "Local Blaster runner",
            ual::EvidenceOutcome::Verified,
            &date,
            "blaster",
            artifact.clone(),
            context_hash,
        );
        evidence.notes = Some(format!(
            "Fresh SMT verification; trusts the translation and solver. No reconstructed Lean proof. {BUDGET}"
        ));
        for id in property.scope.validators.iter().flatten() {
            let validator = blueprint
                .validators
                .iter()
                .find(|validator| validator.id == *id)
                .ok_or_else(|| miette::miette!("unknown scoped validator '{id}'"))?;
            let hash = validator
                .hash
                .clone()
                .ok_or_else(|| miette::miette!("validator '{id}' has no script hash"))?;
            if evidence
                .script_hash
                .as_ref()
                .is_some_and(|previous| previous != &hash)
            {
                return Err(miette::miette!(
                    "pinned evidence schema cannot bind multiple distinct validator hashes for property '{}'",
                    property.id
                ));
            }
            evidence.script_hash = Some(hash);
        }
        let mut function_hashes = BTreeMap::new();
        for id in property.scope.functions.iter().flatten() {
            let function = assurance
                .functions
                .as_ref()
                .and_then(|functions| functions.get(id))
                .ok_or_else(|| miette::miette!("unknown scoped function '{id}'"))?;
            function_hashes.insert(id.clone(), function.hash.clone());
        }
        evidence.function_hashes = (!function_hashes.is_empty()).then_some(function_hashes);
        property.evidence = Some(vec![evidence]);
    }
    ual::validate_bindings(assurance, &blueprint_bytes, &context_refs).into_diagnostic()?;
    let verified =
        ual::validated_bytes(ual::DocumentKind::Assurance, assurance).into_diagnostic()?;
    replace_file(&output.join("verification-artifact.zip"), &artifact_bytes)?;
    replace_file(&output.join("run-report.json"), &report_bytes)?;
    // Publish the success marker last; no earlier failure can leave fresh evidence.
    replace_file(&output.join("verified-assurance.json"), &verified)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(log: &str) -> (Vec<VerificationProperty>, Vec<String>) {
        let mut properties = vec![VerificationProperty {
            id: "p".to_owned(),
            verdict: VerificationVerdict::Missing,
            counterexample: None,
        }];
        let errors = PinnedVerdictReader::read(log, "test", &mut properties);
        (properties, errors)
    }

    const VERIFIED: &str = "Property 'p': verified (Blaster SMT; no reconstructed Lean proof)\n";
    const SUMMARY: &str = "Assurance 'test': 1 checked, 0 not checked, 0 failed/inconclusive\n";

    #[test]
    fn accepts_exact_verdict_and_summary_with_lean_locations() {
        let (properties, errors) =
            parse(&format!("/tmp/Check.lean:3:0: info: {VERIFIED}{SUMMARY}"));
        assert!(errors.is_empty(), "{errors:?}");
        assert!(matches!(
            properties[0].verdict,
            VerificationVerdict::Verified
        ));
    }

    #[test]
    fn rejects_missing_duplicate_unknown_malformed_and_mismatched_results() {
        for log in [
            SUMMARY.to_owned(),
            format!("{VERIFIED}{VERIFIED}{SUMMARY}"),
            format!(
                "Property 'other': verified (Blaster SMT; no reconstructed Lean proof)\n{SUMMARY}"
            ),
            format!("Property 'p': verified\n{SUMMARY}"),
            format!(
                "{VERIFIED}Assurance 'wrong': 1 checked, 0 not checked, 0 failed/inconclusive\n"
            ),
            format!(
                "{VERIFIED}Assurance 'test': 2 checked, 0 not checked, 0 failed/inconclusive\n"
            ),
            format!("{VERIFIED}{SUMMARY}{SUMMARY}"),
            VERIFIED.to_owned(),
        ] {
            assert!(!parse(&log).1.is_empty(), "accepted {log}");
        }
    }

    #[test]
    fn preserves_counterexample_and_counts_failed_checks() {
        let log = "counterexample: n = 0\nProperty 'p': falsified\nAssurance 'test': 1 checked, 0 not checked, 1 failed/inconclusive\n";
        let (properties, errors) = parse(log);
        assert!(errors.is_empty(), "{errors:?}");
        assert!(matches!(
            properties[0].verdict,
            VerificationVerdict::Falsified
        ));
        assert_eq!(properties[0].counterexample.as_deref(), Some(log));
    }

    #[test]
    fn inconclusive_diagnostics_are_not_counterexamples() {
        let (properties, errors) = parse(
            "solver timeout\nProperty 'p': inconclusive\nAssurance 'test': 1 checked, 0 not checked, 1 failed/inconclusive\n",
        );
        assert!(errors.is_empty(), "{errors:?}");
        assert!(matches!(
            properties[0].verdict,
            VerificationVerdict::Inconclusive
        ));
        assert!(properties[0].counterexample.is_none());
    }

    #[test]
    fn distinguishes_skips_and_rejects_vacuous_success() {
        let (properties, errors) = parse(
            "Property 'p': not checked (natural-language only)\nAssurance 'test': 0 checked, 1 not checked, 0 failed/inconclusive\n",
        );
        assert!(errors.is_empty());
        assert!(matches!(
            properties[0].verdict,
            VerificationVerdict::NotChecked
        ));
        assert!(
            !PinnedVerdictReader::read(
                "Assurance 'test': 0 checked, 0 not checked, 0 failed/inconclusive\n",
                "test",
                &mut []
            )
            .is_empty()
        );
    }
}
