use super::output;
use aiken_project::verify;

/// Show supported verification capabilities and skip catalogue codes.
#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[command(
    verbatim_doc_comment,
    about = color_print::cstr!(r#"
Show supported verification capabilities and skip catalogue codes.
"#),
    after_long_help = color_print::cstr!(r#"<bold><underline>Examples:</underline></bold>

    <bold>aiken verify capabilities</bold>
        Print the supported verification surface in text form

    <bold>aiken verify capabilities --json</bold>
        Emit the supported verification surface as JSON

You are seeing the extended help. Use `-h` instead of `--help` for a more compact view.
"#),
)]
pub struct Args {
    /// Output as JSON
    #[clap(long)]
    pub(super) json: bool,
}

pub fn exec(args: Args) -> miette::Result<()> {
    let result = run_capabilities_command(args)?;
    print!("{}", result.output);
    Ok(())
}

pub(super) fn format_capabilities_output(
    caps: &verify::VerificationCapabilities,
    json: bool,
) -> miette::Result<String> {
    if json {
        let output = serde_json::to_string_pretty(caps)
            .map_err(|e| miette::miette!("Failed to serialize capabilities as JSON: {e}"))?;
        return Ok(format!("{output}\n"));
    }

    let mut lines = vec![
        "Verification Capabilities".to_string(),
        "=========================".to_string(),
        String::new(),
        "Supported test kinds:".to_string(),
    ];

    for k in &caps.supported_test_kinds {
        lines.push(format!("  - {k}"));
    }

    lines.push(String::new());
    lines.push("Unsupported test kinds:".to_string());
    for n in &caps.unsupported_test_kinds {
        lines.push(format!("  - {} [{}]: {}", n.kind, n.status, n.reason));
    }

    lines.push(String::new());
    lines.push("Target modes:".to_string());
    for m in &caps.target_modes {
        lines.push(format!("  - {m}"));
    }

    lines.push(String::new());
    lines.push("Supported fuzzer output types:".to_string());
    for t in &caps.supported_fuzzer_types {
        lines.push(format!("  - {t}"));
    }

    lines.push(String::new());
    lines.push("Unsupported fuzzer output types:".to_string());
    for t in &caps.unsupported_fuzzer_types {
        lines.push(format!("  - {t}"));
    }

    lines.push(String::new());
    lines.push("Existential modes:".to_string());
    for m in &caps.existential_modes {
        lines.push(format!("  - {m}"));
    }

    lines.push(String::new());
    lines.push("Solver profiles:".to_string());
    for profile in &caps.solver_profiles {
        lines.push(format!("  - {profile}"));
    }

    lines.push(String::new());
    lines.push("Trust profiles:".to_string());
    for profile in &caps.trust_profiles {
        lines.push(format!("  - {profile}"));
    }

    lines.push(String::new());
    lines.push("Backends:".to_string());
    lines.push(output::render_availability_line(
        "Blaster",
        &caps.backends.blaster,
    ));
    lines.push(output::render_availability_line(
        "Solver backend (Z3)",
        &caps.backends.solver_backend,
    ));
    lines.push(output::render_availability_line(
        "PlutusCoreBlaster",
        &caps.backends.plutus_core_blaster,
    ));
    lines.push(output::render_availability_line(
        "CardanoLedgerApiBlaster",
        &caps.backends.cardano_ledger_api_blaster,
    ));

    lines.push(String::new());
    lines.push("Certification:".to_string());
    lines.push(output::render_availability_line(
        "SolverValidated support",
        &caps.certification.solver_validated,
    ));
    lines.push(output::render_availability_line(
        "LeanCertified support",
        &caps.certification.lean_certified,
    ));
    lines.push(output::render_availability_line(
        "Proof reconstruction",
        &caps.certification.proof_reconstruction,
    ));
    lines.push(output::render_availability_line(
        "strict-cert profile",
        &caps.certification.strict_cert_profile,
    ));

    lines.push(String::new());
    lines.push(format!("Max test arity: {}", caps.max_test_arity));

    // Catalogue table — drives `--skip-unsupported=<CODE>` filtering. Sourced
    // from the currently surfaced `aiken-project` catalogue projection so the
    // CLI only advertises codes the verifier can actually emit today.
    if !caps.unsupported.is_empty() {
        lines.push(String::new());
        lines.push("Error catalogue:".to_string());
        let code_width = caps
            .unsupported
            .iter()
            .map(|c| c.code.len())
            .max()
            .unwrap_or(4);
        let feature_width = caps
            .unsupported
            .iter()
            .map(|c| c.feature.len())
            .max()
            .unwrap_or(7);
        // Header row plus a separator so users can scan the table.
        lines.push(format!(
            "  {:code_width$}  {:feature_width$}  {}",
            "CODE", "FEATURE", "SKIPPABLE"
        ));
        lines.push(format!(
            "  {}  {}  {}",
            "-".repeat(code_width),
            "-".repeat(feature_width),
            "---------"
        ));
        for entry in &caps.unsupported {
            lines.push(format!(
                "  {:code_width$}  {:feature_width$}  {}",
                entry.code,
                entry.feature,
                if entry.skippable { "yes" } else { "no" },
            ));
        }
    }

    Ok(format!("{}\n", lines.join("\n")))
}

pub(super) fn run_capabilities_command(
    Args { json }: Args,
) -> miette::Result<output::CommandBranchResult> {
    let caps = verify::capabilities();
    let output = format_capabilities_output(&caps, json)?;

    Ok(output::CommandBranchResult {
        output,
        exit_code: 0,
    })
}
