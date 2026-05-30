use super::output;
use aiken_project::verify::{self, DEFAULT_BLASTER_REV, DEFAULT_PLUTUS_CORE_REV};
use owo_colors::{OwoColorize, Stream::Stderr};
use std::process;

/// Check the verification toolchain, dependencies, and local configuration.
#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[command(
    verbatim_doc_comment,
    about = color_print::cstr!(r#"
Check the verification toolchain, dependencies, and local configuration.
"#),
    after_long_help = color_print::cstr!(r#"<bold><underline>Examples:</underline></bold>

    <bold>aiken verify doctor</bold>
        Check the default verification toolchain status

    <bold>aiken verify doctor --json</bold>
        Emit the report as JSON

You are seeing the extended help. Use `-h` instead of `--help` for a more compact view.
"#),
)]
pub struct Args {
    /// Output results as JSON
    #[clap(long)]
    pub(super) json: bool,

    /// Git revision (commit, tag, or branch) for the Blaster dependency to report
    #[clap(long, default_value = DEFAULT_BLASTER_REV)]
    pub(super) blaster_rev: String,

    /// Git revision (commit, tag, or branch) for the PlutusCore dependency to report
    #[clap(long, default_value = DEFAULT_PLUTUS_CORE_REV)]
    pub(super) plutus_core_rev: String,
}

pub fn exec(
    Args {
        json,
        blaster_rev,
        plutus_core_rev,
    }: Args,
) -> miette::Result<()> {
    let result = run_doctor_command_with(json, blaster_rev, plutus_core_rev, verify::run_doctor)?;

    print!("{}", result.output);

    if result.exit_code != 0 {
        process::exit(result.exit_code);
    }

    Ok(())
}

pub(super) fn doctor_ready(report: &verify::DoctorReport) -> bool {
    report.all_ok
        && report.certification.solver_validated.available
        && report.certification.lean_certified.available
        && report.certification.proof_reconstruction.available
        && report.certification.strict_cert_profile.available
}

pub(super) fn doctor_uses_local_plutus_core(report: &verify::DoctorReport) -> bool {
    !report.plutus_core.path.starts_with("git:")
}

pub(super) fn normalize_doctor_report_provenance(report: &mut verify::DoctorReport) {
    if doctor_uses_local_plutus_core(report) {
        report.plutus_core_rev = "<local checkout; git revision not inspected>".to_string();
        if report.backends.plutus_core_blaster.note.is_some() {
            report.backends.plutus_core_blaster.note = Some(format!(
                "PlutusCoreBlaster dependency is configured via local checkout {}.",
                report.plutus_core.path
            ));
        }
    }
    report.all_ok = doctor_ready(report);
}

pub(super) fn format_doctor_output(
    report: &verify::DoctorReport,
    json: bool,
) -> miette::Result<String> {
    if json {
        let output = serde_json::to_string_pretty(report)
            .map_err(|e| miette::miette!("Failed to serialize doctor report as JSON: {e}"))?;
        return Ok(format!("{output}\n"));
    }

    let mut lines = vec![
        "Verify Doctor Report".to_string(),
        "====================".to_string(),
        String::new(),
    ];

    for tool in &report.tools {
        let status = if !tool.found {
            "NOT FOUND"
                .if_supports_color(Stderr, |s| s.red())
                .to_string()
        } else if !tool.meets_minimum {
            format!(
                "{} (minimum: {})",
                "VERSION TOO LOW".if_supports_color(Stderr, |s| s.red()),
                tool.minimum_version,
            )
        } else {
            "OK".if_supports_color(Stderr, |s| s.green()).to_string()
        };

        let version_str = tool.version.as_deref().unwrap_or("unknown");
        lines.push(format!("  {}: {} ({})", tool.tool, status, version_str));

        if let Some(err) = &tool.error {
            lines.push(format!(
                "    {}",
                err.if_supports_color(Stderr, |s| s.yellow())
            ));
        }
    }

    lines.push(String::new());

    let pc_status = if report.plutus_core.found && report.plutus_core.has_lakefile {
        "OK".if_supports_color(Stderr, |s| s.green()).to_string()
    } else if report.plutus_core.found {
        "INCOMPLETE (missing lakefile.lean)"
            .if_supports_color(Stderr, |s| s.red())
            .to_string()
    } else {
        "NOT FOUND"
            .if_supports_color(Stderr, |s| s.red())
            .to_string()
    };
    lines.push(format!(
        "  PlutusCore: {} ({})",
        pc_status, report.plutus_core.path
    ));
    if let Some(err) = &report.plutus_core.error {
        lines.push(format!(
            "    {}",
            err.if_supports_color(Stderr, |s| s.yellow())
        ));
    }

    lines.push(String::new());
    lines.push(format!("  Blaster revision: {}", report.blaster_rev));
    if doctor_uses_local_plutus_core(report) {
        lines.push(format!(
            "  PlutusCore source: local checkout at {}",
            report.plutus_core.path
        ));
        lines.push(
            "  PlutusCore revision: <local checkout; git revision not inspected>".to_string(),
        );
    } else {
        lines.push(format!("  PlutusCore revision: {}", report.plutus_core_rev));
    }
    lines.push(String::new());
    lines.push("Backends:".to_string());
    lines.push(output::render_availability_line(
        "Blaster",
        &report.backends.blaster,
    ));
    lines.push(output::render_availability_line(
        "Solver backend (Z3)",
        &report.backends.solver_backend,
    ));
    lines.push(output::render_availability_line(
        "PlutusCoreBlaster",
        &report.backends.plutus_core_blaster,
    ));
    lines.push(output::render_availability_line(
        "CardanoLedgerApiBlaster",
        &report.backends.cardano_ledger_api_blaster,
    ));
    lines.push(String::new());
    lines.push("Certification:".to_string());
    lines.push(output::render_availability_line(
        "SolverValidated support",
        &report.certification.solver_validated,
    ));
    lines.push(output::render_availability_line(
        "LeanCertified support",
        &report.certification.lean_certified,
    ));
    lines.push(output::render_availability_line(
        "Proof reconstruction",
        &report.certification.proof_reconstruction,
    ));
    lines.push(output::render_availability_line(
        "strict-cert profile",
        &report.certification.strict_cert_profile,
    ));
    lines.push(String::new());

    if doctor_ready(report) {
        lines.push(format!(
            "{}",
            "All checks passed."
                .if_supports_color(Stderr, |s| s.green())
                .if_supports_color(Stderr, |s| s.bold())
        ));
    } else {
        lines.push(format!(
            "{}",
            "Some checks failed. See above for details."
                .if_supports_color(Stderr, |s| s.red())
                .if_supports_color(Stderr, |s| s.bold())
        ));
    }

    Ok(format!("{}\n", lines.join("\n")))
}

pub(super) fn doctor_exit_code(report: &verify::DoctorReport) -> i32 {
    if doctor_ready(report) { 0 } else { 1 }
}

pub(super) fn run_doctor_command_with<F>(
    json: bool,
    blaster_rev: String,
    plutus_core_rev: String,
    run_doctor: F,
) -> miette::Result<output::CommandBranchResult>
where
    F: FnOnce(&str, &str) -> verify::DoctorReport,
{
    let mut report = run_doctor(&blaster_rev, &plutus_core_rev);
    normalize_doctor_report_provenance(&mut report);
    let output = format_doctor_output(&report, json)?;
    let exit_code = doctor_exit_code(&report);

    Ok(output::CommandBranchResult { output, exit_code })
}
