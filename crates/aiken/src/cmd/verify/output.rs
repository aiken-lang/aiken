use aiken_project::{telemetry::EventTarget, verify};
use owo_colors::{OwoColorize, Stream::Stderr};
use std::path::Path;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum OutputMode {
    Text,
    Json,
    Silent,
}

impl OutputMode {
    pub(super) fn from_flags(json: bool, silent: bool) -> Self {
        if json {
            Self::Json
        } else if silent {
            Self::Silent
        } else {
            Self::Text
        }
    }

    pub(super) fn emits_json(self) -> bool {
        matches!(self, Self::Json)
    }

    pub(super) fn shows_advisories(self) -> bool {
        matches!(self, Self::Text)
    }

    pub(super) fn project_event_target(self) -> EventTarget {
        match self {
            Self::Text => EventTarget::default(),
            Self::Json | Self::Silent => EventTarget::Silent,
        }
    }

    pub(super) fn reports_project_diagnostics(self) -> bool {
        self.shows_advisories()
    }
}

fn render_manifest_return_mode(
    mode: Option<&aiken_project::export::TestReturnMode>,
) -> &'static str {
    match mode {
        Some(aiken_project::export::TestReturnMode::Bool) => "bool",
        Some(aiken_project::export::TestReturnMode::Void) => "void",
        Some(_) => "unknown",
        None => "legacy-unknown",
    }
}

fn render_manifest_test_mode(mode: Option<&verify::ManifestTestMode>) -> &'static str {
    match mode {
        Some(verify::ManifestTestMode::Normal) => "normal",
        Some(verify::ManifestTestMode::Fail) => "fail",
        Some(verify::ManifestTestMode::FailOnce) => "fail_once",
        Some(_) => "unknown",
        None => "legacy-unknown",
    }
}

fn render_manifest_on_test_failure(
    on_test_failure: Option<&verify::ManifestOnTestFailure>,
) -> &'static str {
    match on_test_failure {
        Some(verify::ManifestOnTestFailure::FailImmediately) => "fail_immediately",
        Some(verify::ManifestOnTestFailure::SucceedImmediately) => "succeed_immediately",
        Some(verify::ManifestOnTestFailure::SucceedEventually) => "succeed_eventually",
        Some(_) => "unknown",
        None => "legacy-unknown",
    }
}

fn render_manifest_plutus_version(
    plutus_version: Option<aiken_project::config::PlutusVersion>,
) -> &'static str {
    match plutus_version {
        Some(aiken_project::config::PlutusVersion::V1) => "v1",
        Some(aiken_project::config::PlutusVersion::V2) => "v2",
        Some(aiken_project::config::PlutusVersion::V3) => "v3",
        None => "legacy-unknown",
    }
}

fn render_manifest_decode_policy(
    decode_policy: Option<&verify::ManifestDecodePolicy>,
) -> &'static str {
    match decode_policy {
        Some(verify::ManifestDecodePolicy::DecodeErrorIsTestFailure) => {
            "decode_error_is_test_failure"
        }
        Some(verify::ManifestDecodePolicy::DecodeErrorIsExportError) => {
            "decode_error_is_export_error"
        }
        Some(_) => "unknown",
        None => "legacy-unknown",
    }
}

fn render_sampler_run_kind(kind: verify::SamplerRunKind) -> &'static str {
    match kind {
        verify::SamplerRunKind::SeededGeneration => "seeded_generation",
        verify::SamplerRunKind::ReplayOnly => "replay_only",
        verify::SamplerRunKind::WitnessReplay => "witness_replay",
        _ => "unknown",
    }
}

fn render_sampler_prng_mode(mode: verify::SamplerPrngMode) -> &'static str {
    match mode {
        verify::SamplerPrngMode::Seeded => "seeded",
        verify::SamplerPrngMode::Replayed => "replayed",
        _ => "unknown",
    }
}

fn render_manifest_sampler(sampler: Option<&verify::SamplerMetadata>) -> Option<String> {
    sampler.map(|sampler| {
        format!(
            "sampler={}/{}",
            render_sampler_run_kind(sampler.run_kind),
            render_sampler_prng_mode(sampler.prng_mode)
        )
    })
}

fn render_manifest_domain(entry: &verify::ManifestEntry) -> Option<String> {
    entry
        .domain
        .as_ref()
        .map(|domain| format!("domain={}/{}", domain.precision, domain.certificate))
}

pub(super) fn render_manifest_entry_debug(entry: &verify::ManifestEntry) -> String {
    let mut parts = vec![format!(
        "{} -> {} [return_mode={}, test_mode={}, on_test_failure={}]",
        entry.aiken_module,
        entry.lean_module,
        render_manifest_return_mode(entry.return_mode.as_ref()),
        render_manifest_test_mode(entry.test_mode.as_ref()),
        render_manifest_on_test_failure(entry.on_test_failure.as_ref()),
    )];
    if let Some(domain) = render_manifest_domain(entry) {
        parts.push(domain);
    }
    if let Some(sampler) = render_manifest_sampler(entry.sampler.as_ref()) {
        parts.push(sampler);
    }
    parts.join(" ")
}

pub(super) fn render_manifest_debug_header(manifest: &verify::GeneratedManifest) -> String {
    format!(
        "Manifest schema v{}; plutus_version={}; cek_fuel={}; decode_policy={}",
        manifest.schema_version,
        render_manifest_plutus_version(manifest.execution.plutus_version),
        manifest
            .execution
            .cek_budget
            .fuel
            .map(|fuel| fuel.to_string())
            .unwrap_or_else(|| "legacy-unknown".to_string()),
        render_manifest_decode_policy(manifest.execution.decode_policy.as_ref()),
    )
}

pub(super) fn failure_artifact_advice(
    resolved_out_dir: &Path,
    artifacts_retained: bool,
) -> Vec<String> {
    if artifacts_retained {
        return vec![
            format!("Logs available at {}/logs/", resolved_out_dir.display()),
            format!(
                "To reproduce: cd {} && lake build",
                resolved_out_dir.display()
            ),
        ];
    }

    vec![
        "Artifacts were cleaned up after this run. Re-run with --artifacts always to keep the generated workspace and logs.".to_string(),
    ]
}

pub(super) struct CommandBranchResult {
    pub(super) output: String,
    pub(super) exit_code: i32,
}

pub(super) fn render_availability_line(
    label: &str,
    availability: &verify::CapabilityAvailability,
) -> String {
    let status = if availability.available {
        "available"
            .if_supports_color(Stderr, |s| s.green())
            .to_string()
    } else {
        "unavailable"
            .if_supports_color(Stderr, |s| s.red())
            .to_string()
    };
    let mut line = format!("  {label}: {status}");
    if let Some(note) = &availability.note {
        line.push_str(&format!(" — {note}"));
    }
    line
}
