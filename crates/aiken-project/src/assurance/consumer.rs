//! Invocation of the externally provisioned, revision-pinned Lean consumer.

use crate::config::AssuranceConfig;
use aiken_ual as ual;
use miette::{IntoDiagnostic, WrapErr};
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
    process::Command,
    sync::atomic::{AtomicU64, Ordering},
};

/// Environment capture and check drivers must load exactly the same modules:
/// the consumer hashes their loaded `.olean` files into the manifest.
pub const DRIVER_IMPORTS: &str =
    "import PlutusCore.UPLC.BlueprintEncoding.Assurance\nimport CardanoLedgerApi.V3\n";

const SETUP: &str = "Set [assurance] workspace = \"/path/to/ual-workspace/CardanoLedgerApiBlaster\" in aiken.toml, or AIKEN_ASSURANCE_WORKSPACE. Provision the pinned sibling workspace with scripts/ual/workspace-setup.sh (see scripts/ual/README.md), and put Lean 4.24.0/Lake and Z3 with recfun-finder on PATH. Aiken does not install them.";

/// The ledger checkout whose Lake environment exposes both required imports.
pub struct ConsumerWorkspace {
    directory: PathBuf,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct WorkspaceLock {
    blaster_commit: String,
    repositories: Repositories,
}

#[derive(Deserialize)]
struct Repositories {
    ledger: RevisionPin,
}

#[derive(Deserialize)]
struct RevisionPin {
    commit: String,
}

#[derive(Serialize)]
struct Revision {
    expected: String,
    actual: Option<String>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct EnvironmentRun<'a> {
    format: &'static str,
    workspace: &'a Path,
    imports: &'static str,
    revisions: BTreeMap<&'static str, Revision>,
    warnings: Vec<String>,
    status: &'static str,
    environment_hash: Option<ual::Digest>,
    error: Option<String>,
}

impl ConsumerWorkspace {
    /// Config paths are relative to their manifest; environment paths to the caller.
    /// An explicitly configured empty or missing path never falls back silently.
    pub fn load(project_directory: &Path) -> miette::Result<Self> {
        let config = AssuranceConfig::load(project_directory).map_err(|error| {
            error.report();
            crate::watch::ExitFailure::into_report()
        })?;
        let path = resolve_workspace(
            config.workspace.as_deref(),
            std::env::var_os("AIKEN_ASSURANCE_WORKSPACE"),
            project_directory,
        )?;
        let directory = path.canonicalize().into_diagnostic().wrap_err_with(|| {
            format!(
                "consumer workspace {} is unavailable. {SETUP}",
                path.display()
            )
        })?;
        let root = directory
            .parent()
            .ok_or_else(|| miette::miette!("consumer workspace has no sibling root. {SETUP}"))?;
        for checkout in [
            directory.clone(),
            root.join("PlutusCoreBlaster"),
            root.join("Lean-blaster"),
        ] {
            if !checkout.is_dir() {
                return Err(miette::miette!(
                    "consumer checkout {} is missing. {SETUP}",
                    checkout.display()
                ));
            }
        }
        Ok(Self { directory })
    }

    pub(super) fn lean_command(&self, driver: &Path) -> Command {
        let mut command = self.lake_command();
        command.args(["env", "lean"]).arg(driver);
        command
    }

    pub(super) fn lake_command(&self) -> Command {
        let mut command = Command::new("lake");
        command.current_dir(&self.directory);
        command
    }

    pub(super) fn verification_revisions(
        &self,
    ) -> miette::Result<(BTreeMap<String, String>, Vec<String>)> {
        let run = self.inspect_revisions()?;
        Ok((
            run.revisions
                .into_iter()
                .map(|(name, revision)| {
                    (
                        name.to_owned(),
                        revision.actual.unwrap_or_else(|| "unknown".to_owned()),
                    )
                })
                .collect(),
            run.warnings,
        ))
    }

    /// Capture through a temporary driver, validate before replacing the manifest,
    /// and retain pin warnings separately from the consumer's closed JSON format.
    pub fn capture_environment(&self, output: &Path) -> miette::Result<PathBuf> {
        let output = std::path::absolute(output).into_diagnostic()?;
        let mut record_path = output.as_os_str().to_os_string();
        record_path.push(".run.json");
        let record_path = PathBuf::from(record_path);
        let mut run = self.inspect_revisions()?;
        for warning in &run.warnings {
            eprintln!("WARNING: {warning}");
        }
        write_record(&record_path, &run)?;
        let result = (|| {
            let temporary = TemporaryDirectory::new()?;
            let manifest = temporary.0.join("environment.json");
            let driver = temporary.0.join("Environment.lean");
            let literal = lean_path_literal(&manifest)?;
            fs::write(
                &driver,
                format!("{DRIVER_IMPORTS}#write_checking_environment {literal}\n"),
            )
            .into_diagnostic()?;
            let status = Command::new("lake")
                .args(["env", "lean"])
                .arg(&driver)
                .current_dir(&self.directory)
                .status()
                .into_diagnostic()
                .wrap_err_with(|| format!("cannot run lake env lean. {SETUP}"))?;
            if !status.success() {
                return Err(miette::miette!(
                    "consumer environment capture failed ({status}). {SETUP}"
                ));
            }
            let bytes = fs::read(&manifest)
                .into_diagnostic()
                .wrap_err("consumer did not write an environment manifest")?;
            let value = serde_json::from_slice(&bytes).into_diagnostic()?;
            ual::validate(ual::DocumentKind::Environment, &value)
                .into_diagnostic()
                .wrap_err("consumer emitted an incompatible environment manifest")?;
            fs::write(&output, &bytes)
                .into_diagnostic()
                .wrap_err_with(|| {
                    format!("cannot write environment manifest {}", output.display())
                })?;
            Ok(ual::sha256_digest(&bytes))
        })();
        match &result {
            Ok(hash) => {
                run.status = "succeeded";
                run.environment_hash = Some(hash.clone());
            }
            Err(error) => {
                run.status = "failed";
                run.error = Some(format!("{error:?}"));
            }
        }
        write_record(&record_path, &run)?;
        result?;
        Ok(record_path)
    }

    fn inspect_revisions(&self) -> miette::Result<EnvironmentRun<'_>> {
        let lock: WorkspaceLock =
            serde_json::from_str(ual::ASSURANCE_WORKSPACE).into_diagnostic()?;
        let core_pin = ual::SCHEMA_PIN
            .lines()
            .find_map(|line| line.strip_prefix("commit: "))
            .ok_or_else(|| miette::miette!("vendored consumer commit is missing"))?;
        let root = self.directory.parent().expect("validated sibling root");
        let mut run = EnvironmentRun {
            format: "aiken-assurance-environment-run-v1",
            workspace: &self.directory,
            imports: DRIVER_IMPORTS,
            revisions: BTreeMap::new(),
            warnings: Vec::new(),
            status: "running",
            environment_hash: None,
            error: None,
        };
        for (name, path, expected) in [
            (
                "PlutusCoreBlaster",
                root.join("PlutusCoreBlaster"),
                core_pin.to_owned(),
            ),
            (
                "Lean-blaster",
                root.join("Lean-blaster"),
                lock.blaster_commit,
            ),
            (
                "CardanoLedgerApiBlaster",
                self.directory.clone(),
                lock.repositories.ledger.commit,
            ),
        ] {
            let actual = checkout_revision(&path);
            match &actual {
                Ok(actual) if *actual == expected => (),
                Ok(actual) => run.warnings.push(format!("{name} revision mismatch: expected {expected}, found {actual}; continuing with the installed consumer.")),
                Err(error) => run.warnings.push(format!("cannot determine {name} revision (expected {expected}): {error}; continuing with the installed consumer.")),
            }
            run.revisions.insert(
                name,
                Revision {
                    expected,
                    actual: actual.ok(),
                },
            );
        }
        match fs::read(root.join("PlutusCoreBlaster/scripts/assurance-workspace.json")) {
            Ok(bytes) if bytes == ual::ASSURANCE_WORKSPACE.as_bytes() => (),
            Ok(_) => run.warnings.push("consumer assurance-workspace.json differs from the vendored lock; continuing with the installed consumer.".to_owned()),
            Err(error) => run.warnings.push(format!("cannot read consumer assurance-workspace.json: {error}; continuing with the installed consumer.")),
        }
        Ok(run)
    }
}

fn resolve_workspace(
    config: Option<&Path>,
    environment: Option<OsString>,
    base: &Path,
) -> miette::Result<PathBuf> {
    match config {
        Some(path) if !path.as_os_str().is_empty() => Ok(base.join(path)),
        Some(_) => Err(miette::miette!("assurance.workspace is empty. {SETUP}")),
        None => environment
            .filter(|value| !value.is_empty())
            .map(PathBuf::from)
            .ok_or_else(|| miette::miette!("no consumer workspace configured. {SETUP}")),
    }
}

fn checkout_revision(path: &Path) -> Result<String, String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(path)
        .args(["rev-parse", "--show-toplevel", "HEAD"])
        .output()
        .map_err(|error| error.to_string())?;
    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).trim().to_owned());
    }
    let text = String::from_utf8(output.stdout).map_err(|error| error.to_string())?;
    let (root, revision) = text
        .trim_end()
        .rsplit_once('\n')
        .ok_or("unexpected git rev-parse output")?;
    if Path::new(root)
        .canonicalize()
        .map_err(|error| error.to_string())?
        != path.canonicalize().map_err(|error| error.to_string())?
    {
        return Err("checkout is not a Git repository root".to_owned());
    }
    Ok(revision.to_owned())
}

fn write_record(path: &Path, run: &EnvironmentRun<'_>) -> miette::Result<()> {
    fs::write(path, ual::canonical_bytes(run).into_diagnostic()?)
        .into_diagnostic()
        .wrap_err_with(|| format!("cannot write environment run record {}", path.display()))
}

pub(super) fn lean_path_literal(path: &Path) -> miette::Result<String> {
    let text = path
        .to_str()
        .ok_or_else(|| miette::miette!("Lean driver path is not UTF-8"))?;
    use std::fmt::Write;

    let mut literal = String::with_capacity(text.len() + 2);
    literal.push('"');
    for character in text.chars() {
        match character {
            '"' => literal.push_str("\\\""),
            '\\' => literal.push_str("\\\\"),
            '\n' => literal.push_str("\\n"),
            '\r' => literal.push_str("\\r"),
            '\t' => literal.push_str("\\t"),
            // Lean accepts four-digit Unicode escapes, but not JSON's \b or \f.
            control if control.is_control() => {
                write!(literal, "\\u{:04x}", control as u32).expect("writing a string cannot fail")
            }
            other => literal.push(other),
        }
    }
    literal.push('"');
    Ok(literal)
}

pub(super) struct TemporaryDirectory(pub(super) PathBuf);

impl TemporaryDirectory {
    pub(super) fn new() -> miette::Result<Self> {
        static NEXT: AtomicU64 = AtomicU64::new(0);
        let builder = fs::DirBuilder::new();
        #[cfg(unix)]
        let builder = {
            use std::os::unix::fs::DirBuilderExt;
            let mut builder = builder;
            builder.mode(0o700);
            builder
        };
        loop {
            let path = std::env::temp_dir().join(format!(
                "aiken-assurance-{}-{}",
                std::process::id(),
                NEXT.fetch_add(1, Ordering::Relaxed)
            ));
            match builder.create(&path) {
                Ok(()) => return Ok(Self(path)),
                Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(error) => return Err(error).into_diagnostic(),
            }
        }
    }
}

impl Drop for TemporaryDirectory {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn configuration_precedes_environment_and_paths_keep_their_base() {
        let base = Path::new("project");
        assert_eq!(
            resolve_workspace(Some(Path::new("../consumer")), Some("ignored".into()), base)
                .unwrap(),
            base.join("../consumer")
        );
        assert_eq!(
            resolve_workspace(None, Some("consumer".into()), base).unwrap(),
            PathBuf::from("consumer")
        );
        assert!(resolve_workspace(Some(Path::new("")), Some("ignored".into()), base).is_err());
        assert!(resolve_workspace(None, Some("".into()), base).is_err());
        assert!(resolve_workspace(None, None, base).is_err());
    }

    #[test]
    fn revision_mismatches_are_reported_without_refusing_the_workspace() {
        let temporary = TemporaryDirectory::new().unwrap();
        for name in [
            "PlutusCoreBlaster",
            "Lean-blaster",
            "CardanoLedgerApiBlaster",
        ] {
            let checkout = temporary.0.join(name);
            fs::create_dir(&checkout).unwrap();
            for args in [
                vec!["init", "--quiet"],
                vec![
                    "-c",
                    "user.name=Test",
                    "-c",
                    "user.email=test@example.com",
                    "-c",
                    "commit.gpgsign=false",
                    "commit",
                    "--quiet",
                    "--allow-empty",
                    "-m",
                    "Fixture",
                ],
            ] {
                let status = Command::new("git")
                    .args(args)
                    .current_dir(&checkout)
                    .status()
                    .unwrap();
                assert!(status.success());
            }
        }
        let core = temporary.0.join("PlutusCoreBlaster");
        fs::create_dir(core.join("scripts")).unwrap();
        fs::write(
            core.join("scripts/assurance-workspace.json"),
            ual::ASSURANCE_WORKSPACE,
        )
        .unwrap();
        let workspace = ConsumerWorkspace {
            directory: temporary.0.join("CardanoLedgerApiBlaster"),
        };
        let run = workspace.inspect_revisions().unwrap();
        for (name, revision) in &run.revisions {
            let actual = checkout_revision(&temporary.0.join(name)).unwrap();
            assert_eq!(revision.actual.as_deref(), Some(actual.as_str()));
            assert!(run.warnings.iter().any(|warning| warning.contains(&format!(
                "{name} revision mismatch: expected {}, found {actual}",
                revision.expected
            ))));
        }
        let record = temporary.0.join("run.json");
        write_record(&record, &run).unwrap();
        let recorded: serde_json::Value =
            serde_json::from_slice(&fs::read(record).unwrap()).unwrap();
        assert_eq!(
            recorded["warnings"],
            serde_json::to_value(&run.warnings).unwrap()
        );
        fs::write(core.join("scripts/assurance-workspace.json"), "{}").unwrap();
        assert!(
            workspace
                .inspect_revisions()
                .unwrap()
                .warnings
                .iter()
                .any(|warning| warning.contains("differs from the vendored lock"))
        );
        let nested = core.join("nested");
        fs::create_dir(&nested).unwrap();
        assert_eq!(
            checkout_revision(&nested).unwrap_err(),
            "checkout is not a Git repository root"
        );
    }
}
