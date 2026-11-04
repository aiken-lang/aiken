//! Project compilation, environment binding and publication for `verify generate`.

use super::bundle;
use crate::{Project, options::Options, telemetry::EventListener};
use aiken_lang::ast::Tracing;
use aiken_ual as ual;
use miette::{IntoDiagnostic, WrapErr};
use std::{
    collections::BTreeMap,
    fs,
    io::Write,
    path::{Path, PathBuf},
    time::UNIX_EPOCH,
};

/// Original manifest bytes, validated without claiming equality to a live consumer.
pub struct Environment {
    path: PathBuf,
    bytes: Vec<u8>,
}

impl Environment {
    /// Resolve relative paths against the caller's working directory; hash raw bytes.
    pub fn read(path: &Path) -> miette::Result<Self> {
        let bytes = fs::read(path)
            .into_diagnostic()
            .wrap_err_with(|| format!("cannot read environment manifest {}", path.display()))?;
        let value = serde_json::from_slice(&bytes)
            .into_diagnostic()
            .wrap_err("invalid environment manifest JSON")?;
        ual::validate(ual::DocumentKind::Environment, &value).into_diagnostic()?;
        Ok(Self {
            path: path.canonicalize().into_diagnostic()?,
            bytes,
        })
    }
}

impl<T: EventListener> Project<T> {
    /// Run the producer pass. An absent environment always means a write-free dry run.
    /// Dry runs only read already installed dependencies; ordinary builds resolve them.
    pub fn generate_assurance(
        &mut self,
        environment: Option<&Environment>,
        dry_run: bool,
        tracing: Tracing,
        env: Option<String>,
    ) -> miette::Result<usize> {
        let dry_run = dry_run || environment.is_none();
        let options = Options {
            tracing,
            env,
            ..Options::default()
        };
        if dry_run {
            self.compile_read_only(options)
        } else {
            self.compile(options)
        }
        .map_err(|errors| {
            for error in errors {
                error.report();
            }
            crate::watch::ExitFailure::into_report()
        })?;

        // The manifest's date is stable across reruns, unlike the invocation date.
        // The package owner is the only author identity declared by ProjectConfig.
        let modified = fs::metadata(self.root.join("aiken.toml"))
            .and_then(|metadata| metadata.modified())
            .into_diagnostic()?;
        let days = modified
            .duration_since(UNIX_EPOCH)
            .into_diagnostic()?
            .as_secs()
            / 86_400;
        let pending = self
            .assurance_bundle(
                ual::AssurancePreamble {
                    title: self.config.name.to_string(),
                    description: (!self.config.description.is_empty())
                        .then(|| self.config.description.clone()),
                    version: Some(self.config.version.clone()),
                    authors: vec![self.config.name.owner.clone()],
                    created: civil_date(days)?,
                    license: self.config.license.clone(),
                    extensions: BTreeMap::new(),
                },
                tracing,
            )
            .map_err(report_bundle_error)?;
        let count = pending.contexts().len();
        if let Some(environment) = environment {
            let output = self
                .root
                .canonicalize()
                .into_diagnostic()?
                .join("assurance");
            let uri = relative_path(&output, &environment.path);
            let uri = uri
                .to_str()
                .ok_or_else(|| miette::miette!("environment path is not UTF-8"))?;
            let bundle = pending
                .bind_environment(ual::Artifact {
                    uri: uri.to_owned(),
                    hash: ual::sha256_digest(&environment.bytes),
                    media_type: Some("application/json".to_owned()),
                    extensions: BTreeMap::new(),
                })
                .map_err(report_bundle_error)?;
            for bytes in bundle.contexts().values() {
                let context = serde_json::from_slice(bytes).into_diagnostic()?;
                ual::validate_environment(&context, &environment.bytes).into_diagnostic()?;
            }
            if !dry_run {
                write_bundle(&output, &bundle, &environment.path)?;
            }
        }
        Ok(count)
    }
}

fn report_bundle_error(error: bundle::Error) -> miette::Report {
    // Compiler errors contain Rc values, whereas miette::Report requires Send + Sync.
    // Render the borrowed diagnostic, retaining related source spans, as build does.
    let mut rendered = String::new();
    match miette::GraphicalReportHandler::new().render_report(&mut rendered, &error) {
        Ok(()) => eprintln!("{rendered}"),
        Err(_) => eprintln!("{error}"),
    }
    crate::watch::ExitFailure::into_report()
}

fn relative_path(base: &Path, target: &Path) -> PathBuf {
    let mut base_components = base.components().peekable();
    let mut target_components = target.components().peekable();
    while base_components.peek().is_some() && base_components.peek() == target_components.peek() {
        base_components.next();
        target_components.next();
    }
    // Different Windows volumes cannot be expressed as a relative path.
    if target_components
        .peek()
        .is_some_and(|part| matches!(part, std::path::Component::Prefix(_)))
    {
        return target.to_path_buf();
    }
    base_components
        .map(|_| std::ffi::OsStr::new(".."))
        .chain(target_components.map(|part| part.as_os_str()))
        .collect()
}

fn write_bundle(output: &Path, bundle: &bundle::Bundle, environment: &Path) -> miette::Result<()> {
    // In particular, never follow `assurance -> .` and overwrite root plutus.json.
    match fs::symlink_metadata(output) {
        Ok(metadata) if !metadata.file_type().is_dir() => {
            return Err(miette::miette!(
                "{} must be a real directory, not a symlink or file",
                output.display()
            ));
        }
        Ok(_) => (),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => (),
        Err(error) => return Err(error).into_diagnostic(),
    }
    let artifacts = std::iter::once(("plutus.json".to_owned(), bundle.blueprint_bytes()))
        .chain(std::iter::once((
            "assurance.json".to_owned(),
            bundle.assurance_bytes(),
        )))
        .chain(
            bundle
                .contexts()
                .iter()
                .map(|(id, bytes)| (format!("checking-{id}.json"), bytes.as_slice())),
        )
        .collect::<BTreeMap<_, _>>();
    if environment.parent() == Some(output)
        && environment
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| artifacts.contains_key(name) || is_checking_file(name))
    {
        return Err(miette::miette!(
            "environment manifest overlaps generated bundle files"
        ));
    }
    fs::create_dir_all(output).into_diagnostic()?;
    for (name, bytes) in &artifacts {
        replace_file(&output.join(name), bytes)?;
    }
    // Prune only producer-owned checking files, after all new artifacts are written.
    for entry in fs::read_dir(output).into_diagnostic()? {
        let entry = entry.into_diagnostic()?;
        let name = entry.file_name();
        if let Some(name) = name.to_str()
            && is_checking_file(name)
            && !artifacts.contains_key(name)
            && !entry.file_type().into_diagnostic()?.is_dir()
        {
            fs::remove_file(entry.path()).into_diagnostic()?;
        }
    }
    Ok(())
}

fn is_checking_file(name: &str) -> bool {
    name.starts_with("checking-") && name.ends_with(".json")
}

/// Replace rather than truncate: existing symlinks/hard links cannot redirect writes.
fn replace_file(path: &Path, bytes: &[u8]) -> miette::Result<()> {
    let temporary = path.with_extension(format!("json.{}.tmp", std::process::id()));
    let mut file = fs::File::create_new(&temporary).into_diagnostic()?;
    let result = file.write_all(bytes).and_then(|()| {
        drop(file);
        fs::rename(&temporary, path)
    });
    if result.is_err() {
        let _ = fs::remove_file(&temporary);
    }
    result
        .into_diagnostic()
        .wrap_err_with(|| format!("cannot write {}", path.display()))
}

/// Convert UTC days since the Unix epoch using the Gregorian 400-year cycle.
fn civil_date(days: u64) -> miette::Result<String> {
    // UAL dates have exactly four year digits.
    if days > 2_932_896 {
        return Err(miette::miette!(
            "project manifest date is outside the UAL date range"
        ));
    }
    let shifted = days + 719_468;
    let era = shifted / 146_097;
    let day_of_era = shifted % 146_097;
    let year_of_era =
        (day_of_era - day_of_era / 1460 + day_of_era / 36_524 - day_of_era / 146_096) / 365;
    let day_of_year = day_of_era - (365 * year_of_era + year_of_era / 4 - year_of_era / 100);
    let month_prime = (5 * day_of_year + 2) / 153;
    let day = day_of_year - (153 * month_prime + 2) / 5 + 1;
    let month = if month_prime < 10 {
        month_prime + 3
    } else {
        month_prime - 9
    };
    let year = year_of_era + era * 400 + u64::from(month <= 2);
    Ok(format!("{year:04}-{month:02}-{day:02}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manifest_date_handles_leap_centuries_and_schema_boundaries() {
        for (days, expected) in [
            (0, "1970-01-01"),
            (11_016, "2000-02-29"),
            (47_541, "2100-03-01"),
            (20_761, "2026-11-04"),
            (2_932_896, "9999-12-31"),
        ] {
            assert_eq!(civil_date(days).unwrap(), expected);
        }
        assert!(civil_date(2_932_897).is_err());
        assert!(civil_date(u64::MAX).is_err());
    }
}
