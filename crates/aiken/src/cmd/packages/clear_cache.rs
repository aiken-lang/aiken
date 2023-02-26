use aiken_project::{paths, pretty};
use miette::IntoDiagnostic;
use owo_colors::{OwoColorize, Stream::Stderr};
use std::fs;

pub fn exec() -> miette::Result<()> {
    let dir = paths::packages_cache();
    eprintln!(
        "{} {}",
        pretty::pad_left("Clearing".to_string(), 13, " ")
            .if_supports_color(Stderr, |s| s.purple())
            .if_supports_color(Stderr, |s| s.bold()),
        dir.display().if_supports_color(Stderr, |s| s.bold()),
    );
    let packages = fs::read_dir(&dir).into_diagnostic()?;
    for package in packages {
        let path = package.into_diagnostic()?.path();
        eprintln!(
            "{} {}",
            pretty::pad_left("Removing".to_string(), 13, " ")
                .if_supports_color(Stderr, |s| s.purple())
                .if_supports_color(Stderr, |s| s.bold()),
            path.file_name()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default()
                .if_supports_color(Stderr, |s| s.bright_blue()),
        );
        fs::remove_file(path).into_diagnostic()?;
    }
    println!(
        "{}",
        pretty::pad_left("Done".to_string(), 13, " ")
            .if_supports_color(Stderr, |s| s.purple())
            .if_supports_color(Stderr, |s| s.bold())
    );
    Ok(())
}
