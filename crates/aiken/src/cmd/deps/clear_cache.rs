use aiken_project::{paths, pretty};
use miette::IntoDiagnostic;
use owo_colors::OwoColorize;
use std::fs;

pub fn exec() -> miette::Result<()> {
    let dir = paths::packages_cache();
    println!(
        "{} {}",
        pretty::pad_left("Clearing".to_string(), 13, " ")
            .bold()
            .purple(),
        dir.display().bold(),
    );
    let packages = fs::read_dir(&dir).into_diagnostic()?;
    for package in packages {
        let path = package.into_diagnostic()?.path();
        println!(
            "{} {}",
            pretty::pad_left("Removing".to_string(), 13, " ")
                .bold()
                .purple(),
            path.file_name()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default()
                .bright_blue(),
        );
        fs::remove_file(path).into_diagnostic()?;
    }
    println!(
        "{}",
        pretty::pad_left("Done".to_string(), 13, " ")
            .bold()
            .purple()
    );
    Ok(())
}
