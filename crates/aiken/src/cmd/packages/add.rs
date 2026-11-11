use aiken_project::{
    config::{Dependency, Platform, ProjectConfig},
    error::Warning,
    package_name::PackageName,
    package_subdir::PackageSubdir,
    pretty,
};
use miette::IntoDiagnostic;
use owo_colors::{OwoColorize, Stream::Stderr};
use std::{path::PathBuf, process};

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
/// Add a new project package as dependency
pub struct Args {
    /// Package name, in the form of {owner}/{repository}.
    ///
    /// For example → 'add aiken-lang/stdlib'
    ///
    /// Note that by default, this assumes the package is located
    /// on Github.
    pub package: String,
    /// The package version, as a git commit hash, a tag or a branch name.
    #[clap(long)]
    pub version: String,

    /// Directory containing aiken.toml within the repository (for example, onchain).
    #[clap(long)]
    pub subdir: Option<PackageSubdir>,

    #[clap(hide = true, long)]
    pub overwrite: bool,
}

pub fn exec(args: Args) -> miette::Result<()> {
    let root = PathBuf::from(".");

    let dependency = Dependency {
        name: PackageName::from_str_unchecked(&args.package)?,
        version: args.version,
        source: Platform::Github,
        subdir: args.subdir,
    };

    let config = match ProjectConfig::load(&root) {
        Ok(config) => config,
        Err(e) => {
            e.report();
            process::exit(1);
        }
    };

    // An upgrade must target an existing entry. Otherwise a missing or wrong
    // --subdir would silently add a second dependency on the same repository.
    if args.overwrite {
        let entries: Vec<_> = config
            .dependencies
            .iter()
            .filter(|existing| existing.name == dependency.name)
            .collect();

        if !entries.is_empty() && !entries.iter().any(|e| e.subdir == dependency.subdir) {
            let location = match &dependency.subdir {
                Some(subdir) => format!("in subdirectory '{subdir}'"),
                None => "at the repository root".to_string(),
            };
            let options = entries
                .iter()
                .map(|e| match &e.subdir {
                    Some(subdir) => format!("--subdir {subdir}"),
                    None => "no --subdir".to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            miette::bail!(
                help = format!(
                    "This project depends on '{}' with: {options}.",
                    dependency.name
                ),
                "I couldn't find a dependency on '{}' {location}.",
                dependency.name
            );
        }
    }

    eprintln!(
        "{} {}",
        pretty::pad_left("Package".to_string(), 13, " ")
            .bold()
            .purple(),
        dependency
            .name
            .if_supports_color(Stderr, |s| s.bright_blue()),
    );

    match config.insert(&dependency, args.overwrite) {
        Some(config) => {
            config.save(&root).into_diagnostic()?;
            eprintln!(
                "{} version → {}",
                pretty::pad_left(
                    if args.overwrite { "Changed" } else { "Added" }.to_string(),
                    13,
                    " "
                )
                .if_supports_color(Stderr, |s| s.purple())
                .if_supports_color(Stderr, |s| s.bold()),
                dependency.version.if_supports_color(Stderr, |s| s.yellow())
            );
            Ok(())
        }
        None => {
            let warning = Warning::DependencyAlreadyExists {
                name: dependency.name,
            };
            warning.report();
            process::exit(1)
        }
    }
}
