use aiken_project::{
    config::{Config, Dependency, Platform},
    error::Warning,
    package_name::PackageName,
    pretty,
};
use miette::IntoDiagnostic;
use owo_colors::OwoColorize;
use std::{path::PathBuf, process, str::FromStr};

#[derive(clap::Args)]
/// Add a new project package as dependency
pub struct Args {
    /// Package name, in the form of {owner}/{repository}.
    ///
    /// For example → 'add aiken-lang/stdlib'
    ///
    /// Note that by default, this assumes the package is located
    /// on Github.
    package: String,
    /// The package version, as a git commit hash, a tag or a branch name.
    #[clap(long)]
    version: String,
}

pub fn exec(args: Args) -> miette::Result<()> {
    let root = PathBuf::from(".");

    let dependency = Dependency {
        name: PackageName::from_str(&args.package)?,
        version: args.version,
        source: Platform::Github,
    };

    let config = match Config::load(&root) {
        Ok(config) => config,
        Err(e) => {
            e.report();
            process::exit(1);
        }
    };

    println!(
        "{} {}",
        pretty::pad_left("Adding".to_string(), 13, " ")
            .bold()
            .purple(),
        dependency.name.bright_blue(),
    );

    match config.insert(&dependency, false) {
        Some(config) => {
            config.save(&root).into_diagnostic()?;
            println!(
                "{} version = {}",
                pretty::pad_left("Added".to_string(), 13, " ")
                    .bold()
                    .purple(),
                dependency.version.yellow()
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
