use aiken_project::{
    config::{Config, Dependency, Platform},
    error::Warning,
    github,
    package_name::PackageName,
    pretty,
};
use miette::IntoDiagnostic;
use owo_colors::{OwoColorize, Stream::Stderr};
use std::{path::PathBuf, process, str::FromStr};

#[derive(clap::Args)]
#[clap(disable_version_flag = true)]
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
    pub version: Option<String>,

    #[clap(hide = true, long)]
    pub overwrite: bool,
}

pub fn exec(args: Args) -> miette::Result<()> {
    let root = PathBuf::from(".");

    let default_branch = "main".into();
    let default_version =
        || github::repo::default_version_of(&args.package).unwrap_or(default_branch);

    let dependency = Dependency {
        name: PackageName::from_str(&args.package)?,
        version: args.version.unwrap_or_else(default_version),
        source: Platform::Github,
    };

    let config = match Config::load(&root) {
        Ok(config) => config,
        Err(e) => {
            e.report();
            process::exit(1);
        }
    };

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
