use std::path::PathBuf;

#[derive(clap::Args)]
/// Type-check an Aiken project
pub struct Args {
    /// Path to project
    #[clap(short, long)]
    directory: Option<PathBuf>,

    /// Skip tests; run only the type-checker
    #[clap(short, long)]
    skip_tests: bool,

    /// Only run tests if their path + name match the given string
    #[clap(short, long)]
    match_tests: Option<String>,
}

pub fn exec(
    Args {
        directory,
        skip_tests,
        match_tests,
    }: Args,
) -> miette::Result<()> {
    crate::with_project(directory, |p| p.check(skip_tests, match_tests.clone()))
}
