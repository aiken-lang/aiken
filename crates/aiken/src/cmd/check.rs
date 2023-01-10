use std::path::PathBuf;

#[derive(clap::Args)]
/// Type-check an Aiken project
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Skip tests; run only the type-checker
    #[clap(short, long)]
    skip_tests: bool,

    /// When enabled, also pretty-print test UPLC on failure
    #[clap(long)]
    debug: bool,

    /// Only run tests if their path + name match the given string
    #[clap(short, long)]
    match_tests: Option<Vec<String>>,
}

pub fn exec(
    Args {
        directory,
        skip_tests,
        debug,
        match_tests,
    }: Args,
) -> miette::Result<()> {
    crate::with_project(directory, |p| {
        p.check(skip_tests, match_tests.clone(), debug)
    })
}
