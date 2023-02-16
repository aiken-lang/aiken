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

    /// Only run tests if they match any of these strings.
    /// You can match a module with `-m aiken/list` or `-m list`.
    /// You can match a test with `-m "aiken/list.{map}"` or `-m "aiken/option.{flatten_1}"`
    #[clap(short, long)]
    match_tests: Option<Vec<String>>,

    /// This is meant to be used with `--match-tests`.
    /// It forces test names to match exactly
    #[clap(short, long)]
    exact_match: bool,

    /// Remove traces when generating code (including tests)
    #[clap(long)]
    no_traces: bool,
}

pub fn exec(
    Args {
        directory,
        skip_tests,
        debug,
        match_tests,
        exact_match,
        no_traces,
    }: Args,
) -> miette::Result<()> {
    crate::with_project(directory, |p| {
        p.check(
            skip_tests,
            match_tests.clone(),
            debug,
            exact_match,
            (!no_traces).into(),
        )
    })
}
