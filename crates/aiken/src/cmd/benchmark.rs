use aiken_lang::test_framework::PropertyTest;
use aiken_project::watch::with_project;
use rand::prelude::*;
use std::{
    io::{self, IsTerminal},
    path::PathBuf,
    process,
};

#[derive(clap::Args)]
/// Benchmark an Aiken project
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// An initial seed to initialize the pseudo-random generator for property-tests.
    #[clap(long)]
    seed: Option<u32>,

    /// How many times we will run each benchmark in the relevant project.
    #[clap(long, default_value_t = PropertyTest::DEFAULT_MAX_SUCCESS)]
    times_to_run: usize,

    /// Only run tests if they match any of these strings.
    /// You can match a module with `-m aiken/list` or `-m list`.
    /// You can match a test with `-m "aiken/list.{map}"` or `-m "aiken/option.{flatten_1}"`
    #[clap(short, long)]
    match_tests: Option<Vec<String>>,

    /// This is meant to be used with `--match-tests`.
    /// It forces test names to match exactly
    #[clap(short, long)]
    exact_match: bool,

    /// Environment to use for benchmarking
    env: Option<String>,
}

pub fn exec(
    Args {
        directory,
        match_tests,
        exact_match,
        seed,
        times_to_run,
        env,
    }: Args,
) -> miette::Result<()> {
    let mut rng = rand::thread_rng();

    let seed = seed.unwrap_or_else(|| rng.gen());

    let result = with_project(
        directory.as_deref(),
        false,
        !io::stdout().is_terminal(),
        |p| {
            // We don't want to check here, we want to benchmark
            p.benchmark(
                match_tests.clone(),
                exact_match,
                seed,
                times_to_run,
                env.clone(),
            )
        },
    );
    result.map_err(|_| process::exit(1))
}
