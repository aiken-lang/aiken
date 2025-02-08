use aiken_lang::test_framework::Benchmark;
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

    /// The maximum size to benchmark with. Note that this does not necessarily equates the number
    /// of measurements actually performed but controls the maximum size given to a Sampler.
    #[clap(long, default_value_t = Benchmark::DEFAULT_MAX_SIZE)]
    max_size: usize,

    /// Only run benchmarks if they match any of these strings.
    ///
    /// You can match a module with `-m aiken/list` or `-m list`.
    /// You can match a test with `-m "aiken/list.{map}"` or `-m "aiken/option.{flatten_1}"`
    #[clap(short, long)]
    match_benchmarks: Option<Vec<String>>,

    /// This is meant to be used with `--match-benchmarks`.
    /// It forces benchmark names to match exactly
    #[clap(short, long)]
    exact_match: bool,

    /// Environment to use for benchmarking
    env: Option<String>,
}

pub fn exec(
    Args {
        directory,
        match_benchmarks,
        exact_match,
        seed,
        max_size,
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
            p.benchmark(
                match_benchmarks.clone(),
                exact_match,
                seed,
                max_size,
                env.clone(),
            )
        },
    );
    result.map_err(|_| process::exit(1))
}
