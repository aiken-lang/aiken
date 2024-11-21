use super::build::{filter_traces_parser, trace_level_parser};
use aiken_lang::ast::{TraceLevel, Tracing};
use aiken_project::{
    test_framework::PropertyTest,
    watch::{self, watch_project, with_project},
};
use rand::prelude::*;
use std::{path::PathBuf, process};

#[derive(clap::Args)]
/// Type-check an Aiken project
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// An initial seed to initialize the pseudo-random generator for property-tests.
    #[clap(long)]
    seed: Option<u32>,

    /// Maximum number of successful test run for considering a property-based test valid.
    #[clap(long, default_value_t = PropertyTest::DEFAULT_MAX_SUCCESS)]
    max_success: usize,

    /// Only run tests if they match any of these strings.
    /// You can match a module with `-m aiken/list` or `-m list`.
    /// You can match a test with `-m "aiken/list.{map}"` or `-m "aiken/option.{flatten_1}"`
    #[clap(short, long)]
    match_tests: Option<Vec<String>>,

    /// This is meant to be used with `--match-tests`.
    /// It forces test names to match exactly
    #[clap(short, long)]
    exact_match: bool,
}

pub fn exec(
    Args {
        directory,
        match_tests,
        exact_match,
        seed,
        max_success,
    }: Args,
) -> miette::Result<()> {
    // Actually we don't want to use check right? 
    let mut rng = rand::thread_rng();

    let seed = seed.unwrap_or_else(|| rng.gen());

    let result = with_project(directory.as_deref(), false, |p| {
        // We don't want to check here, we want to benchmark
        p.benchmark(
            match_tests.clone(),
            exact_match,
            seed,
            max_success
        )
    });
// todo riley - We need to either print or output the results to a file.
    result.map_err(|_| process::exit(1))
}