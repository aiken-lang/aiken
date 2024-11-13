use super::build::{trace_filter_parser, trace_level_parser};
use aiken_lang::{
    ast::{TraceLevel, Tracing},
    test_framework::PropertyTest,
};
use aiken_project::watch::{self, watch_project, with_project};
use rand::prelude::*;
use std::{
    io::{self, IsTerminal},
    path::PathBuf,
    process,
};

#[derive(clap::Args)]
#[command(
    verbatim_doc_comment,
    about = color_print::cstr!(r#"
Type-check an Aiken project and run any tests found.

Test results are printed as stylized outputs when `stdout` is a TTY-capable terminal. If it
isn't, (e.g. because you are redirecting the output to a file), test results are printed as
a JSON structured object. Use `--help` to see the whole schema.
"#),
    after_long_help = color_print::cstr!(r#"<bold><underline>Output JSON schema:</underline></bold>
  <bold>type</bold>: object
  <bold>properties</bold>:
    <bold>seed</bold>: <cyan>&type_integer</cyan>
      <bold>type</bold>: integer
    <bold>summary</bold>:
      <bold>type</bold>: object
      <bold>properties</bold>: <cyan>&type_summary</cyan>
        <bold>total</bold>: *type_integer
        <bold>passed</bold>: *type_integer
        <bold>failed</bold>: *type_integer
        <bold>kind</bold>:
          <bold>type</bold>: object
          <bold>properties</bold>:
            <bold>unit</bold>: *type_integer
            <bold>property</bold>: *type_integer
    <bold>modules</bold>:
      <bold>type</bold>: array
      <bold>items</bold>:
        <bold>type</bold>: object
        <bold>properties</bold>:
          <bold>name</bold>: <cyan>&type_string</cyan>
            <bold>type</bold>: string
          <bold>summary</bold>: *type_summary
          <bold>test</bold>:
            <bold>type</bold>: array
            <bold>items</bold>:
              <bold>oneOf</bold>:
                - <bold>type</bold>: object
                  <bold>required</bold>:
                    - kind
                    - title
                    - status
                    - on_failure
                    - execution_units
                  <bold>properties</bold>:
                    <bold>kind</bold>
                      <bold>type</bold>: string
                      <bold>enum</bold>: [ "unit" ]
                    <bold>title</bold>: *type_string
                    <bold>status</bold>: <cyan>&type_status</cyan>
                      <bold>type</bold>: string
                      <bold>enum</bold>: [ "pass", "fail" ]
                    <bold>on_failure</bold>: <cyan>&type_on_failure</cyan>
                      <bold>type</bold>: string
                      <bold>enum</bold>:
                        - fail_immediately
                        - succeed_immediately
                        - succeed_eventually
                    <bold>execution_units</bold>:
                        <bold>type</bold>: object
                        <bold>properties</bold>:
                          <bold>mem</bold>: *type_integer
                          <bold>cpu</bold>: *type_integer
                    <bold>assertion</bold>: *type_string
                - <bold>type</bold>: object
                  <bold>required</bold>:
                    - kind
                    - title
                    - status
                    - on_failure
                    - iterations
                    - counterexample
                  <bold>properties</bold>:
                    <bold>kind</bold>
                      <bold>type</bold>: string
                      <bold>enum</bold>: [ "property" ]
                    <bold>title</bold>: *type_string
                    <bold>status</bold>: *type_status
                    <bold>on_failure</bold>: *type_on_failure
                    <bold>iterations</bold>: *type_integer
                    <bold>labels</bold>:
                      <bold>type</bold>: object
                      <bold>additionalProperties</bold>: *type_integer
                    <bold>counterexample</bold>:
                      <bold>oneOf</bold>:
                        - *type_string
                        - <bold>type</bold>: "null"
                        - <bold>type</bold>: object
                          <bold>properties</bold>:
                            <bold>error</bold>: *type_string

<bold><underline>Note:</underline></bold>
  You are seeing the extended help. Use `-h` instead of `--help` for a more compact view.
"#
))]
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,

    /// Deny warnings; warnings will be treated as errors
    #[clap(short = 'D', long)]
    deny: bool,

    /// Skip tests; run only the type-checker
    #[clap(short, long)]
    skip_tests: bool,

    /// When enabled, also pretty-print test UPLC on failure
    #[clap(long)]
    debug: bool,

    /// When enabled, re-run the command on file changes instead of exiting
    #[clap(long)]
    watch: bool,

    /// An initial seed to initialize the pseudo-random generator for property-tests.
    #[clap(long, value_name = "UINT")]
    seed: Option<u32>,

    /// Maximum number of successful test run for considering a property-based test valid.
    #[clap(long, default_value_t = PropertyTest::DEFAULT_MAX_SUCCESS, value_name="UINT")]
    max_success: usize,

    /// Only run tests if they match any of these strings.
    /// You can match a module with `-m aiken/list` or `-m list`.
    /// You can match a test with `-m "aiken/list.{map}"` or `-m "aiken/option.{flatten_1}"`
    #[clap(short, long, verbatim_doc_comment)]
    match_tests: Option<Vec<String>>,

    /// This is meant to be used with `--match-tests`.
    /// It forces test names to match exactly
    #[clap(short, long)]
    exact_match: bool,

    /// Environment to build against.
    #[clap(long)]
    env: Option<String>,

    /// Filter traces to be included in the generated program(s).
    ///
    ///   - user-defined:
    ///       only consider traces that you've explicitly defined
    ///       either through the 'trace' keyword of via the trace-if-false
    ///       ('?') operator.
    ///
    ///   - compiler-generated:
    ///       only included internal traces generated by the
    ///       Aiken compiler, for example in usage of 'expect'.
    ///
    ///   - all:
    ///       include both user-defined and compiler-generated traces.
    ///
    /// [default: all]
    #[clap(short = 'f', long, value_parser=trace_filter_parser(), default_missing_value="all", verbatim_doc_comment, alias="filter_traces")]
    trace_filter: Option<fn(TraceLevel) -> Tracing>,

    /// Choose the verbosity level of traces:
    ///
    ///   - silent: disable traces altogether
    ///   - compact: only culprit line numbers are shown on failures
    ///   - verbose: enable full verbose traces as provided by the user or the compiler
    ///
    /// [optional]
    #[clap(short, long, value_parser=trace_level_parser(), default_value_t=TraceLevel::Verbose, verbatim_doc_comment)]
    trace_level: TraceLevel,
}

pub fn exec(
    Args {
        directory,
        deny,
        skip_tests,
        debug,
        match_tests,
        exact_match,
        watch,
        trace_filter,
        trace_level,
        seed,
        max_success,
        env,
    }: Args,
) -> miette::Result<()> {
    let mut rng = rand::thread_rng();

    let seed = seed.unwrap_or_else(|| rng.gen());

    let result = if watch {
        watch_project(directory.as_deref(), watch::default_filter, 500, |p| {
            p.check(
                skip_tests,
                match_tests.clone(),
                debug,
                exact_match,
                seed,
                max_success,
                match trace_filter {
                    Some(trace_filter) => trace_filter(trace_level),
                    None => Tracing::All(trace_level),
                },
                env.clone(),
            )
        })
    } else {
        with_project(
            directory.as_deref(),
            deny,
            !io::stdout().is_terminal(),
            |p| {
                p.check(
                    skip_tests,
                    match_tests.clone(),
                    debug,
                    exact_match,
                    seed,
                    max_success,
                    match trace_filter {
                        Some(trace_filter) => trace_filter(trace_level),
                        None => Tracing::All(trace_level),
                    },
                    env.clone(),
                )
            },
        )
    };

    result.map_err(|_| process::exit(1))
}
