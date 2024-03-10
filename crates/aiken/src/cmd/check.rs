use super::build::{filter_traces_parser, trace_level_parser};
use aiken_lang::ast::{TraceLevel, Tracing};
use aiken_project::{
    test_framework::PropertyTest,
    watch::{self, watch_project, with_project},
};
use owo_colors::{OwoColorize, Stream::Stderr};
use rand::prelude::*;
use termion::input::TermRead;
use std::{path::PathBuf, process, sync::{Arc, Mutex}, thread};

#[derive(clap::Args)]
/// Type-check an Aiken project
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

    /// Filter traces to be considered during testing:
    ///   - user-defined: only consider traces that you've explicitly defined (either through the
    ///   'trace' keyword of via the trace-if-false ('?') operator.
    ///   - compiler-generated: only included internal traces generated by the Aiken compiler, for
    ///   example in usage of 'expect'.
    ///   - all: include both user-defined and compiler-generated traces.
    /// [optional] [default: all]
    #[clap(short, long, value_parser=filter_traces_parser(), default_missing_value="all", verbatim_doc_comment)]
    filter_traces: Option<fn(TraceLevel) -> Tracing>,

    /// Choose the verbosity level of traces:
    ///   - silent: disable traces altogether
    ///   - compact: only culprit line numbers are shown on failures
    ///   - verbose: enable full verbose traces as provided by the user or the compiler
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
        filter_traces,
        trace_level,
        seed,
        max_success,
    }: Args,
) -> miette::Result<()> {
    let mut rng = rand::thread_rng();

    let seed = seed.unwrap_or_else(|| rng.gen());

    let result = if watch {
        let mut stdin = termion::async_stdin().keys();

        let (sender, receiver) = std::sync::mpsc::channel();
        let filter = Arc::new(Mutex::new("".to_string()));
        {
            let filter = filter.clone();
            thread::spawn(move || {
                let mut working = "".to_string();
                loop {
                    if let Some(c) = stdin.next() {
                        match c {
                            Ok(termion::event::Key::Backspace) => {
                                if working != "" {
                                    working = working[..working.len() - 1].to_string();
                                }
                            },
                            Ok(termion::event::Key::Char('\n')) => {
                                let mut filt = filter.lock().unwrap();
                                *filt = working;
                                working = "".to_string();
                                sender.send(()).unwrap();
                            }
                            Ok(termion::event::Key::Char(c)) => {
                                working.push(c);
                            },
                            _ => {},
                        }
                    }
                }
            });
        }

        watch_project(directory.as_deref(), Some(receiver), watch::default_filter, 500, |p| {
            let filt = filter.lock().unwrap();
            let final_match_tests = match match_tests.clone() {
                Some(existing) if *filt == "" => { Some(existing) }
                Some(existing) => {
                    let mut e = existing.clone();
                    e.push(filt.clone());
                    Some(e)
                },
                None if *filt != "" => Some(vec![filt.clone()]),
                None => None,
            };
            let result = p.check(
                skip_tests,
                final_match_tests.clone(),
                debug,
                exact_match,
                seed,
                max_success,
                match filter_traces {
                    Some(filter_traces) => filter_traces(trace_level),
                    None => Tracing::All(trace_level),
                },
            );
            if let Some(fmt) = final_match_tests {
                println!("      {} {}",
                    "Filtered by:"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                        fmt.join(", ")
                );
            }
            result
        })
    } else {
        with_project(directory.as_deref(), deny, |p| {
            p.check(
                skip_tests,
                match_tests.clone(),
                debug,
                exact_match,
                seed,
                max_success,
                match filter_traces {
                    Some(filter_traces) => filter_traces(trace_level),
                    None => Tracing::All(trace_level),
                },
            )
        })
    };

    result.map_err(|_| process::exit(1))
}
