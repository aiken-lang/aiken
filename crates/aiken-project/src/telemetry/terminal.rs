use super::{find_max_execution_units, group_by_module, DownloadSource, Event, EventListener};
use crate::pretty;
use aiken_lang::{
    ast::OnTestFailure,
    expr::UntypedExpr,
    format::Formatter,
    test_framework::{AssertionStyleOptions, PropertyTestResult, TestResult, UnitTestResult},
};
use owo_colors::{OwoColorize, Stream::Stderr};
use uplc::machine::cost_model::ExBudget;

#[derive(Debug, Default, Clone, Copy)]
pub struct Terminal;

impl EventListener for Terminal {
    fn handle_event(&self, event: Event) {
        match event {
            Event::StartingCompilation {
                name,
                version,
                root,
            } => {
                eprintln!(
                    "{} {} {} ({})",
                    "    Compiling"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    name.if_supports_color(Stderr, |s| s.bold()),
                    version,
                    root.display()
                        .if_supports_color(Stderr, |s| s.bright_blue())
                );
            }
            Event::BuildingDocumentation {
                name,
                version,
                root,
            } => {
                eprintln!(
                    "{} {} for {} {} ({})",
                    "   Generating"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    "documentation".if_supports_color(Stderr, |s| s.bold()),
                    name.if_supports_color(Stderr, |s| s.bold()),
                    version,
                    root.to_str()
                        .unwrap_or("")
                        .if_supports_color(Stderr, |s| s.bright_blue())
                );
            }
            Event::WaitingForBuildDirLock => {
                eprintln!(
                    "{}",
                    "Waiting for build directory lock ..."
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple())
                );
            }
            Event::DumpingUPLC { path } => {
                eprintln!(
                    "{} {} ({})",
                    "    Exporting"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    "UPLC".if_supports_color(Stderr, |s| s.bold()),
                    path.display()
                        .if_supports_color(Stderr, |s| s.bright_blue())
                );
            }
            Event::GeneratingBlueprint { path } => {
                eprintln!(
                    "{} {} ({})",
                    "   Generating"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    "project's blueprint".if_supports_color(Stderr, |s| s.bold()),
                    path.display()
                        .if_supports_color(Stderr, |s| s.bright_blue())
                );
            }
            Event::GeneratingDocFiles { output_path } => {
                eprintln!(
                    "{} {} to {}",
                    "      Writing"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    "documentation files".if_supports_color(Stderr, |s| s.bold()),
                    output_path
                        .to_str()
                        .unwrap_or("")
                        .if_supports_color(Stderr, |s| s.bright_blue())
                );
            }
            Event::GeneratingUPLCFor { name, path } => {
                eprintln!(
                    "{} {} {}.{{{}}}",
                    "   Generating"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    "UPLC for"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.white()),
                    path.to_str()
                        .unwrap_or("")
                        .if_supports_color(Stderr, |s| s.blue()),
                    name.if_supports_color(Stderr, |s| s.bright_blue()),
                );
            }
            Event::RunningTests => {
                eprintln!(
                    "{} {}",
                    "      Testing"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    "...".if_supports_color(Stderr, |s| s.bold())
                );
            }
            Event::FinishedTests { seed, tests } => {
                let (max_mem, max_cpu, max_iter) = find_max_execution_units(&tests);

                for (module, results) in &group_by_module(&tests) {
                    let title = module
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.blue())
                        .to_string();

                    let tests = results
                        .iter()
                        .map(|r| fmt_test(r, max_mem, max_cpu, max_iter, true))
                        .collect::<Vec<String>>()
                        .join("\n");

                    let seed_info = if results
                        .iter()
                        .any(|t| matches!(t, TestResult::PropertyTestResult { .. }))
                    {
                        format!(
                            "with {opt}={seed} → ",
                            opt = "--seed".if_supports_color(Stderr, |s| s.bold()),
                            seed = format!("{seed}").if_supports_color(Stderr, |s| s.bold())
                        )
                    } else {
                        String::new()
                    };

                    if !tests.is_empty() {
                        println!();
                    }

                    let summary = format!("{}{}", seed_info, fmt_test_summary(results, true));
                    println!(
                        "{}\n",
                        pretty::indent(
                            &pretty::open_box(&title, &tests, &summary, |border| border
                                .if_supports_color(Stderr, |s| s.bright_black())
                                .to_string()),
                            4
                        )
                    );
                }

                if !tests.is_empty() {
                    println!();
                }
            }
            Event::ResolvingPackages { name } => {
                eprintln!(
                    "{} {}",
                    "    Resolving"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    name.if_supports_color(Stderr, |s| s.bold())
                )
            }
            Event::PackageResolveFallback { name } => {
                eprintln!(
                    "{} {}\n        ↳ You're seeing this message because the package version is unpinned and the network is not accessible.",
                    "        Using"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.yellow()),
                    format!("uncertain local version for {name}")
                        .if_supports_color(Stderr, |s| s.yellow())
                )
            }
            Event::PackagesDownloaded {
                start,
                count,
                source,
            } => {
                let elapsed = format!("{:.2}s", start.elapsed().as_millis() as f32 / 1000.);

                let msg = match count {
                    1 => format!("1 package in {elapsed}"),
                    _ => format!("{count} packages in {elapsed}"),
                };

                eprintln!(
                    "{} {} from {source}",
                    match source {
                        DownloadSource::Network => "   Downloaded",
                        DownloadSource::Cache => "      Fetched",
                    }
                    .if_supports_color(Stderr, |s| s.bold())
                    .if_supports_color(Stderr, |s| s.purple()),
                    msg.if_supports_color(Stderr, |s| s.bold())
                )
            }
            Event::ResolvingVersions => {
                eprintln!(
                    "{} {}",
                    "    Resolving"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    "dependencies".if_supports_color(Stderr, |s| s.bold())
                )
            }
        }
    }
}

fn fmt_test(
    result: &TestResult<UntypedExpr, UntypedExpr>,
    max_mem: usize,
    max_cpu: usize,
    max_iter: usize,
    styled: bool,
) -> String {
    // Status
    let mut test = if result.is_success() {
        pretty::style_if(styled, "PASS".to_string(), |s| {
            s.if_supports_color(Stderr, |s| s.bold())
                .if_supports_color(Stderr, |s| s.green())
                .to_string()
        })
    } else {
        pretty::style_if(styled, "FAIL".to_string(), |s| {
            s.if_supports_color(Stderr, |s| s.bold())
                .if_supports_color(Stderr, |s| s.red())
                .to_string()
        })
    };

    // Execution units / iteration steps
    match result {
        TestResult::UnitTestResult(UnitTestResult { spent_budget, .. }) => {
            let ExBudget { mem, cpu } = spent_budget;
            let mem_pad = pretty::pad_left(mem.to_string(), max_mem, " ");
            let cpu_pad = pretty::pad_left(cpu.to_string(), max_cpu, " ");

            test = format!(
                "{test} [mem: {mem_unit}, cpu: {cpu_unit}]",
                mem_unit = pretty::style_if(styled, mem_pad, |s| s
                    .if_supports_color(Stderr, |s| s.cyan())
                    .to_string()),
                cpu_unit = pretty::style_if(styled, cpu_pad, |s| s
                    .if_supports_color(Stderr, |s| s.cyan())
                    .to_string()),
            );
        }
        TestResult::PropertyTestResult(PropertyTestResult { iterations, .. }) => {
            test = format!(
                "{test} [after {} test{}]",
                pretty::pad_left(
                    if *iterations == 0 {
                        "?".to_string()
                    } else {
                        iterations.to_string()
                    },
                    max_iter,
                    " "
                ),
                if *iterations > 1 { "s" } else { "" }
            );
        }
    }

    // Title
    test = format!(
        "{test} {title}",
        title = pretty::style_if(styled, result.title().to_string(), |s| s
            .if_supports_color(Stderr, |s| s.bright_blue())
            .to_string())
    );

    // Annotations
    match result {
        TestResult::UnitTestResult(UnitTestResult {
            assertion: Some(assertion),
            test: unit_test,
            ..
        }) if !result.is_success() => {
            test = format!(
                "{test}\n{}",
                assertion.to_string(
                    match unit_test.on_test_failure {
                        OnTestFailure::FailImmediately => false,
                        OnTestFailure::SucceedEventually | OnTestFailure::SucceedImmediately =>
                            true,
                    },
                    &AssertionStyleOptions::new(Some(&Stderr))
                ),
            );
        }
        _ => (),
    }

    // CounterExamples
    if let TestResult::PropertyTestResult(PropertyTestResult { counterexample, .. }) = result {
        match counterexample {
            Err(err) => {
                test = format!(
                    "{test}\n{}\n{}",
                    "× fuzzer failed unexpectedly"
                        .if_supports_color(Stderr, |s| s.red())
                        .if_supports_color(Stderr, |s| s.bold()),
                    format!("| {err}").if_supports_color(Stderr, |s| s.red())
                );
            }

            Ok(None) => {
                if !result.is_success() {
                    test = format!(
                        "{test}\n{}",
                        "× no counterexample found"
                            .if_supports_color(Stderr, |s| s.red())
                            .if_supports_color(Stderr, |s| s.bold())
                    );
                }
            }

            Ok(Some(counterexample)) => {
                let is_expected_failure = result.is_success();

                test = format!(
                    "{test}\n{}\n{}",
                    if is_expected_failure {
                        "★ counterexample"
                            .if_supports_color(Stderr, |s| s.green())
                            .if_supports_color(Stderr, |s| s.bold())
                            .to_string()
                    } else {
                        "× counterexample"
                            .if_supports_color(Stderr, |s| s.red())
                            .if_supports_color(Stderr, |s| s.bold())
                            .to_string()
                    },
                    &Formatter::new()
                        .expr(counterexample, false)
                        .to_pretty_string(60)
                        .lines()
                        .map(|line| {
                            format!(
                                "{} {}",
                                "│".if_supports_color(Stderr, |s| if is_expected_failure {
                                    s.green().to_string()
                                } else {
                                    s.red().to_string()
                                }),
                                line
                            )
                        })
                        .collect::<Vec<String>>()
                        .join("\n"),
                );
            }
        }
    }

    // Labels
    if let TestResult::PropertyTestResult(PropertyTestResult { labels, .. }) = result {
        if !labels.is_empty() && result.is_success() {
            test = format!(
                "{test}\n{title}",
                title = "· with coverage".if_supports_color(Stderr, |s| s.bold())
            );
            let mut total = 0;
            let mut pad = 0;
            for (k, v) in labels {
                total += v;
                if k.len() > pad {
                    pad = k.len();
                }
            }

            let mut labels = labels.iter().collect::<Vec<_>>();
            labels.sort_by(|a, b| b.1.cmp(a.1));

            for (k, v) in labels {
                test = format!(
                    "{test}\n| {} {:>5.1}%",
                    pretty::pad_right(k.to_owned(), pad, " ")
                        .if_supports_color(Stderr, |s| s.bold()),
                    100.0 * (*v as f64) / (total as f64),
                );
            }
        }
    }

    // Traces
    if !result.traces().is_empty() {
        test = format!(
            "{test}\n{title}\n{traces}",
            title = "· with traces".if_supports_color(Stderr, |s| s.bold()),
            traces = result
                .traces()
                .iter()
                .map(|line| { format!("| {line}",) })
                .collect::<Vec<_>>()
                .join("\n")
        );
    };

    test
}

fn fmt_test_summary<T>(tests: &[&TestResult<T, T>], styled: bool) -> String {
    let (n_passed, n_failed) = tests.iter().fold((0, 0), |(n_passed, n_failed), result| {
        if result.is_success() {
            (n_passed + 1, n_failed)
        } else {
            (n_passed, n_failed + 1)
        }
    });
    format!(
        "{} | {} | {}",
        pretty::style_if(styled, format!("{} tests", tests.len()), |s| s
            .if_supports_color(Stderr, |s| s.bold())
            .to_string()),
        pretty::style_if(styled, format!("{n_passed} passed"), |s| s
            .if_supports_color(Stderr, |s| s.bright_green())
            .if_supports_color(Stderr, |s| s.bold())
            .to_string()),
        pretty::style_if(styled, format!("{n_failed} failed"), |s| s
            .if_supports_color(Stderr, |s| s.bright_red())
            .if_supports_color(Stderr, |s| s.bold())
            .to_string()),
    )
}
