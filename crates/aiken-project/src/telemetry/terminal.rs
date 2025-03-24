use super::{DownloadSource, Event, EventListener, find_max_execution_units, group_by_module};
use crate::pretty;
use aiken_lang::{
    ast::OnTestFailure,
    expr::UntypedExpr,
    format::Formatter,
    test_framework::{
        AssertionStyleOptions, BenchmarkResult, PropertyTestResult, TestResult, UnitTestResult,
    },
};
use owo_colors::{OwoColorize, Stream::Stderr};
use rgb::RGB8;
use std::sync::LazyLock;
use uplc::machine::cost_model::ExBudget;

static BENCH_PLOT_COLOR: LazyLock<RGB8> = LazyLock::new(|| RGB8 {
    r: 250,
    g: 211,
    b: 144,
});

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
            Event::CollectingTests {
                matching_module,
                matching_names,
            } => {
                eprintln!(
                    "{:>13} {tests} {module}",
                    "Collecting"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    tests = if matching_names.is_empty() {
                        if matching_module.is_some() {
                            "all tests scenarios"
                                .if_supports_color(Stderr, |s| s.bold())
                                .to_string()
                        } else {
                            "all tests scenarios".to_string()
                        }
                    } else {
                        format!(
                            "test{} {}",
                            if matching_names.len() > 1 { "s" } else { "" },
                            matching_names
                                .iter()
                                .map(|s| format!("*{s}*"))
                                .collect::<Vec<_>>()
                                .join(", ")
                                .if_supports_color(Stderr, |s| s.bold())
                        )
                    },
                    module = match matching_module {
                        None => format!(
                            "across {}",
                            if matching_names.is_empty() {
                                "all modules".to_string()
                            } else {
                                "all modules"
                                    .if_supports_color(Stderr, |s| s.bold())
                                    .to_string()
                            }
                        ),
                        Some(module) => format!(
                            "within module(s): {}",
                            format!("*{module}*").if_supports_color(Stderr, |s| s.bold())
                        ),
                    }
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
            Event::RunningBenchmarks => {
                eprintln!(
                    "{} {}",
                    " Benchmarking"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    "...".if_supports_color(Stderr, |s| s.bold())
                );
            }
            Event::FinishedBenchmarks { seed, benchmarks } => {
                let (max_mem, max_cpu, max_iter) = find_max_execution_units(&benchmarks);

                for (module, results) in &group_by_module(&benchmarks) {
                    let title = module
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.blue())
                        .to_string();

                    let benchmarks = results
                        .iter()
                        .map(|r| fmt_test(r, max_mem, max_cpu, max_iter, true))
                        .collect::<Vec<String>>()
                        .join("\n")
                        .chars()
                        .skip(1) // Remove extra first newline
                        .collect::<String>();

                    let seed_info = format!(
                        "with {opt}={seed}",
                        opt = "--seed".if_supports_color(Stderr, |s| s.bold()),
                        seed = format!("{seed}").if_supports_color(Stderr, |s| s.bold())
                    );

                    if !benchmarks.is_empty() {
                        println!();
                    }

                    println!(
                        "{}\n",
                        pretty::indent(
                            &pretty::open_box(&title, &benchmarks, &seed_info, |border| border
                                .if_supports_color(Stderr, |s| s.bright_black())
                                .to_string()),
                            4
                        )
                    );
                }

                if !benchmarks.is_empty() {
                    println!();
                }
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
    let mut test = if matches!(result, TestResult::BenchmarkResult { .. }) {
        format!(
            "\n{label}{title}\n",
            label = if result.is_success() {
                String::new()
            } else {
                pretty::style_if(styled, "FAIL ".to_string(), |s| {
                    s.if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.red())
                        .to_string()
                })
            },
            title = pretty::style_if(styled, result.title().to_string(), |s| s
                .if_supports_color(Stderr, |s| s.bright_blue())
                .to_string())
        )
    } else if result.is_success() {
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
        TestResult::BenchmarkResult(BenchmarkResult { error: Some(e), .. }) => {
            test = format!(
                "{test}{}",
                e.to_string().if_supports_color(Stderr, |s| s.red())
            );
        }
        TestResult::BenchmarkResult(BenchmarkResult {
            measures,
            error: None,
            ..
        }) => {
            let max_size = measures
                .iter()
                .map(|(size, _)| *size)
                .max()
                .unwrap_or_default();

            let mem_chart = format!(
                "{title}\n{chart}",
                title = "memory units"
                    .if_supports_color(Stderr, |s| s.yellow())
                    .if_supports_color(Stderr, |s| s.bold()),
                chart = plot(
                    &BENCH_PLOT_COLOR,
                    measures
                        .iter()
                        .map(|(size, budget)| (*size as f32, budget.mem as f32))
                        .collect::<Vec<_>>(),
                    max_size
                )
            );

            let cpu_chart = format!(
                "{title}\n{chart}",
                title = "cpu units"
                    .if_supports_color(Stderr, |s| s.yellow())
                    .if_supports_color(Stderr, |s| s.bold()),
                chart = plot(
                    &BENCH_PLOT_COLOR,
                    measures
                        .iter()
                        .map(|(size, budget)| (*size as f32, budget.cpu as f32))
                        .collect::<Vec<_>>(),
                    max_size
                )
            );

            let charts = mem_chart
                .lines()
                .zip(cpu_chart.lines())
                .map(|(l, r)| format!("  {}{r}", pretty::pad_right(l.to_string(), 55, " ")))
                .collect::<Vec<_>>()
                .join("\n");

            test = format!("{test}{charts}",);
        }
    }

    // Title
    test = match result {
        TestResult::BenchmarkResult(..) => test,
        TestResult::UnitTestResult(..) | TestResult::PropertyTestResult(..) => {
            format!(
                "{test} {title}",
                title = pretty::style_if(styled, result.title().to_string(), |s| s
                    .if_supports_color(Stderr, |s| s.bright_blue())
                    .to_string())
            )
        }
    };

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
    if !result.logs().is_empty() {
        test = format!(
            "{test}\n{title}\n{traces}",
            title = "· with traces".if_supports_color(Stderr, |s| s.bold()),
            traces = result
                .logs()
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

fn plot(color: &RGB8, points: Vec<(f32, f32)>, max_size: usize) -> String {
    use textplots::{Chart, ColorPlot, Shape};
    let mut chart = Chart::new(80, 50, 1.0, max_size as f32);
    let plot = Shape::Lines(&points);
    let chart = chart.linecolorplot(&plot, *color);
    chart.borders();
    chart.axis();
    chart.figures();
    chart.to_string()
}
