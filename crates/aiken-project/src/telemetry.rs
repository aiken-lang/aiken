use crate::{
    pretty,
    test_framework::{PropertyTestResult, TestResult, UnitTestResult},
};
use aiken_lang::{expr::UntypedExpr, format::Formatter};
use owo_colors::{OwoColorize, Stream::Stderr};
use std::{collections::BTreeMap, fmt::Display, path::PathBuf};
use uplc::machine::cost_model::ExBudget;

pub trait EventListener {
    fn handle_event(&self, _event: Event) {}
}

pub enum Event {
    StartingCompilation {
        name: String,
        version: String,
        root: PathBuf,
    },
    BuildingDocumentation {
        name: String,
        version: String,
        root: PathBuf,
    },
    GeneratingDocFiles {
        output_path: PathBuf,
    },
    GeneratingBlueprint {
        path: PathBuf,
    },
    DumpingUPLC {
        path: PathBuf,
    },
    GeneratingUPLCFor {
        name: String,
        path: PathBuf,
    },
    RunningTests,
    FinishedTests {
        seed: u32,
        tests: Vec<TestResult<UntypedExpr>>,
    },
    WaitingForBuildDirLock,
    ResolvingPackages {
        name: String,
    },
    PackageResolveFallback {
        name: String,
    },
    PackagesDownloaded {
        start: tokio::time::Instant,
        count: usize,
        source: DownloadSource,
    },
    ResolvingVersions,
}

pub enum DownloadSource {
    Network,
    Cache,
}

impl Display for DownloadSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DownloadSource::Network => write!(f, "network"),
            DownloadSource::Cache => write!(f, "cache"),
        }
    }
}

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
                    "{} {} {} ({})",
                    "   Generating documentation"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
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
                    "{} in {}",
                    "   Generating documentation files"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    output_path
                        .to_str()
                        .unwrap_or("")
                        .if_supports_color(Stderr, |s| s.bright_blue())
                );
            }
            Event::GeneratingUPLCFor { name, path } => {
                eprintln!(
                    "{} {}.{{{}}}",
                    "   Generating UPLC for"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                    path.to_str()
                        .unwrap_or("")
                        .if_supports_color(Stderr, |s| s.blue()),
                    name.if_supports_color(Stderr, |s| s.bright_blue()),
                );
            }
            Event::RunningTests => {
                eprintln!(
                    "{} {}\n",
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

                    let summary = format!("{}{}", seed_info, fmt_test_summary(results, true));
                    println!(
                        "{}",
                        pretty::indent(
                            &pretty::open_box(&title, &tests, &summary, |border| border
                                .if_supports_color(Stderr, |s| s.bright_black())
                                .to_string()),
                            4
                        )
                    );
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
                    "{}",
                    "    Resolving dependencies"
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple()),
                )
            }
        }
    }
}

fn fmt_test(
    result: &TestResult<UntypedExpr>,
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
            test = pretty::pad_right(
                format!(
                    "{test} [after {} test{}]",
                    pretty::pad_left(iterations.to_string(), max_iter, " "),
                    if *iterations > 1 { "s" } else { "" }
                ),
                18 + max_mem + max_cpu + max_iter,
                " ",
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

    // CounterExample
    if let TestResult::PropertyTestResult(PropertyTestResult {
        counterexample: Some(counterexample),
        ..
    }) = result
    {
        test = format!(
            "{test}\n{}\n{}\n",
            "× counterexample"
                .if_supports_color(Stderr, |s| s.red())
                .if_supports_color(Stderr, |s| s.bold()),
            &Formatter::new()
                .expr(counterexample, false)
                .to_pretty_string(60)
                .lines()
                .map(|line| {
                    format!("{} {}", "│".if_supports_color(Stderr, |s| s.red()), line)
                })
                .collect::<Vec<String>>()
                .join("\n")
        );
    }

    // Traces
    if !result.logs().is_empty() {
        test = format!(
            "{test}\n{logs}",
            logs = result
                .logs()
                .iter()
                .map(|line| {
                    format!(
                        "{arrow} {styled_line}",
                        arrow = "↳".if_supports_color(Stderr, |s| s.bright_yellow()),
                        styled_line = line
                            .split('\n')
                            .map(|l| format!(
                                "{}",
                                l.if_supports_color(Stderr, |s| s.bright_black())
                            ))
                            .collect::<Vec<_>>()
                            .join("\n")
                    )
                })
                .collect::<Vec<_>>()
                .join("\n")
        );
    };

    test
}

fn fmt_test_summary<T>(tests: &[&TestResult<T>], styled: bool) -> String {
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

fn group_by_module<T>(results: &Vec<TestResult<T>>) -> BTreeMap<String, Vec<&TestResult<T>>> {
    let mut modules = BTreeMap::new();
    for r in results {
        let xs: &mut Vec<&TestResult<_>> = modules.entry(r.module().to_string()).or_default();
        xs.push(r);
    }
    modules
}

fn find_max_execution_units<T>(xs: &[TestResult<T>]) -> (usize, usize, usize) {
    let (max_mem, max_cpu, max_iter) =
        xs.iter()
            .fold((0, 0, 0), |(max_mem, max_cpu, max_iter), test| match test {
                TestResult::PropertyTestResult(PropertyTestResult { iterations, .. }) => {
                    (max_mem, max_cpu, std::cmp::max(max_iter, *iterations))
                }
                TestResult::UnitTestResult(UnitTestResult { spent_budget, .. }) => {
                    if spent_budget.mem >= max_mem && spent_budget.cpu >= max_cpu {
                        (spent_budget.mem, spent_budget.cpu, max_iter)
                    } else if spent_budget.mem > max_mem {
                        (spent_budget.mem, max_cpu, max_iter)
                    } else if spent_budget.cpu > max_cpu {
                        (max_mem, spent_budget.cpu, max_iter)
                    } else {
                        (max_mem, max_cpu, max_iter)
                    }
                }
            });

    (
        max_mem.to_string().len(),
        max_cpu.to_string().len(),
        max_iter.to_string().len(),
    )
}
