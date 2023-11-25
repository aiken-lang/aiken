use crate::pretty;
use crate::script::EvalInfo;
use owo_colors::{
    OwoColorize,
    Stream::{self, Stderr},
};
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
    EvaluatingFunction {
        results: Vec<EvalInfo>,
    },
    RunningTests,
    FinishedTests {
        tests: Vec<EvalInfo>,
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
            Event::EvaluatingFunction { results } => {
                eprintln!(
                    "{}\n",
                    "  Evaluating function ..."
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.purple())
                );

                let (max_mem, max_cpu) = find_max_execution_units(&results);

                for eval_info in &results {
                    println!("    {}", fmt_eval(eval_info, max_mem, max_cpu, Stderr))
                }
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
            Event::FinishedTests { tests } => {
                let (max_mem, max_cpu) = find_max_execution_units(&tests);

                for (module, infos) in &group_by_module(&tests) {
                    let title = module
                        .if_supports_color(Stderr, |s| s.bold())
                        .if_supports_color(Stderr, |s| s.blue())
                        .to_string();

                    let tests = infos
                        .iter()
                        .map(|eval_info| fmt_test(eval_info, max_mem, max_cpu, true))
                        .collect::<Vec<String>>()
                        .join("\n");

                    let summary = fmt_test_summary(infos, true);

                    eprintln!(
                        "{}\n",
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

fn fmt_test(eval_info: &EvalInfo, max_mem: usize, max_cpu: usize, styled: bool) -> String {
    let EvalInfo {
        success,
        script,
        spent_budget,
        logs,
        ..
    } = eval_info;

    let ExBudget { mem, cpu } = spent_budget;
    let mem_pad = pretty::pad_left(mem.to_string(), max_mem, " ");
    let cpu_pad = pretty::pad_left(cpu.to_string(), max_cpu, " ");

    let test = format!(
        "{status} [mem: {mem_unit}, cpu: {cpu_unit}] {module}",
        status = if *success {
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
        },
        mem_unit = pretty::style_if(styled, mem_pad, |s| s
            .if_supports_color(Stderr, |s| s.cyan())
            .to_string()),
        cpu_unit = pretty::style_if(styled, cpu_pad, |s| s
            .if_supports_color(Stderr, |s| s.cyan())
            .to_string()),
        module = pretty::style_if(styled, script.name.clone(), |s| s
            .if_supports_color(Stderr, |s| s.bright_blue())
            .to_string()),
    );

    let logs = if logs.is_empty() {
        String::new()
    } else {
        logs.iter()
            .map(|line| {
                format!(
                    "{arrow} {styled_line}",
                    arrow = "↳".if_supports_color(Stderr, |s| s.bright_yellow()),
                    styled_line = line.if_supports_color(Stderr, |s| s.bright_black())
                )
            })
            .collect::<Vec<_>>()
            .join("\n")
    };

    if logs.is_empty() {
        test
    } else {
        [test, logs].join("\n")
    }
}

fn fmt_test_summary(tests: &Vec<&EvalInfo>, styled: bool) -> String {
    let (n_passed, n_failed) = tests
        .iter()
        .fold((0, 0), |(n_passed, n_failed), test_info| {
            if test_info.success {
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

fn fmt_eval(eval_info: &EvalInfo, max_mem: usize, max_cpu: usize, stream: Stream) -> String {
    let EvalInfo {
        output,
        script,
        spent_budget,
        ..
    } = eval_info;

    let ExBudget { mem, cpu } = spent_budget;

    format!(
        "    {}::{} [mem: {}, cpu: {}]\n    │\n    ╰─▶ {}",
        script.module.if_supports_color(stream, |s| s.blue()),
        script.name.if_supports_color(stream, |s| s.bright_blue()),
        pretty::pad_left(mem.to_string(), max_mem, " "),
        pretty::pad_left(cpu.to_string(), max_cpu, " "),
        output
            .as_ref()
            .map(|x| format!("{x}"))
            .unwrap_or_else(|| "Error.".to_string()),
    )
}

fn group_by_module(infos: &Vec<EvalInfo>) -> BTreeMap<String, Vec<&EvalInfo>> {
    let mut modules = BTreeMap::new();
    for eval_info in infos {
        let xs: &mut Vec<&EvalInfo> = modules.entry(eval_info.script.module.clone()).or_default();
        xs.push(eval_info);
    }
    modules
}

fn find_max_execution_units(xs: &[EvalInfo]) -> (usize, usize) {
    let (max_mem, max_cpu) = xs.iter().fold(
        (0, 0),
        |(max_mem, max_cpu), EvalInfo { spent_budget, .. }| {
            if spent_budget.mem >= max_mem && spent_budget.cpu >= max_cpu {
                (spent_budget.mem, spent_budget.cpu)
            } else if spent_budget.mem > max_mem {
                (spent_budget.mem, max_cpu)
            } else if spent_budget.cpu > max_cpu {
                (max_mem, spent_budget.cpu)
            } else {
                (max_mem, max_cpu)
            }
        },
    );

    (max_mem.to_string().len(), max_cpu.to_string().len())
}
