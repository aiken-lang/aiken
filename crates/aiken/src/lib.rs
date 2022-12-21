use aiken_project::{pretty, script::EvalInfo, telemetry, Project};
use miette::IntoDiagnostic;
use owo_colors::OwoColorize;
use std::{collections::BTreeMap, env, path::PathBuf, process};
use uplc::machine::cost_model::ExBudget;

pub mod cmd;

pub fn with_project<A>(directory: Option<PathBuf>, mut action: A) -> miette::Result<()>
where
    A: FnMut(&mut Project<Terminal>) -> Result<(), aiken_project::error::Error>,
{
    let project_path = if let Some(d) = directory {
        d
    } else {
        env::current_dir().into_diagnostic()?
    };

    let mut project = match Project::new(project_path, Terminal::default()) {
        Ok(p) => p,
        Err(e) => {
            e.report();
            process::exit(1);
        }
    };

    let build_result = action(&mut project);

    let warning_count = project.warnings.len();

    for warning in project.warnings {
        warning.report()
    }

    if let Err(err) = build_result {
        err.report();
        println!("\n{}", "Summary".purple().bold());
        println!(
            "    {} error(s), {}",
            err.len(),
            format!("{warning_count} warning(s)").yellow(),
        );
        process::exit(1);
    } else {
        println!("\n{}", "Summary".purple().bold());
        println!(
            "    0 error, {}",
            format!("{warning_count} warning(s)").yellow(),
        );
    }
    Ok(())
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Terminal;

impl telemetry::EventListener for Terminal {
    fn handle_event(&self, event: telemetry::Event) {
        match event {
            telemetry::Event::StartingCompilation {
                name,
                version,
                root,
            } => {
                println!(
                    "{} {} {} ({})",
                    "    Compiling".bold().purple(),
                    name.bold(),
                    version,
                    root.display().bright_blue()
                );
            }
            telemetry::Event::BuildingDocumentation {
                name,
                version,
                root,
            } => {
                println!(
                    "{} {} {} ({})",
                    "   Generating documentation".bold().purple(),
                    name.bold(),
                    version,
                    root.to_str().unwrap_or("").bright_blue()
                );
            }
            telemetry::Event::WaitingForBuildDirLock => {
                println!("{}", "Waiting for build directory lock ...".bold().purple());
            }
            telemetry::Event::GeneratingUPLC { output_path, name } => {
                println!(
                    "{} {} in {}",
                    "   Generating".bold().purple(),
                    name.bold(),
                    output_path.display().bright_blue()
                );
            }
            telemetry::Event::GeneratingDocFiles { output_path } => {
                println!(
                    "{} in {}",
                    "   Generating documentation files".bold().purple(),
                    output_path.to_str().unwrap_or("").bright_blue()
                );
            }
            telemetry::Event::GeneratingUPLCFor { name, path } => {
                println!(
                    "{} {}.{{{}}}",
                    "   Generating Untyped Plutus Core for".bold().purple(),
                    path.to_str().unwrap_or("").blue(),
                    name.bright_blue(),
                );
            }
            telemetry::Event::EvaluatingFunction { results } => {
                println!("{}\n", "  Evaluating function ...".bold().purple());

                let (max_mem, max_cpu) = find_max_execution_units(&results);

                for eval_info in &results {
                    println!("    {}", fmt_eval(eval_info, max_mem, max_cpu))
                }
            }
            telemetry::Event::RunningTests => {
                println!("{} {}\n", "      Testing".bold().purple(), "...".bold());
            }
            telemetry::Event::FinishedTests { tests } => {
                let (max_mem, max_cpu) = find_max_execution_units(&tests);

                for (module, infos) in &group_by_module(&tests) {
                    let title = module.bold().blue().to_string();

                    let tests = infos
                        .iter()
                        .map(|eval_info| fmt_test(eval_info, max_mem, max_cpu, true))
                        .collect::<Vec<String>>()
                        .join("\n");

                    let summary = fmt_test_summary(infos, true);

                    println!(
                        "{}\n",
                        pretty::indent(
                            &pretty::open_box(&title, &tests, &summary, |border| border
                                .bright_black()
                                .to_string()),
                            4
                        )
                    );
                }
            }
            telemetry::Event::DownloadingPackage { name } => {
                println!("{} {}", "  Downloading".bold().purple(), name.bold())
            }
            telemetry::Event::PackagesDownloaded { start, count } => {
                let elapsed = format!("{:.2}s", start.elapsed().as_millis() as f32 / 1000.);

                let msg = match count {
                    1 => format!("1 package in {}", elapsed),
                    _ => format!("{} packages in {}", count, elapsed),
                };

                println!("{} {}", "   Downloaded".bold().purple(), msg.bold())
            }
            telemetry::Event::ResolvingVersions => {
                println!("{}", "    Resolving versions".bold().purple(),)
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
            pretty::style_if(styled, "PASS".to_string(), |s| s.bold().green().to_string())
        } else {
            pretty::style_if(styled, "FAIL".to_string(), |s| s.bold().red().to_string())
        },
        mem_unit = pretty::style_if(styled, mem_pad, |s| s.bright_white().to_string()),
        cpu_unit = pretty::style_if(styled, cpu_pad, |s| s.bright_white().to_string()),
        module = pretty::style_if(styled, script.name.clone(), |s| s.bright_blue().to_string()),
    );

    let logs = if logs.is_empty() {
        String::new()
    } else {
        logs.iter()
            .map(|line| {
                format!(
                    "{arrow} {styled_line}",
                    arrow = "↳".bright_yellow(),
                    styled_line = line.bright_black()
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
            .bold()
            .to_string()),
        pretty::style_if(styled, format!("{} passed", n_passed), |s| s
            .bright_green()
            .bold()
            .to_string()),
        pretty::style_if(styled, format!("{} failed", n_failed), |s| s
            .bright_red()
            .bold()
            .to_string()),
    )
}

fn fmt_eval(eval_info: &EvalInfo, max_mem: usize, max_cpu: usize) -> String {
    let EvalInfo {
        output,
        script,
        spent_budget,
        ..
    } = eval_info;

    let ExBudget { mem, cpu } = spent_budget;

    format!(
        "    {}::{} [mem: {}, cpu: {}]\n    │\n    ╰─▶ {}",
        script.module.blue(),
        script.name.bright_blue(),
        pretty::pad_left(mem.to_string(), max_mem, " "),
        pretty::pad_left(cpu.to_string(), max_cpu, " "),
        output
            .as_ref()
            .map(|x| format!("{}", x))
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
