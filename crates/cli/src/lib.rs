use std::{env, path::PathBuf};

use aiken_project::{
    config::Config,
    telemetry::{self, EvalInfo},
    Project,
};
use miette::IntoDiagnostic;
use owo_colors::OwoColorize;
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

    let config = Config::load(project_path.clone()).into_diagnostic()?;

    let mut project = Project::new(config, project_path, Terminal::default());

    let build_result = action(&mut project);

    let warning_count = project.warnings.len();

    for warning in project.warnings {
        warning.report()
    }

    if let Err(err) = build_result {
        err.report();

        miette::bail!("Failed: {} error(s), {warning_count} warning(s)", err.len(),);
    };

    println!("\nFinished with {warning_count} warning(s)\n");

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
                    "Compiling".bold().purple(),
                    name.bold(),
                    version,
                    root.to_str().unwrap_or("").bright_blue()
                );
            }
            telemetry::Event::ParsingProjectFiles => {
                println!("{}", "...Parsing project files".bold().purple());
            }
            telemetry::Event::TypeChecking => {
                println!("{}", "...Type-checking project".bold().purple());
            }
            telemetry::Event::GeneratingUPLC { output_path } => {
                println!(
                    "{} in {}",
                    "...Generating Untyped Plutus Core".bold().purple(),
                    output_path.to_str().unwrap_or("").bright_blue()
                );
            }
            telemetry::Event::EvaluatingFunction { results } => {
                println!("{}\n", "...Evaluating function".bold().purple());

                let (max_mem, max_cpu) = find_max_execution_units(&results);

                for eval_info in &results {
                    println!("{}", fmt_eval(eval_info, max_mem, max_cpu))
                }
            }
            telemetry::Event::RunningTests => {
                println!("{}\n", "...Running tests".bold().purple());
            }
            telemetry::Event::FinishedTests { tests } => {
                let (max_mem, max_cpu) = find_max_execution_units(&tests);

                for eval_info in &tests {
                    println!("{}", fmt_test(eval_info, max_mem, max_cpu))
                }

                let (n_passed, n_failed) =
                    tests
                        .iter()
                        .fold((0, 0), |(n_passed, n_failed), test_info| {
                            if test_info.success {
                                (n_passed + 1, n_failed)
                            } else {
                                (n_passed, n_failed + 1)
                            }
                        });

                println!(
                    "{}",
                    format!(
                        "\n    Summary: {} test(s), {}; {}.",
                        tests.len(),
                        format!("{} passed", n_passed).bright_green(),
                        format!("{} failed", n_failed).bright_red()
                    )
                    .bold()
                )
            }
        }
    }
}

fn fmt_test(eval_info: &EvalInfo, max_mem: i32, max_cpu: i32) -> String {
    let EvalInfo {
        success,
        script,
        spent_budget,
        ..
    } = eval_info;

    let ExBudget { mem, cpu } = spent_budget;

    format!(
        "    [{}] [mem: {}, cpu: {}] {}::{}",
        if *success {
            "PASS".bold().green().to_string()
        } else {
            "FAIL".bold().red().to_string()
        },
        pad_left(mem.to_string(), max_mem, " "),
        pad_left(cpu.to_string(), max_cpu, " "),
        script.module.blue(),
        script.name.bright_blue()
    )
}

fn fmt_eval(eval_info: &EvalInfo, max_mem: i32, max_cpu: i32) -> String {
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
        pad_left(mem.to_string(), max_mem, " "),
        pad_left(cpu.to_string(), max_cpu, " "),
        output
            .as_ref()
            .map(|x| format!("{}", x))
            .unwrap_or_else(|| "Error.".to_string()),
    )
}

fn find_max_execution_units(xs: &[EvalInfo]) -> (i32, i32) {
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

    (
        max_mem.to_string().len() as i32,
        max_cpu.to_string().len() as i32,
    )
}

fn pad_left(mut text: String, n: i32, delimiter: &str) -> String {
    let diff = n - text.len() as i32;

    if diff.is_positive() {
        for _ in 0..diff {
            text.insert_str(0, delimiter);
        }
    }

    text
}
