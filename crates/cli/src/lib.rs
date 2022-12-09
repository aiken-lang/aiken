use std::{env, path::PathBuf};

use aiken_project::{
    config::Config,
    telemetry::{self, TestInfo},
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
            telemetry::Event::CompilingPackage { .. } => todo!(),
            telemetry::Event::RunningTests => {
                println!("\n{}\n", "Running tests...".bold().underline().purple());
            }
            telemetry::Event::FinishedTests { tests } => {
                let (max_mem, max_cpu) = tests.iter().fold(
                    (0, 0),
                    |(max_mem, max_cpu), TestInfo { spent_budget, .. }| {
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

                let max_mem = max_mem.to_string().len() as i32;
                let max_cpu = max_cpu.to_string().len() as i32;

                for test_info in &tests {
                    println!("{}", fmt_test(test_info, max_mem, max_cpu))
                }

                let (n_passed, n_failed) =
                    tests
                        .iter()
                        .fold((0, 0), |(n_passed, n_failed), test_info| {
                            if test_info.is_passing {
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

fn fmt_test(test_info: &TestInfo, max_mem: i32, max_cpu: i32) -> String {
    let TestInfo {
        is_passing,
        test,
        spent_budget,
    } = test_info;

    let ExBudget { mem, cpu } = spent_budget;

    format!(
        "    [{}] [mem: {}, cpu: {}] {}::{}",
        if *is_passing {
            "PASS".bold().green().to_string()
        } else {
            "FAIL".bold().red().to_string()
        },
        pad_left(mem.to_string(), max_mem, " "),
        pad_left(cpu.to_string(), max_cpu, " "),
        test.module.blue(),
        test.name.bright_blue()
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
