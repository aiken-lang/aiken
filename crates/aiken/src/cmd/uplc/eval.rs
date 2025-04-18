use miette::IntoDiagnostic;
use pallas_primitives::conway::Language;
use serde_json::json;
use std::{path::PathBuf, process};

use strum::IntoEnumIterator;

use uplc::{
    ast::{FakeNamedDeBruijn, Name, NamedDeBruijn, Program, Term},
    builtins::DefaultFunction,
    machine::{
        TERM_COUNT,
        cost_model::{ExBudget, StepKind},
    },
    parser,
};

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
/// Evaluate an Untyped Plutus Core program
pub struct Args {
    script: PathBuf,

    #[clap(short, long)]
    flat: bool,

    #[clap(short, long)]
    cbor: bool,

    #[clap(short, long, default_value_t = false)]
    debug: bool,

    /// Arguments to pass to the UPLC program
    args: Vec<String>,
}

pub fn exec(
    Args {
        script,
        flat,
        args,
        debug,
        cbor,
    }: Args,
) -> miette::Result<()> {
    let mut program: Program<Name> = if cbor {
        let cbor_hex = std::fs::read_to_string(&script).into_diagnostic()?;

        let raw_cbor = hex::decode(cbor_hex.trim()).into_diagnostic()?;

        let program = Program::<FakeNamedDeBruijn>::from_cbor(&raw_cbor, &mut Vec::new())
            .into_diagnostic()?;

        let program: Program<NamedDeBruijn> = program.into();

        Program::<Name>::try_from(program).into_diagnostic()?
    } else if flat {
        let bytes = std::fs::read(&script).into_diagnostic()?;

        let program = Program::<FakeNamedDeBruijn>::from_flat(&bytes).into_diagnostic()?;

        let program: Program<NamedDeBruijn> = program.into();

        Program::<Name>::try_from(program).into_diagnostic()?
    } else {
        let code = std::fs::read_to_string(&script).into_diagnostic()?;

        parser::program(&code).into_diagnostic()?
    };

    for arg in args {
        let term = parser::term(&arg).into_diagnostic()?;

        program = program.apply_term(&term)
    }

    let budget = ExBudget::default();

    let program = Program::<NamedDeBruijn>::try_from(program).into_diagnostic()?;

    let eval_result = if debug {
        program.eval_debug(ExBudget::default(), &Language::PlutusV3)
    } else {
        program.eval(budget)
    };

    let cost = eval_result.cost();
    let logs = eval_result.logs();

    match eval_result.result() {
        Ok(term) => {
            let term = Term::<Name>::try_from(term).into_diagnostic()?;

            let output = json!({
                "result": term.to_pretty(),
                "cpu": cost.cpu,
                "mem": cost.mem,
            });

            println!(
                "{}",
                serde_json::to_string_pretty(&output).into_diagnostic()?
            );

            if debug {
                println!("---------------DEBUG------------------");
                let costs = eval_result.debug_cost().unwrap();

                let mut output = json!([]);
                for step in StepKind::iter() {
                    if matches!(step, StepKind::StartUp) {
                        continue;
                    }
                    let i = step as usize * 2;

                    if costs[i + 1] != 0 || costs[i] != 0 {
                        output.as_array_mut().unwrap().push(json!({
                            "step": step.to_string(),
                            "cpu": costs[i+1],
                            "mem": costs[i],
                        }));
                    }
                }

                for fun in DefaultFunction::iter() {
                    let i = (fun as usize + TERM_COUNT) * 2;
                    if costs[i + 1] != 0 || costs[i] != 0 {
                        output.as_array_mut().unwrap().push(json!({
                            "fun": fun.to_string(),
                            "cpu": costs[i+1],
                            "mem": costs[i],
                        }));
                    }
                }

                println!(
                    "{}",
                    serde_json::to_string_pretty(&output).into_diagnostic()?
                );
            }

            Ok(())
        }
        Err(err) => {
            eprintln!("\nError\n-----\n\n{err}\n");

            eprintln!("\nCosts\n-----\ncpu: {}\nmemory: {}", cost.cpu, cost.mem);

            if !logs.is_empty() {
                eprintln!("\nLogs\n----\n{}", logs.join("\n"))
            }

            process::exit(1)
        }
    }
}
