use aiken_project::{
    Project,
    error::{Error, ScriptOverrideArgumentError},
    options::Options,
    telemetry::{EventTarget, Terminal},
    watch::{ExitFailure, workspace_root},
};
use hex::FromHexError;
use miette::IntoDiagnostic;
use owo_colors::{OwoColorize, Stream::Stderr};
use pallas_addresses::ScriptHash;
use pallas_primitives::{
    Fragment,
    conway::{Redeemer, TransactionInput, TransactionOutput},
};
use pallas_traverse::{Era, MultiEraTx};
use serde_json::json;
use std::{collections::HashMap, fmt, fs, path::PathBuf, process};
use uplc::{
    machine::Trace,
    tx::{
        self, redeemer_tag_to_string,
        script_context::{PlutusScript, ResolvedInput, SlotConfig},
    },
};

#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
/// Simulate a transaction by evaluating it's script
pub struct Args {
    /// A file containing cbor hex for a transaction
    #[clap(value_name = "FILEPATH")]
    input: PathBuf,

    /// Toggle whether input is raw cbor or a hex string
    #[clap(short, long)]
    cbor: bool,

    /// A file containing cbor hex for the raw inputs
    #[clap(value_name = "FILEPATH")]
    raw_inputs: PathBuf,

    /// A file containing cbor hex for the raw outputs
    #[clap(value_name = "FILEPATH")]
    raw_outputs: PathBuf,

    /// Time between each slot
    #[clap(short, long, default_value_t = 1000, value_name = "MILLISECOND")]
    slot_length: u32,

    /// Time of shelley hardfork
    #[clap(long, default_value_t = 1596059091000, value_name = "POSIX")]
    zero_time: u64,

    /// Slot number at the start of the shelley hardfork
    #[clap(long, default_value_t = 4492800, value_name = "SLOT")]
    zero_slot: u64,

    /// An Aiken blueprint JSON file containing the overriding scripts, if applicable
    #[clap(long, value_name = "FILEPATH")]
    blueprint: Option<PathBuf>,

    /// A mapping (colon-separated) from a script hash (in the transaction) to override by another script found in the blueprint.
    /// For example:`d27cee75:197c9353`
    #[clap(long("script-override"), value_name = "FROM:TO", num_args(0..), verbatim_doc_comment)]
    script_overrides: Vec<String>,
}

pub fn exec(
    Args {
        input,
        cbor,
        raw_inputs,
        raw_outputs,
        slot_length,
        zero_time,
        zero_slot,
        blueprint,
        script_overrides,
    }: Args,
) -> miette::Result<()> {
    eprintln!(
        "{} script context",
        "      Parsing"
            .if_supports_color(Stderr, |s| s.purple())
            .if_supports_color(Stderr, |s| s.bold())
    );

    let (tx_bytes, inputs_bytes, outputs_bytes) = if cbor {
        (
            fs::read(input).into_diagnostic()?,
            fs::read(raw_inputs).into_diagnostic()?,
            fs::read(raw_outputs).into_diagnostic()?,
        )
    } else {
        let cbor_hex = fs::read_to_string(input).into_diagnostic()?;
        let inputs_hex = fs::read_to_string(raw_inputs).into_diagnostic()?;
        let outputs_hex = fs::read_to_string(raw_outputs).into_diagnostic()?;

        (
            hex::decode(cbor_hex.trim()).into_diagnostic()?,
            hex::decode(inputs_hex.trim()).into_diagnostic()?,
            hex::decode(outputs_hex.trim()).into_diagnostic()?,
        )
    };

    let tx = MultiEraTx::decode_for_era(Era::Conway, &tx_bytes).into_diagnostic()?;

    let mut overrides: HashMap<ScriptHash, PlutusScript> = HashMap::new();

    let blueprint_path = blueprint.map(Ok::<_, miette::Error>).unwrap_or_else(|| {
        let root = workspace_root(None)?;
        Ok(root.join(Options::default().blueprint_path))
    })?;

    if !script_overrides.is_empty() {
        eprintln!(
            "{:>13} scripts",
            "Overriding"
                .if_supports_color(Stderr, |s| s.purple())
                .if_supports_color(Stderr, |s| s.bold()),
        );

        let blueprint = Project::<Terminal>::blueprint(&blueprint_path).map_err(|e| {
            e.report();
            ExitFailure::into_report()
        })?;

        let blueprint_validators: HashMap<ScriptHash, PlutusScript> = blueprint.into();

        script_overrides
            .iter()
            .enumerate()
            .try_for_each::<_, Result<_, aiken_project::error::Error>>(
                |(index, script_override)| {
                    let mut parts = script_override.split(":");

                    let from =
                        get_override_part(&mut parts, ScriptOverrideArgumentError::MissingFrom)
                            .and_then(|part| {
                                decode_script_hash(
                                    part,
                                    ScriptOverrideArgumentError::InvalidFromHash,
                                    ScriptOverrideArgumentError::InvalidFromSize,
                                )
                            })
                            .map_err(|error| Error::ScriptOverrideArgumentParseError {
                                index,
                                error,
                            })?;

                    let to = get_override_part(&mut parts, ScriptOverrideArgumentError::MissingTo)
                        .and_then(|part| {
                            decode_script_hash(
                                part,
                                ScriptOverrideArgumentError::InvalidToHash,
                                ScriptOverrideArgumentError::InvalidToSize,
                            )
                        })
                        .map_err(|error| Error::ScriptOverrideArgumentParseError {
                            index,
                            error,
                        })?;

                    overrides.insert(
                        from,
                        blueprint_validators
                            .get(&to)
                            .ok_or_else(|| Error::ScriptOverrideNotFound {
                                script_hash: to,
                                known_scripts: blueprint_validators.keys().cloned().collect(),
                            })?
                            .clone(),
                    );

                    Ok(())
                },
            )
            .map_err(|e| {
                e.report();
                ExitFailure::into_report()
            })?;
    }

    eprintln!(
        "{:>13} {}",
        "Simulating"
            .if_supports_color(Stderr, |s| s.purple())
            .if_supports_color(Stderr, |s| s.bold()),
        tx.hash()
    );

    let inputs = Vec::<TransactionInput>::decode_fragment(&inputs_bytes).unwrap();
    let outputs = Vec::<TransactionOutput>::decode_fragment(&outputs_bytes).unwrap();

    let resolved_inputs: Vec<ResolvedInput> = inputs
        .iter()
        .zip(outputs.iter())
        .map(|(input, output)| ResolvedInput {
            input: input.clone(),
            output: output.clone(),
        })
        .collect();

    if let Some(tx_conway) = tx.as_conway() {
        let slot_config = SlotConfig {
            zero_time,
            zero_slot,
            slot_length,
        };

        let with_redeemer = |redeemer: &Redeemer| {
            eprintln!(
                "{:>13} {}[{}]",
                "Evaluating"
                    .if_supports_color(Stderr, |s| s.purple())
                    .if_supports_color(Stderr, |s| s.bold()),
                redeemer_tag_to_string(&redeemer.tag),
                redeemer.index
            )
        };

        let result = tx::eval_phase_two_with_override(
            tx_conway,
            &resolved_inputs,
            None,
            None,
            &slot_config,
            overrides,
            true,
            with_redeemer,
        );

        match result {
            Ok(redeemers) => {
                let is_terminal = matches!(EventTarget::default(), EventTarget::Terminal(..));

                if is_terminal {
                    redeemers.iter().for_each(|(redeemer, eval)| {
                        eprintln!(
                            "{} {}={}\n{:>6}{:>11}={}",
                            format!(
                                "{:>13}",
                                format!(
                                    "{}[{}]",
                                    redeemer_tag_to_string(&redeemer.tag),
                                    redeemer.index
                                )
                            )
                            .if_supports_color(Stderr, |s| s.purple())
                            .if_supports_color(Stderr, |s| s.bold()),
                            "mem".if_supports_color(Stderr, |s| s.bold()),
                            redeemer.ex_units.mem,
                            "│"
                                .if_supports_color(Stderr, |s| s.purple())
                                .if_supports_color(Stderr, |s| s.bold()),
                            "cpu".if_supports_color(Stderr, |s| s.bold()),
                            redeemer.ex_units.steps,
                        );

                        let traces = eval.traces();

                        eprintln!(
                            "{:>13} {}",
                            "└ Traces"
                                .if_supports_color(Stderr, |s| s.purple())
                                .if_supports_color(Stderr, |s| s.bold()),
                            if traces.is_empty() {
                                "ø".to_string()
                            } else {
                                traces
                                    .into_iter()
                                    .filter_map(|trace| match trace {
                                        Trace::Log(s) => Some(s),
                                        Trace::Label(_) => None,
                                    })
                                    .collect::<Vec<_>>()
                                    .join("\n              ")
                            }
                        );
                    });
                } else {
                    // this should allow N scripts to be
                    let summary: Vec<serde_json::Value> = redeemers
                        .iter()
                        .map(|(redeemer, eval)| {
                            json!({
                                "mem": redeemer.ex_units.mem as i64,
                                "cpu": redeemer.ex_units.steps as i64,
                                "traces": eval.traces().into_iter().filter_map(|trace| match trace {
                                    Trace::Log(s) => Some(s),
                                    Trace::Label(_) => None,
                                }).collect::<Vec<_>>(),
                            })
                        })
                        .collect();

                    println!(
                        "{}",
                        serde_json::to_string_pretty(&summary)
                            .map_err(|_| fmt::Error)
                            .into_diagnostic()?
                    );
                }
            }
            Err(err) => {
                eprintln!(
                    "{:>13} {}",
                    "Error"
                        .if_supports_color(Stderr, |s| s.red())
                        .if_supports_color(Stderr, |s| s.bold()),
                    err.red()
                );

                process::exit(1);
            }
        }
    }

    Ok(())
}

fn get_override_part<'a>(
    parts: &'_ mut std::str::Split<'a, &'a str>,
    when_missing: ScriptOverrideArgumentError,
) -> Result<&'a str, ScriptOverrideArgumentError> {
    parts
        .next()
        .and_then(|s| if s.is_empty() { None } else { Some(s) })
        .ok_or(when_missing)
}

fn decode_script_hash(
    hex_bytes: &str,
    when_hex_invalid: impl FnOnce(FromHexError) -> ScriptOverrideArgumentError,
    when_size_invalid: impl FnOnce(usize) -> ScriptOverrideArgumentError,
) -> Result<ScriptHash, ScriptOverrideArgumentError> {
    let bytes = hex::decode(hex_bytes).map_err(when_hex_invalid)?;
    let size = bytes.len();

    if size != 28 {
        return Err(when_size_invalid(size));
    }

    Ok(ScriptHash::from(bytes.as_slice()))
}
