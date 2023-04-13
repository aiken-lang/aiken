use crate::with_project;
use aiken_project::{blueprint, error::Error};
use owo_colors::{OwoColorize, Stream::Stderr};
use std::{fs, path::PathBuf, process, rc::Rc};
use uplc::ast::{Constant, DeBruijn, Term};

/// Apply a parameter to a parameterized validator.
#[derive(clap::Args)]
pub struct Args {
    /// The parameter, as a Plutus Data (CBOR, hex-encoded).
    ///
    /// For example, `182A` designates an integer of value 42. If you're unsure about the shape of
    /// the parameter, look at the schema specified in the project's blueprint (i.e.
    /// `plutus.json`), or use the `cbor.serialise` function from the Aiken standard library.
    parameter: String,

    /// Path to project
    directory: Option<PathBuf>,

    /// Output file. Optional, print on stdout when omitted.
    #[clap(short, long)]
    out: Option<PathBuf>,

    /// Name of the validator's module within the project. Optional if there's only one validator.
    #[clap(short, long)]
    module: Option<String>,

    /// Name of the validator within the module. Optional if there's only one validator.
    #[clap(short, long)]
    validator: Option<String>,
}

pub fn exec(
    Args {
        parameter,
        directory,
        out,
        module,
        validator,
    }: Args,
) -> miette::Result<()> {
    eprintln!(
        "{} inputs",
        "      Parsing"
            .if_supports_color(Stderr, |s| s.purple())
            .if_supports_color(Stderr, |s| s.bold()),
    );

    let bytes = hex::decode(parameter)
        .map_err::<Error, _>(|e| {
            blueprint::error::Error::MalformedParameter {
                hint: format!("Invalid hex-encoded string: {e}"),
            }
            .into()
        })
        .unwrap_or_else(|e| {
            println!();
            e.report();
            process::exit(1)
        });

    let data = uplc::plutus_data(&bytes)
        .map_err::<Error, _>(|e| {
            blueprint::error::Error::MalformedParameter {
                hint: format!("Invalid Plutus data; malformed CBOR encoding: {e}"),
            }
            .into()
        })
        .unwrap_or_else(|e| {
            println!();
            e.report();
            process::exit(1)
        });

    let term: Term<DeBruijn> = Term::Constant(Rc::new(Constant::Data(data)));

    eprintln!(
        "{} blueprint",
        "    Analyzing"
            .if_supports_color(Stderr, |s| s.purple())
            .if_supports_color(Stderr, |s| s.bold()),
    );

    with_project(directory, |p| {
        let title = module.as_ref().map(|m| {
            format!(
                "{m}{}",
                validator
                    .as_ref()
                    .map(|v| format!(".{v}"))
                    .unwrap_or_default()
            )
        });

        let title = title.as_ref().or(validator.as_ref());

        eprintln!(
            "{} parameter",
            "     Applying"
                .if_supports_color(Stderr, |s| s.purple())
                .if_supports_color(Stderr, |s| s.bold()),
        );

        let blueprint = p.apply_parameter(title, &term)?;

        let json = serde_json::to_string_pretty(&blueprint).unwrap();

        match out {
            None => {
                println!("\n{}\n", json);
                Ok(())
            }
            Some(ref path) => fs::write(path, json).map_err(|error| Error::FileIo {
                error,
                path: p.blueprint_path(),
            }),
        }?;

        eprintln!(
            "{}",
            "         Done"
                .if_supports_color(Stderr, |s| s.purple())
                .if_supports_color(Stderr, |s| s.bold()),
        );

        Ok(())
    })
}
