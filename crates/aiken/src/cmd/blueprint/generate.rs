use aiken_project::blueprint::{error::Error as BlueprintError, Blueprint};
use extism::{Context, Plugin};
use miette::IntoDiagnostic;
use std::{env, fs::File, io::BufReader, path::PathBuf};

/// Compute a validator's address.
#[derive(clap::Args)]
pub struct Args {
    /// Path to project
    directory: Option<PathBuf>,
}

#[derive(serde::Deserialize)]
struct Output {
    pub code: String,
}

pub fn exec(Args { directory }: Args) -> miette::Result<()> {
    let project_path = if let Some(d) = directory {
        d
    } else {
        env::current_dir().into_diagnostic()?
    };

    let blueprint_path = project_path.join("plutus.json");

    // Read blueprint
    let blueprint = File::open(blueprint_path)
        .map_err(|_| BlueprintError::InvalidOrMissingFile)
        .into_diagnostic()?;

    let blueprint: Blueprint =
        serde_json::from_reader(BufReader::new(blueprint)).into_diagnostic()?;

    let wasm = include_bytes!("../../../../../plugin/target/wasm32-wasi/release/plugin.wasm");
    // NOTE: if you encounter an error such as:
    // "Unable to load plugin: unknown import: wasi_snapshot_preview1::fd_write has not been defined"
    // change `false` to `true` in the following function to provide WASI imports to your plugin.
    let context = Context::new();
    let mut plugin = Plugin::new(&context, wasm, [], true).map_err(|e| miette::miette!("{}", e))?;

    let data = plugin
        .call(
            "count_vowels",
            serde_json::to_vec(&blueprint).into_diagnostic()?,
        )
        .map_err(|e| miette::miette!("{e}"))?;

    let output: Output = serde_json::from_reader(data).into_diagnostic()?;

    println!("{}", output.code);

    Ok(())
}
