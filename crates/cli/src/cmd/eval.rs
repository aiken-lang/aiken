use aiken_project::options::{CodeGenMode, Options};
use std::path::PathBuf;

#[derive(clap::Args)]
/// Evaluate a chosen function with no argument.
pub struct Args {
    /// Path to project
    #[clap(short, long)]
    directory: Option<PathBuf>,

    /// Evaluate the given function
    #[clap(short, long)]
    function_name: String,
}

pub fn exec(
    Args {
        directory,
        function_name,
    }: Args,
) -> miette::Result<()> {
    crate::with_project(directory, |p| {
        p.compile(Options {
            code_gen_mode: CodeGenMode::Eval(function_name.clone()),
        })
    })
}
