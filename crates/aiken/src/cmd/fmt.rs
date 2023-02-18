#[derive(clap::Args)]
/// Format an Aiken project
pub struct Args {
    /// Files to format
    #[clap(default_value = ".")]
    files: Vec<String>,

    /// Read source from STDIN
    #[clap(long)]
    stdin: bool,

    /// Check if inputs are formatted without changing them
    #[clap(long)]
    check: bool,
}

pub fn exec(
    Args {
        check,
        stdin,
        files,
    }: Args,
) -> miette::Result<()> {
    if let Err(errs) = aiken_project::format::run(stdin, check, files) {
        for err in &errs {
            err.report();
        }

        miette::bail!("failed: {} error(s)", errs.len());
    };

    Ok(())
}
