use miette::IntoDiagnostic;
use project::{config::Config, Project};
use std::env;
use std::path::PathBuf;

// TODO: Refactor this to remove logic duplication with the 'build command'

#[derive(clap::Args)]
/// Typecheck a project project
pub struct Args {
    /// Path to project
    #[clap(short, long)]
    directory: Option<PathBuf>,
}

pub fn exec(Args { directory }: Args) -> miette::Result<()> {
    let project_path = if let Some(d) = directory {
        d
    } else {
        env::current_dir().into_diagnostic()?
    };

    let config = Config::load(project_path.clone()).into_diagnostic()?;

    let mut project = Project::new(config, project_path);

    let build_result = project.check();

    let warning_count = project.warnings.len();

    for warning in project.warnings {
        warning.report()
    }

    if let Err(err) = build_result {
        err.report();

        miette::bail!(
            "failed: {} error(s), {warning_count} warning(s)",
            err.total(),
        );
    };

    println!("finished with {warning_count} warning(s)");
    return Ok(());
}
