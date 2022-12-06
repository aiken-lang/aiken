pub mod cmd;

use aiken_project::{config::Config, Project};
use miette::IntoDiagnostic;
use std::env;
use std::path::PathBuf;

pub fn with_project<A>(directory: Option<PathBuf>, mut action: A) -> miette::Result<()>
where
    A: FnMut(&mut Project) -> Result<(), aiken_project::error::Error>,
{
    let project_path = if let Some(d) = directory {
        d
    } else {
        env::current_dir().into_diagnostic()?
    };

    let config = Config::load(project_path.clone()).into_diagnostic()?;

    let mut project = Project::new(config, project_path);

    let build_result = action(&mut project);

    let warning_count = project.warnings.len();

    for warning in project.warnings {
        warning.report()
    }

    if let Err(err) = build_result {
        err.report();

        miette::bail!("failed: {} error(s), {warning_count} warning(s)", err.len(),);
    };

    println!("\nfinished with {warning_count} warning(s)\n");

    Ok(())
}
