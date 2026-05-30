use super::{output, paths::resolve_verify_out_dir};
use aiken_project::{
    config::{ProjectConfig, WorkspaceConfig},
    verify,
    watch::workspace_root,
};
use std::path::{Path, PathBuf};

/// Remove generated verification artifacts and logs.
#[derive(clap::Args)]
#[clap(disable_version_flag(true))]
#[command(
    verbatim_doc_comment,
    about = color_print::cstr!(r#"
Remove generated verification artifacts and logs.
"#),
    after_long_help = color_print::cstr!(r#"<bold><underline>Examples:</underline></bold>

    <bold>aiken verify clean</bold>
        Remove the default verification workspace

    <bold>aiken verify clean --out-dir build/verify-ci</bold>
        Remove a non-default verification workspace

You are seeing the extended help. Use `-h` instead of `--help` for a more compact view.
"#),
)]
pub struct Args {
    /// Path to project
    pub(super) directory: Option<PathBuf>,

    /// Output directory containing verification artifacts to remove
    #[clap(long, default_value = "build/verify")]
    pub(super) out_dir: PathBuf,
}

pub fn exec(Args { directory, out_dir }: Args) -> miette::Result<()> {
    let result =
        run_clean_for_directory_with(directory.as_deref(), out_dir, verify::clean_artifacts)?;

    print!("{}", result.output);
    Ok(())
}

pub(super) fn format_clean_output(removed: &[PathBuf], out_dir: &Path) -> String {
    if removed.is_empty() {
        format!("No verification artifacts found at {}\n", out_dir.display())
    } else {
        let mut output = removed
            .iter()
            .map(|p| format!("Removed {}", p.display()))
            .collect::<Vec<_>>()
            .join("\n");
        output.push('\n');
        output
    }
}

pub(super) fn run_clean_command_with<F>(
    project_root: &Path,
    out_dir: PathBuf,
    clean_artifacts: F,
) -> miette::Result<output::CommandBranchResult>
where
    F: FnOnce(&Path) -> std::io::Result<Vec<PathBuf>>,
{
    let out_dir = resolve_verify_out_dir(&out_dir, project_root)?;
    let removed = clean_artifacts(&out_dir).map_err(|e| {
        miette::miette!(
            "Failed to clean verification artifacts at {}: {}",
            out_dir.display(),
            e
        )
    })?;

    Ok(output::CommandBranchResult {
        output: format_clean_output(&removed, &out_dir),
        exit_code: 0,
    })
}

pub(super) fn manifest_declares_top_level_workspace_members(
    config_path: &Path,
    raw_config: &str,
) -> miette::Result<bool> {
    let manifest: toml::Table = raw_config.parse().map_err(|err| {
        miette::miette!(
            "Failed to parse manifest at {}: {}",
            config_path.display(),
            err
        )
    })?;

    Ok(manifest.contains_key("members"))
}

pub(super) fn clean_project_root(workspace_root: &Path) -> miette::Result<PathBuf> {
    if !workspace_root.exists() {
        return Err(miette::miette!(
            "I couldn't find the requested directory {}.",
            workspace_root.display()
        ));
    }

    if !workspace_root.is_dir() {
        return Err(miette::miette!(
            "The requested directory {} is not a directory.",
            workspace_root.display()
        ));
    }

    let config_path = workspace_root.join("aiken.toml");
    let raw_config = std::fs::read_to_string(&config_path).map_err(|err| match err.kind() {
        std::io::ErrorKind::NotFound => miette::miette!(
            "{}",
            aiken_project::error::Error::MissingManifest {
                path: Box::new(workspace_root.to_path_buf()),
            }
        ),
        _ => miette::miette!(
            "Failed to load project manifest at {}: {}",
            config_path.display(),
            err
        ),
    })?;

    if manifest_declares_top_level_workspace_members(&config_path, &raw_config)? {
        WorkspaceConfig::load(workspace_root).map_err(|err| miette::miette!(err.to_string()))?;
    } else {
        ProjectConfig::load(workspace_root).map_err(|err| miette::miette!(err.to_string()))?;
    }

    Ok(workspace_root.to_path_buf())
}

pub(super) fn run_clean_for_directory_with<F>(
    directory: Option<&Path>,
    out_dir: PathBuf,
    mut clean_artifacts: F,
) -> miette::Result<output::CommandBranchResult>
where
    F: FnMut(&Path) -> std::io::Result<Vec<PathBuf>>,
{
    let workspace_root = workspace_root(directory)?;
    let project_roots = if let Ok(workspace) = WorkspaceConfig::load(&workspace_root) {
        workspace.members
    } else {
        vec![clean_project_root(&workspace_root)?]
    };

    let mut output = String::new();
    for project_root in project_roots {
        let result =
            run_clean_command_with(&project_root, out_dir.clone(), |path| clean_artifacts(path))?;
        output.push_str(&result.output);
    }

    Ok(output::CommandBranchResult {
        output,
        exit_code: 0,
    })
}
