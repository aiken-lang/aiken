use std::path::{Path, PathBuf};

pub(super) fn resolve_verify_out_dir(
    out_dir: &Path,
    project_root: &Path,
) -> miette::Result<PathBuf> {
    if out_dir
        .components()
        .any(|component| matches!(component, std::path::Component::ParentDir))
    {
        return Err(miette::miette!(
            "Invalid --out-dir '{}': parent directory segments ('..') are not allowed.",
            out_dir.display()
        ));
    }

    let resolved = if out_dir.is_absolute() {
        out_dir.to_path_buf()
    } else {
        project_root.join(out_dir)
    };

    let project_root_abs = canonicalize_with_existing_ancestors(project_root)?;
    let resolved_abs = canonicalize_with_existing_ancestors(&resolved)?;
    if !resolved_abs.starts_with(&project_root_abs) {
        return Err(miette::miette!(
            "Invalid --out-dir '{}': path must resolve inside project root '{}'.",
            out_dir.display(),
            project_root.display()
        ));
    }

    Ok(resolved)
}

pub(super) fn canonicalize_with_existing_ancestors(path: &Path) -> miette::Result<PathBuf> {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .map(|cwd| cwd.join(path))
            .map_err(|e| miette::miette!("Failed to resolve path '{}': {e}", path.display()))?
    };

    let mut nearest_existing = absolute.as_path();
    let mut missing_suffix = Vec::new();
    while !nearest_existing.exists() {
        let Some(file_name) = nearest_existing.file_name() else {
            return Err(miette::miette!(
                "Failed to canonicalize path '{}': no existing parent directory found.",
                absolute.display()
            ));
        };
        missing_suffix.push(file_name.to_os_string());
        let Some(parent) = nearest_existing.parent() else {
            return Err(miette::miette!(
                "Failed to canonicalize path '{}': no parent directory found.",
                absolute.display()
            ));
        };
        nearest_existing = parent;
    }

    let mut canonical = nearest_existing.canonicalize().map_err(|e| {
        miette::miette!(
            "Failed to canonicalize path '{}': {e}",
            nearest_existing.display()
        )
    })?;
    for component in missing_suffix.iter().rev() {
        canonical.push(component);
    }
    Ok(canonical)
}
