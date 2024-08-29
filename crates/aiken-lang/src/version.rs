pub fn compiler_version(include_commit_hash: bool) -> String {
    if include_commit_hash {
        format!(
            "v{}+{}",
            built_info::PKG_VERSION,
            built_info::GIT_COMMIT_HASH_SHORT.unwrap_or("unknown")
        )
    } else {
        format!("v{}", built_info::PKG_VERSION,)
    }
}

mod built_info {
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}
