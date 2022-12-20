use std::path::PathBuf;

use crate::config::PackageName;

pub fn manifest() -> PathBuf {
    PathBuf::from("aiken.lock")
}

pub fn build() -> PathBuf {
    PathBuf::from("build")
}

pub fn packages() -> PathBuf {
    build().join("packages")
}

pub fn packages_toml() -> PathBuf {
    packages().join("packages.toml")
}

pub fn build_deps_package(package_name: &PackageName) -> PathBuf {
    packages().join(format!("{}-{}", package_name.owner, package_name.repo))
}

pub fn package_cache_zipball(package_name: &PackageName, version: &str) -> PathBuf {
    packages_cache().join(format!(
        "{}-{}-{}.zip",
        package_name.owner, package_name.repo, version
    ))
}

pub fn packages_cache() -> PathBuf {
    default_aiken_cache().join("packages")
}

pub fn default_aiken_cache() -> PathBuf {
    dirs::cache_dir()
        .expect("Failed to determine user cache directory")
        .join("aiken")
}
