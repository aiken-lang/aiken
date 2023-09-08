use crate::deps::manifest::Package;
use crate::{error::Error, package_name::PackageName};
use reqwest::Client;
use std::path::PathBuf;

pub fn project_config() -> PathBuf {
    PathBuf::from("aiken.toml")
}

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

pub fn package_cache_zipball(cache_key: &CacheKey) -> PathBuf {
    packages_cache().join(cache_key.get_key())
}

pub fn packages_cache() -> PathBuf {
    default_aiken_cache().join("packages")
}

pub fn default_aiken_cache() -> PathBuf {
    dirs::cache_dir()
        .expect("Failed to determine user cache directory")
        .join("aiken")
}

#[derive(Debug)]
pub struct CacheKey {
    key: String,
}

impl CacheKey {
    pub async fn new(http: &Client, package: &Package) -> Result<CacheKey, Error> {
        let version = match hex::decode(&package.version) {
            Ok(..) => Ok(package.version.to_string()),
            Err(..) => {
                let url = format!(
                    "https://api.github.com/repos/{}/{}/zipball/{}",
                    package.name.owner, package.name.repo, package.version
                );
                let response = http
                    .head(url)
                    .header("User-Agent", "aiken-lang")
                    .send()
                    .await?;
                let etag = response
                    .headers()
                    .get("etag")
                    .ok_or(Error::UnknownPackageVersion {
                        package: package.clone(),
                    })?
                    .to_str()
                    .unwrap()
                    .replace('"', "");
                Ok(format!("main@{etag}"))
            }
        };
        version.map(|version| CacheKey {
            key: format!(
                "{}-{}-{}.zip",
                package.name.owner, package.name.repo, version
            ),
        })
    }

    pub fn get_key(&self) -> &str {
        self.key.as_ref()
    }
}

// Best-effort to assert whether a version refers is a git sha digest or a tag. When it is, we
// avoid re-downloading it if it's already fetched. But when it isn't, and thus refer to a branch,
// we always re-download it. Note however that the download might be short-circuited by the
// system-wide package cache, so a download doesn't actually mean a network request.
//
// The package cache is however smart-enough to assert whether a package in the cache must be
// re-downloaded (using HTTP ETag). So this is mostly about delegating the re-downloading logic to
// the global packages cache.
pub fn is_git_sha_or_tag(version: &str) -> bool {
    let r_sha = Regex::new("^[0-9a-f]{7,10}$|^[0-9a-f]{40}$").unwrap();
    let r_version = Regex::new("^v?[0-9]+\\.[0-9]+(\\.[0-9]+)?([-+].+)?$").unwrap();
    r_sha.is_match(version) || r_version.is_match(version)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_git_sha_or_tag() {
        assert!(
            is_git_sha_or_tag("8ba5946c32a7dc99ae199e0e7b9948f9f361aaee"),
            "sha full"
        );
        assert!(is_git_sha_or_tag("8ba5946"), "sha short");
        assert!(is_git_sha_or_tag("1.1.0"), "semver");
        assert!(is_git_sha_or_tag("1.1.0-rc1"), "semver rc");
        assert!(is_git_sha_or_tag("1.1.0+foo"), "semver patch");
        assert!(is_git_sha_or_tag("v1.6"), "major/minor + prefix");
        assert!(!is_git_sha_or_tag("release/2.0.0"), "release branch");
        assert!(!is_git_sha_or_tag("main"), "main branch");
        assert!(!is_git_sha_or_tag("8ba594659468ba"), "not sha");
    }
}
