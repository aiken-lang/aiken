use crate::deps::manifest::Package;
use crate::{
    deps::manifest::Manifest,
    error::Error,
    package_name::PackageName,
    telemetry::{Event, EventListener},
};
use regex::Regex;
use reqwest::Client;
use std::{fs, path::PathBuf};

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
    packages_cache().join(format!("{}.zip", cache_key.get_key()))
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
    pub async fn new<T>(
        http: &Client,
        event_listener: &T,
        package: &Package,
        manifest: &mut Manifest,
    ) -> Result<CacheKey, Error>
    where
        T: EventListener,
    {
        Ok(CacheKey::from_package(
            package,
            if is_git_sha_or_tag(&package.version) {
                Ok(package.version.to_string())
            } else {
                match manifest.lookup_etag(package) {
                    None => match new_etag_from_network(http, package).await {
                        Err(_) => {
                            event_listener.handle_event(Event::PackageResolveFallback {
                                name: format!("{}", package.name),
                            });
                            new_cache_key_from_cache(package)
                        }
                        Ok(etag) => {
                            manifest.insert_etag(package, etag.clone());
                            Ok(format!(
                                "{version}@{etag}",
                                version = package.version.replace('/', "_")
                            ))
                        }
                    },
                    Some(etag) => Ok(format!(
                        "{version}@{etag}",
                        version = package.version.replace('/', "_")
                    )),
                }
            }?,
        ))
    }

    fn from_package(package: &Package, version: String) -> CacheKey {
        CacheKey {
            key: format!("{}-{}-{}", package.name.owner, package.name.repo, version),
        }
    }

    pub fn get_key(&self) -> &str {
        self.key.as_ref()
    }
}

async fn new_etag_from_network(http: &Client, package: &Package) -> Result<String, Error> {
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
        })?;
    Ok(etag.to_str().unwrap().replace('"', ""))
}

fn new_cache_key_from_cache(target: &Package) -> Result<String, Error> {
    let packages = fs::read_dir(packages_cache())?;

    let prefix = CacheKey::from_package(target, target.version.replace('/', "_"))
        .get_key()
        .to_string();
    let mut most_recently_modified_date = None;
    let mut most_recently_modified = None;

    for pkg in packages {
        let entry = pkg.unwrap();

        let filename = entry
            .file_name()
            .into_string()
            .expect("cache filename are valid utf8 strings");

        if filename.starts_with(&prefix) {
            let last_modified = entry.metadata()?.modified()?;
            if Some(last_modified) > most_recently_modified_date {
                most_recently_modified_date = Some(last_modified);
                most_recently_modified = Some(filename);
            }
        }
    }

    match most_recently_modified {
        None => Err(Error::UnableToResolvePackage {
            package: target.clone(),
        }),
        Some(pkg) => Ok(format!(
            "{version}{etag}",
            version = target.version,
            etag = pkg
                .strip_prefix(&prefix)
                .expect("cache filename starts with a valid version prefix")
                .strip_suffix(".zip")
                .expect("cache files are all zip archives")
        )),
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
