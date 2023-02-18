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
