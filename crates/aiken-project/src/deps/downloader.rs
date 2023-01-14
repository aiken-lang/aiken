use std::{io::Cursor, path::Path};

use futures::future;
use reqwest::Client;

use crate::{
    config::PackageName,
    error::Error,
    paths::{self, CacheKey},
};

use super::manifest::Package;

pub struct Downloader<'a> {
    http: Client,
    root_path: &'a Path,
}

impl<'a> Downloader<'a> {
    pub fn new(root_path: &'a Path) -> Self {
        Self {
            http: Client::new(),
            root_path,
        }
    }

    pub async fn download_packages<T>(
        &self,
        packages: T,
        project_name: &PackageName,
    ) -> Result<(), Error>
    where
        T: Iterator<Item = &'a Package>,
    {
        let tasks = packages
            .filter(|package| project_name != &package.name)
            .map(|package| self.ensure_package_in_build_directory(package));

        let _results = future::try_join_all(tasks).await?;

        Ok(())
    }

    pub async fn ensure_package_in_build_directory(
        &self,
        package: &Package,
    ) -> Result<bool, Error> {
        let cache_key = paths::CacheKey::new(&self.http, package).await?;
        self.ensure_package_downloaded(package, &cache_key).await?;
        self.extract_package_from_cache(&package.name, &cache_key)
            .await
    }

    pub async fn ensure_package_downloaded(
        &self,
        package: &Package,
        cache_key: &CacheKey,
    ) -> Result<bool, Error> {
        let packages_cache_path = paths::packages_cache();

        let zipball_path = paths::package_cache_zipball(cache_key);

        if !packages_cache_path.exists() {
            tokio::fs::create_dir_all(packages_cache_path).await?;
        }

        if zipball_path.is_file() {
            return Ok(false);
        }

        let url = format!(
            "https://api.github.com/repos/{}/{}/zipball/{}",
            package.name.owner, package.name.repo, package.version
        );

        let response = self
            .http
            .get(url)
            .header("User-Agent", "aiken-lang")
            .send()
            .await?
            .bytes()
            .await?;

        // let PackageSource::Github { url } = &package.source;

        tokio::fs::write(&zipball_path, response).await?;

        Ok(true)
    }

    pub async fn extract_package_from_cache(
        &self,
        name: &PackageName,
        cache_key: &CacheKey,
    ) -> Result<bool, Error> {
        let destination = self.root_path.join(paths::build_deps_package(name));

        // If the directory already exists then there's nothing for us to do
        if destination.is_dir() {
            return Ok(false);
        }

        tokio::fs::create_dir_all(&destination).await?;

        let zipball_path = self.root_path.join(paths::package_cache_zipball(cache_key));

        let zipball = tokio::fs::read(zipball_path).await?;

        let result = {
            let d = destination.clone();

            tokio::task::spawn_blocking(move || {
                zip_extract::extract(Cursor::new(zipball), &d, true)
            })
            .await?
        };

        if result.is_err() {
            tokio::fs::remove_dir_all(destination).await?;
        }

        result?;

        Ok(true)
    }
}
