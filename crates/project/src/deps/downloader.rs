use std::path::Path;

use futures::future;
use reqwest::Client;

use crate::{config::PackageName, error::Error, paths};

use super::manifest::{Package, PackageSource};

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

        let results = future::try_join_all(tasks).await?;

        Ok(())
    }

    pub async fn ensure_package_in_build_directory(
        &self,
        package: &Package,
    ) -> Result<bool, Error> {
        self.ensure_package_downloaded(package).await?;
        self.extract_package_from_cache(&package.name, &package.version)
    }

    pub async fn ensure_package_downloaded(&self, package: &Package) -> Result<bool, Error> {
        let zipball_path =
            paths::package_cache_zipball(&package.name, &package.version.to_string());

        if zipball_path.is_file() {
            return Ok(false);
        }

        let url = format!(
            "https://api.github.com/repos/{}/{}/zipball/{}",
            package.name.owner, package.name.repo, package.version
        );

        let response = self.http.get(url).send().await?;

        let PackageSource::Github { url } = &package.source;

        let zipball =
            hexpm::get_package_tarball_response(response, &outer_checksum.0).map_err(|error| {
                Error::DownloadPackageError {
                    package_name: package.name.to_string(),
                    package_version: package.version.to_string(),
                    error: error.to_string(),
                }
            })?;

        tokio::fs::write(&zipball_path, zipball).await?;

        Ok(true)
    }
}
