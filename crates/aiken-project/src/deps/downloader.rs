use std::{
    io::{self, Cursor, Read},
    path::{Path, PathBuf},
};

use futures::future;
use reqwest::Client;
use zip::result::ZipError;

use crate::{
    deps::manifest::Manifest,
    error::Error,
    package_name::PackageName,
    paths::{self, CacheKey},
    telemetry::EventListener,
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

    pub async fn download_packages<I, T>(
        &self,
        event_listener: &T,
        packages: I,
        project_name: &PackageName,
        manifest: &mut Manifest,
    ) -> Result<Vec<(PackageName, bool)>, Error>
    where
        T: EventListener,
        I: Iterator<Item = &'a Package>,
    {
        let mut tasks = vec![];

        for package in packages.filter(|package| project_name != &package.name) {
            let cache_key =
                paths::CacheKey::new(&self.http, event_listener, package, manifest).await?;
            let task = self.ensure_package_in_build_directory(package, cache_key);
            tasks.push(task);
        }

        future::try_join_all(tasks).await
    }

    pub async fn ensure_package_in_build_directory(
        &self,
        package: &Package,
        cache_key: CacheKey,
    ) -> Result<(PackageName, bool), Error> {
        let downloaded = self
            .ensure_package_downloaded(package, &cache_key)
            .await
            .map(|downloaded| (package.name.clone(), downloaded))?;
        self.extract_package_from_cache(&package.name, &cache_key)
            .await?;
        Ok(downloaded)
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
            .await?;

        if response.status().as_u16() >= 400 {
            return Err(Error::UnknownPackageVersion {
                package: package.clone(),
            });
        }

        let bytes = response.bytes().await?;

        tokio::fs::write(&zipball_path, bytes).await?;

        Ok(true)
    }

    pub async fn extract_package_from_cache(
        &self,
        name: &PackageName,
        cache_key: &CacheKey,
    ) -> Result<(), Error> {
        let destination = self.root_path.join(paths::build_deps_package(name));

        tokio::fs::create_dir_all(&destination).await?;

        let zipball_path = self.root_path.join(paths::package_cache_zipball(cache_key));

        let zipball = tokio::fs::read(zipball_path).await?;

        let result = {
            let d = destination.clone();

            tokio::task::spawn_blocking(move || {
                let mut archive =
                    zip::ZipArchive::new(Cursor::new(zipball)).expect("failed to load zip archive");

                extract_zip(&mut archive, &d)
            })
            .await?
        };

        if result.is_err() {
            tokio::fs::remove_dir_all(destination).await?;
        }

        result?;

        Ok(())
    }
}

fn extract_zip<R: Read + io::Seek, P: AsRef<Path>>(
    archive: &mut zip::ZipArchive<R>,
    directory: P,
) -> Result<(), ZipError> {
    use std::fs;

    for i in 0..archive.len() {
        let mut file = archive.by_index(i)?;

        let filepath = file
            .enclosed_name()
            .ok_or(ZipError::InvalidArchive("Invalid file path"))?
            .iter()
            .skip(1)
            .collect::<PathBuf>();

        let outpath = directory.as_ref().join(filepath);

        if file.name().ends_with('/') {
            fs::create_dir_all(&outpath)?;
        } else {
            if let Some(p) = outpath.parent() {
                if !p.exists() {
                    fs::create_dir_all(p)?;
                }
            }
            let mut outfile = fs::File::create(&outpath)?;
            std::io::copy(&mut file, &mut outfile)?;
        }
        // Get and Set permissions
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            if let Some(mode) = file.unix_mode() {
                fs::set_permissions(&outpath, fs::Permissions::from_mode(mode))?;
            }
        }
    }

    Ok(())
}
