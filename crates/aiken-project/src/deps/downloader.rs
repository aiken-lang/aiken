use std::{
    collections::BTreeMap,
    io::{self, Cursor, Read},
    path::{Component, Path, PathBuf},
};

use futures::future;
use reqwest::Client;
use zip::result::ZipError;

use crate::{
    deps::manifest::Manifest,
    error::Error,
    package_name::PackageName,
    package_subdir::PackageSubdir,
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
        let mut archives: BTreeMap<String, (CacheKey, Vec<&Package>)> = BTreeMap::new();
        for package in packages.filter(|package| project_name != &package.name) {
            let cache_key =
                paths::CacheKey::new(&self.http, event_listener, package, manifest).await?;
            archives
                .entry(cache_key.get_key().to_owned())
                .or_insert_with(|| (cache_key, Vec::new()))
                .1
                .push(package);
        }

        // Subdirectory packages at the same revision share a repository archive.
        // Fetch each archive once, avoiding concurrent writes to the same cache file.
        let tasks = archives
            .into_values()
            .map(|(cache_key, packages)| async move {
                let mut statuses = Vec::new();
                for package in packages {
                    statuses.push(
                        self.ensure_package_in_build_directory(package, &cache_key)
                            .await?,
                    );
                }
                Ok::<_, Error>(statuses)
            });

        Ok(future::try_join_all(tasks)
            .await?
            .into_iter()
            .flatten()
            .collect())
    }

    pub async fn ensure_package_in_build_directory(
        &self,
        package: &Package,
        cache_key: &CacheKey,
    ) -> Result<(PackageName, bool), Error> {
        let downloaded = self
            .ensure_package_downloaded(package, cache_key)
            .await
            .map(|downloaded| (package.name.clone(), downloaded))?;
        self.extract_package_from_cache(package, cache_key).await?;
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
                package: Box::new(package.clone()),
            });
        }

        let bytes = response.bytes().await?;

        tokio::fs::write(&zipball_path, bytes).await?;

        Ok(true)
    }

    pub async fn extract_package_from_cache(
        &self,
        package: &Package,
        cache_key: &CacheKey,
    ) -> Result<(), Error> {
        let destination = self.root_path.join(paths::build_deps_package(
            &package.name,
            package.subdir.as_ref(),
        ));

        tokio::fs::create_dir_all(&destination).await?;

        let zipball_path = self.root_path.join(paths::package_cache_zipball(cache_key));

        let zipball = tokio::fs::read(zipball_path).await?;

        let result = {
            let d = destination.clone();
            let subdir = package.subdir.clone();

            tokio::task::spawn_blocking(move || {
                let mut archive =
                    zip::ZipArchive::new(Cursor::new(zipball)).expect("failed to load zip archive");

                extract_zip(&mut archive, &d, subdir.as_ref())
            })
            .await?
        };

        let result = result.map_err(Error::from).and_then(|()| {
            if package.subdir.is_some() && !destination.join(paths::project_config()).is_file() {
                Err(Error::MissingPackageSubdir {
                    package: Box::new(package.clone()),
                })
            } else {
                Ok(())
            }
        });

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
    subdir: Option<&PackageSubdir>,
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

        // enclosed_name checks the complete archive path. After removing the
        // archive wrapper and selected subdirectory, '..' could still escape
        // the destination, so require normal components before selecting a subtree.
        if filepath
            .components()
            .any(|part| !matches!(part, Component::Normal(_)))
        {
            return Err(ZipError::InvalidArchive("Invalid file path"));
        }

        let filepath = match subdir {
            Some(subdir) => match filepath.strip_prefix(subdir.as_str()) {
                Ok(path) => path,
                Err(_) => continue,
            },
            None => &filepath,
        };

        if filepath.as_os_str().is_empty() {
            continue;
        }

        let outpath = directory.as_ref().join(filepath);

        if file.name().ends_with('/') {
            fs::create_dir_all(&outpath)?;
        } else {
            if let Some(p) = outpath.parent()
                && !p.exists()
            {
                fs::create_dir_all(p)?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        fs,
        io::Write,
        time::{SystemTime, UNIX_EPOCH},
    };

    fn archive(entries: &[&str]) -> zip::ZipArchive<Cursor<Vec<u8>>> {
        let mut zip = zip::ZipWriter::new(Cursor::new(Vec::new()));
        for path in entries {
            zip.start_file(
                format!("example-contracts-abc123/{path}"),
                Default::default(),
            )
            .unwrap();
            zip.write_all(b"").unwrap();
        }
        zip::ZipArchive::new(zip.finish().unwrap()).unwrap()
    }

    fn temp_dir(name: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!("aiken-{name}-{unique}"))
    }

    #[test]
    fn extracts_only_the_selected_subdir() {
        let root = temp_dir("extract-subdir");
        let subdir = "onchain".parse().unwrap();
        let mut zip = archive(&[
            "aiken.toml",
            "lib/root.ak",
            "onchain/aiken.toml",
            "onchain/lib/contract.ak",
            "onchain2/lib/sibling.ak",
        ]);

        extract_zip(&mut zip, &root, Some(&subdir)).unwrap();

        assert!(root.join("aiken.toml").is_file());
        assert!(root.join("lib/contract.ak").is_file());
        assert!(!root.join("lib/root.ak").exists());
        assert!(!root.join("lib/sibling.ak").exists());
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn rejects_paths_escaping_the_selected_subdir() {
        let root = temp_dir("extract-escape");
        let subdir = "onchain".parse().unwrap();
        let mut zip = archive(&["onchain/../escaped.ak"]);

        assert!(extract_zip(&mut zip, root.join("package"), Some(&subdir)).is_err());
        assert!(!root.join("escaped.ak").exists());
        let _ = fs::remove_dir_all(root);
    }
}
