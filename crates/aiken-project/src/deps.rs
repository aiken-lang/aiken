use std::{collections::HashSet, fs, path::Path};

use aiken_lang::ast::Span;
use miette::NamedSource;
use serde::{Deserialize, Serialize};
use tokio::time::Instant;

use crate::{
    config::{Config, Dependency},
    error::Error,
    package_name::PackageName,
    paths,
    telemetry::{Event, EventListener},
};

use self::{
    downloader::Downloader,
    manifest::{Manifest, Package},
};

pub mod downloader;
pub mod manifest;

pub enum UseManifest {
    Yes,
    No,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct LocalPackages {
    packages: Vec<Dependency>,
}

impl LocalPackages {
    pub fn load(root_path: &Path) -> Result<Self, Error> {
        let path = root_path.join(paths::packages_toml());

        if !path.exists() {
            return Ok(Self {
                packages: Vec::new(),
            });
        }

        let src = fs::read_to_string(&path)?;

        let result: Self = toml::from_str(&src).map_err(|e| Error::TomlLoading {
            path: path.clone(),
            src: src.clone(),
            named: NamedSource::new(path.display().to_string(), src).into(),
            // this isn't actually a legit way to get the span
            location: e.span().map(|range| Span {
                start: range.start,
                end: range.end,
            }),
            help: e.to_string(),
        })?;

        Ok(result)
    }

    pub fn save(&self, root_path: &Path) -> Result<(), Error> {
        let packages_path = root_path.join(paths::packages());
        let path = root_path.join(paths::packages_toml());

        if !packages_path.exists() {
            fs::create_dir_all(&packages_path)?;
        }

        let toml = toml::to_string(&self).expect("packages.toml serialization");

        fs::write(path, toml)?;

        Ok(())
    }

    fn remove_extra_packages(&self, manifest: &Manifest, root_path: &Path) -> Result<(), Error> {
        for (package, _version) in self.extra_local_packages(manifest) {
            let path = root_path.join(paths::build_deps_package(&package));

            if path.exists() {
                fs::remove_dir_all(&path)?;
            }
        }

        Ok(())
    }
    pub fn extra_local_packages(&self, manifest: &Manifest) -> Vec<(PackageName, String)> {
        let manifest_packages: HashSet<_> = manifest
            .packages
            .iter()
            .map(|p| (&p.name, &p.version))
            .collect();

        self.packages
            .iter()
            .filter(|dep| !manifest_packages.contains(&(&dep.name, &dep.version)))
            .map(|dep| (dep.name.clone(), dep.version.clone()))
            .collect()
    }

    pub fn missing_local_packages<'a>(
        &self,
        manifest: &'a Manifest,
        root: &PackageName,
    ) -> Vec<&'a Package> {
        manifest
            .packages
            .iter()
            .filter(|p| {
                &p.name != root
                    && !matches!(
                        self.packages.iter().find(|p2| p2.name == p.name),
                        Some(Dependency { version, .. }) if &p.version == version,
                    )
            })
            .collect()
    }
}

impl From<&Manifest> for LocalPackages {
    fn from(value: &Manifest) -> Self {
        Self {
            packages: value
                .packages
                .iter()
                .map(|p| Dependency {
                    name: p.name.clone(),
                    version: p.version.clone(),
                    source: p.source,
                })
                .collect(),
        }
    }
}

pub fn download<T>(
    event_listener: &T,
    use_manifest: UseManifest,
    root_path: &Path,
    config: &Config,
) -> Result<Manifest, Error>
where
    T: EventListener,
{
    let build_path = root_path.join(paths::build());

    if !build_path.is_dir() {
        fs::create_dir_all(&build_path)?;
    }

    let mut build_lock = fslock::LockFile::open(&build_path.join("aiken-compile.lock"))
        .expect("Build Lock Creation");

    if !build_lock
        .try_lock_with_pid()
        .expect("Trying build locking")
    {
        event_listener.handle_event(Event::WaitingForBuildDirLock);

        build_lock.lock_with_pid().expect("Build locking")
    }

    let project_name = config.name.clone();

    let runtime = tokio::runtime::Runtime::new().expect("Unable to start Tokio");

    let (manifest, changed) = Manifest::load(
        runtime.handle().clone(),
        event_listener,
        config,
        use_manifest,
        root_path,
    )?;

    let local = LocalPackages::load(root_path)?;

    local.remove_extra_packages(&manifest, root_path)?;

    runtime.block_on(fetch_missing_packages(
        &manifest,
        &local,
        project_name,
        root_path,
        event_listener,
    ))?;

    if changed {
        manifest.save(root_path)?;
    }

    LocalPackages::from(&manifest).save(root_path)?;

    Ok(manifest)
}

async fn fetch_missing_packages<T>(
    manifest: &Manifest,
    local: &LocalPackages,
    project_name: PackageName,
    root_path: &Path,
    event_listener: &T,
) -> Result<(), Error>
where
    T: EventListener,
{
    let mut count = 0;

    let mut missing = local
        .missing_local_packages(manifest, &project_name)
        .into_iter()
        .map(|package| {
            count += 1;
            package
        })
        .peekable();

    if missing.peek().is_some() {
        let start = Instant::now();

        event_listener.handle_event(Event::DownloadingPackage {
            name: "packages".to_string(),
        });

        let downloader = Downloader::new(root_path);

        downloader.download_packages(missing, &project_name).await?;

        event_listener.handle_event(Event::PackagesDownloaded { start, count });
    }

    Ok(())
}
