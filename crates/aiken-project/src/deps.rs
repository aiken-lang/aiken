use std::{
    collections::{HashMap, HashSet},
    fs,
    path::Path,
};

use aiken_lang::ast::Span;
use miette::NamedSource;
use serde::{Deserialize, Serialize};
use tokio::time::Instant;

use crate::{
    config::{Dependency, ProjectConfig},
    error::{Error, TomlLoadingContext},
    package_name::PackageName,
    paths,
    telemetry::{DownloadSource, Event, EventListener},
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
    #[allow(clippy::result_large_err)]
    pub fn load(root_path: &Path) -> Result<Self, Error> {
        let path = root_path.join(paths::packages_toml());

        if !path.exists() {
            return Ok(Self {
                packages: Vec::new(),
            });
        }

        let src = fs::read_to_string(&path)?;

        let result: Self = toml::from_str(&src).map_err(|e| Error::TomlLoading {
            ctx: TomlLoadingContext::Package,
            path: Box::new(path.clone()),
            src: Box::new(src.clone()),
            named: NamedSource::new(path.display().to_string(), src).into(),
            // this isn't actually a legit way to get the span
            location: e.span().map(|range| Span {
                start: range.start,
                end: range.end,
            }),
            help: Box::new(e.message().to_string()),
        })?;

        Ok(result)
    }

    #[allow(clippy::result_large_err)]
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

    #[allow(clippy::result_large_err)]
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
        packages: &'a [Package],
        root: &PackageName,
    ) -> Vec<&'a Package> {
        packages
            .iter()
            .filter(|p| {
                &p.name != root
                    && !matches!(
                        self.packages.iter().find(|p2| p2.name == p.name),
                        Some(Dependency { version, .. }) if paths::is_git_sha_or_tag(version) && &p.version == version,
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

/// Load an already installed dependency graph without acquiring locks, resolving
/// versions, downloading packages, or changing the project or shared cache.
///
/// Dependency-free projects need no lockfile or build directory. Other projects
/// must first populate their lockfile and local package installation with
/// `aiken build`.
#[allow(clippy::result_large_err)]
pub fn load_read_only(root_path: &Path, config: &ProjectConfig) -> Result<Manifest, Error> {
    if config.dependencies.is_empty() {
        return Ok(Manifest {
            requirements: Vec::new(),
            packages: Vec::new(),
            etags: Default::default(),
        });
    }

    let manifest_path = root_path.join(paths::manifest());
    let src = fs::read_to_string(&manifest_path).map_err(|error| {
        read_only_error(format!("cannot read {}: {error}", manifest_path.display()))
    })?;
    let manifest: Manifest = toml::from_str(&src).map_err(|error| Error::TomlLoading {
        ctx: TomlLoadingContext::Manifest,
        path: Box::new(manifest_path.clone()),
        src: Box::new(src.clone()),
        named: NamedSource::new(manifest_path.display().to_string(), src).into(),
        location: error.span().map(|range| Span {
            start: range.start,
            end: range.end,
        }),
        help: Box::new(format!("{}. Run `aiken build` first.", error.message())),
    })?;

    if manifest.requirements != config.dependencies {
        return Err(read_only_error(
            "aiken.lock requirements do not match aiken.toml",
        ));
    }

    let mut resolved = HashMap::with_capacity(manifest.packages.len());
    for package in &manifest.packages {
        if package.name == config.name
            || package.version.is_empty()
            || resolved.insert(&package.name, package).is_some()
        {
            return Err(read_only_error(format!(
                "aiken.lock contains an invalid or duplicate package resolution for {}",
                package.name
            )));
        }
    }

    for dependency in &config.dependencies {
        check_read_only_resolution(&resolved, dependency)?;
    }

    let local = LocalPackages::load(root_path)
        .map_err(|error| read_only_error(format!("cannot load local package metadata: {error}")))?;
    let mut installed = HashMap::with_capacity(local.packages.len());
    for dependency in &local.packages {
        if installed.insert(&dependency.name, dependency).is_some() {
            return Err(read_only_error(format!(
                "local package metadata contains duplicate entries for {}",
                dependency.name
            )));
        }
    }

    for package in &manifest.packages {
        for requirement in &package.requirements {
            let name: PackageName = requirement.parse().map_err(|error| {
                read_only_error(format!(
                    "invalid requirement {requirement:?} for {} in aiken.lock: {error}",
                    package.name
                ))
            })?;
            if !resolved.contains_key(&name) {
                return Err(read_only_error(format!(
                    "aiken.lock is missing transitive dependency {name} required by {}",
                    package.name
                )));
            }
        }

        if !matches!(
            installed.get(&package.name),
            Some(dependency)
                if dependency.version == package.version && dependency.source == package.source
        ) {
            return Err(read_only_error(format!(
                "local package metadata does not match {} at {} from {}",
                package.name, package.version, package.source
            )));
        }

        let package_root = root_path.join(paths::build_deps_package(&package.name));
        if !package_root.is_dir() || !package_root.join("lib").is_dir() {
            return Err(read_only_error(format!(
                "installed library directory for {} at {} is missing from {}",
                package.name,
                package.version,
                package_root.display()
            )));
        }

        let installed_config = ProjectConfig::load(&package_root).map_err(|error| {
            read_only_error(format!(
                "cannot load installed package configuration at {}: {error}",
                package_root.display()
            ))
        })?;
        if installed_config.name != package.name {
            return Err(read_only_error(format!(
                "installed package at {} is {}, but aiken.lock requires {}",
                package_root.display(),
                installed_config.name,
                package.name
            )));
        }

        // The installed config's version is a package label, not necessarily its
        // git revision. packages.toml records the revision actually installed.
        // Inspect its dependencies too: older lockfiles may omit graph edges.
        for dependency in &installed_config.dependencies {
            check_read_only_resolution(&resolved, dependency)?;
        }
    }

    Ok(manifest)
}

#[allow(clippy::result_large_err)]
fn check_read_only_resolution(
    resolved: &HashMap<&PackageName, &Package>,
    dependency: &Dependency,
) -> Result<(), Error> {
    if !matches!(
        resolved.get(&dependency.name),
        Some(package)
            if package.version == dependency.version && package.source == dependency.source
    ) {
        return Err(read_only_error(format!(
            "aiken.lock does not resolve {} at {} from {}",
            dependency.name, dependency.version, dependency.source
        )));
    }
    Ok(())
}

fn read_only_error(reason: impl std::fmt::Display) -> Error {
    std::io::Error::other(format!(
        "Cannot compile dependencies read-only: {reason}. Run `aiken build` first."
    ))
    .into()
}

#[allow(clippy::result_large_err)]
pub fn download<T>(
    event_listener: &T,
    root_path: &Path,
    config: &ProjectConfig,
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

    let (mut manifest, changed) = Manifest::load(event_listener, config, root_path)?;

    let local = LocalPackages::load(root_path)?;

    local.remove_extra_packages(&manifest, root_path)?;

    runtime.block_on(fetch_missing_packages(
        &mut manifest,
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
    manifest: &mut Manifest,
    local: &LocalPackages,
    project_name: PackageName,
    root_path: &Path,
    event_listener: &T,
) -> Result<(), Error>
where
    T: EventListener,
{
    let packages = manifest.packages.to_owned();

    let mut missing = local
        .missing_local_packages(&packages, &project_name)
        .into_iter()
        .peekable();

    if missing.peek().is_some() {
        let start = Instant::now();

        event_listener.handle_event(Event::ResolvingPackages {
            name: format!("{project_name}"),
        });

        let downloader = Downloader::new(root_path);

        let statuses = downloader
            .download_packages(event_listener, missing, &project_name, manifest)
            .await?;

        let downloaded_from_network = statuses
            .iter()
            .filter(|(_, downloaded)| *downloaded)
            .count();
        if downloaded_from_network > 0 {
            event_listener.handle_event(Event::PackagesDownloaded {
                start,
                count: downloaded_from_network,
                source: DownloadSource::Network,
            });
        }

        let downloaded_from_cache = statuses
            .iter()
            .filter(|(_, downloaded)| !downloaded)
            .count();
        if downloaded_from_cache > 0 {
            event_listener.handle_event(Event::PackagesDownloaded {
                start,
                count: downloaded_from_cache,
                source: DownloadSource::Cache,
            });
        }
    }

    manifest.save(root_path)
}

#[cfg(test)]
mod read_only_tests {
    use super::*;
    use crate::config::Platform;
    use miette::Diagnostic;
    use std::{
        path::PathBuf,
        sync::atomic::{AtomicUsize, Ordering},
        time::SystemTime,
    };

    struct Fixture {
        root: PathBuf,
        config: ProjectConfig,
    }

    impl Fixture {
        fn new(installed: bool) -> Self {
            static NEXT: AtomicUsize = AtomicUsize::new(0);
            let root = std::env::temp_dir().join(format!(
                "aiken-read-only-dependencies-{}-{}",
                std::process::id(),
                NEXT.fetch_add(1, Ordering::Relaxed)
            ));
            fs::create_dir_all(&root).unwrap();
            let config: ProjectConfig = toml::from_str(
                r#"
name = "test/root"
version = "0.0.0"
plutus = "v3"
[[dependencies]]
name = "test/cached"
version = "v1.0.0"
source = "github"
"#,
            )
            .unwrap();
            config.save(&root).unwrap();
            let fixture = Self { root, config };
            if installed {
                let dependency = &fixture.config.dependencies[0];
                let manifest = Manifest {
                    requirements: fixture.config.dependencies.clone(),
                    packages: vec![Package {
                        name: dependency.name.clone(),
                        version: dependency.version.clone(),
                        source: dependency.source,
                        requirements: Vec::new(),
                    }],
                    etags: Default::default(),
                };
                manifest.save(&fixture.root).unwrap();
                LocalPackages::from(&manifest).save(&fixture.root).unwrap();
                let package_root = fixture.package_root();
                fs::create_dir_all(package_root.join("lib")).unwrap();
                let package: ProjectConfig =
                    toml::from_str("name = \"test/cached\"\nversion = \"1.0.0\"\nplutus = \"v3\"")
                        .unwrap();
                package.save(&package_root).unwrap();
                fs::write(
                    package_root.join("lib/cached.ak"),
                    "pub fn value() -> Int { 42 }",
                )
                .unwrap();
            }
            fixture
        }

        fn package_root(&self) -> PathBuf {
            self.root
                .join(paths::build_deps_package(&self.config.dependencies[0].name))
        }
    }

    impl Drop for Fixture {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.root);
        }
    }

    fn snapshot(root: &Path) -> Vec<(PathBuf, SystemTime, Option<Vec<u8>>)> {
        let mut pending = vec![root.to_path_buf()];
        let mut result = Vec::new();
        while let Some(path) = pending.pop() {
            let metadata = fs::metadata(&path).unwrap();
            let bytes = if metadata.is_dir() {
                pending.extend(
                    fs::read_dir(&path)
                        .unwrap()
                        .map(|entry| entry.unwrap().path()),
                );
                None
            } else {
                Some(fs::read(&path).unwrap())
            };
            result.push((path, metadata.modified().unwrap(), bytes));
        }
        result.sort_by(|left, right| left.0.cmp(&right.0));
        result
    }

    #[test]
    fn cached_dependencies_load_without_modifying_or_cleaning_files() {
        let fixture = Fixture::new(true);
        let extra = fixture.root.join(paths::packages()).join("unused-package");
        fs::create_dir(&extra).unwrap();
        fs::write(extra.join("keep.txt"), "do not clean up").unwrap();
        let before = snapshot(&fixture.root);

        let manifest = load_read_only(&fixture.root, &fixture.config).unwrap();

        assert_eq!(manifest.packages.len(), 1);
        assert_eq!(manifest.packages[0].version, "v1.0.0");
        assert_eq!(snapshot(&fixture.root), before);
    }

    #[test]
    fn complete_cached_transitive_dependencies_load_without_mutation() {
        let fixture = Fixture::new(true);
        let mut manifest = load_read_only(&fixture.root, &fixture.config).unwrap();
        let dependency = Dependency {
            name: "test/transitive".parse().unwrap(),
            version: "v1.0.0".into(),
            source: Platform::Github,
        };
        manifest.packages[0]
            .requirements
            .push(dependency.name.to_string());
        manifest.packages.push(Package {
            name: dependency.name.clone(),
            version: dependency.version.clone(),
            source: dependency.source,
            requirements: Vec::new(),
        });
        manifest.save(&fixture.root).unwrap();
        LocalPackages::from(&manifest).save(&fixture.root).unwrap();
        let mut cached_config = ProjectConfig::load(&fixture.package_root()).unwrap();
        cached_config.dependencies.push(dependency.clone());
        cached_config.save(&fixture.package_root()).unwrap();
        let transitive_root = fixture
            .root
            .join(paths::build_deps_package(&dependency.name));
        fs::create_dir_all(transitive_root.join("lib")).unwrap();
        cached_config.name = dependency.name;
        cached_config.dependencies.clear();
        cached_config.save(&transitive_root).unwrap();
        let before = snapshot(&fixture.root);

        let loaded = load_read_only(&fixture.root, &fixture.config).unwrap();

        assert_eq!(loaded.packages.len(), 2);
        assert_eq!(snapshot(&fixture.root), before);
    }

    #[test]
    fn dependency_free_load_ignores_existing_dependency_artifacts() {
        let mut fixture = Fixture::new(true);
        fixture.config.dependencies.clear();
        fixture.config.save(&fixture.root).unwrap();
        fs::write(fixture.root.join(paths::manifest()), "stale lockfile").unwrap();
        fs::write(fixture.root.join(paths::packages_toml()), "stale metadata").unwrap();
        let before = snapshot(&fixture.root);

        let manifest = load_read_only(&fixture.root, &fixture.config).unwrap();

        assert!(manifest.requirements.is_empty());
        assert!(manifest.packages.is_empty());
        assert_eq!(snapshot(&fixture.root), before);
    }

    #[test]
    fn missing_dependencies_fail_without_creating_locks_or_directories() {
        let fixture = Fixture::new(false);
        let before = snapshot(&fixture.root);

        let error = load_read_only(&fixture.root, &fixture.config).unwrap_err();

        assert!(error.to_string().contains("aiken.lock"));
        assert!(error.to_string().contains("aiken build"));
        assert_eq!(snapshot(&fixture.root), before);
    }

    #[test]
    fn stale_or_incomplete_dependencies_fail_without_mutation() {
        for case in [
            "requirements",
            "unresolved",
            "resolved_version",
            "resolved_source",
            "duplicate_resolution",
            "transitive_lock_requirement",
            "local_version",
            "local_source",
            "missing_metadata",
            "missing_package",
            "missing_library",
            "missing_config",
            "wrong_package",
            "transitive_config_requirement",
            "malformed_lock",
        ] {
            let fixture = Fixture::new(true);
            let mut manifest = load_read_only(&fixture.root, &fixture.config).unwrap();
            match case {
                "requirements" => manifest.requirements[0].version = "v2.0.0".into(),
                "unresolved" => manifest.packages.clear(),
                "resolved_version" => manifest.packages[0].version = "v2.0.0".into(),
                "resolved_source" => manifest.packages[0].source = Platform::Gitlab,
                "duplicate_resolution" => manifest.packages.push(manifest.packages[0].clone()),
                "transitive_lock_requirement" => {
                    manifest.packages[0]
                        .requirements
                        .push("test/transitive".into());
                }
                "local_version" | "local_source" => {
                    let mut local = LocalPackages::from(&manifest);
                    if case == "local_version" {
                        local.packages[0].version = "v2.0.0".into();
                    } else {
                        local.packages[0].source = Platform::Gitlab;
                    }
                    local.save(&fixture.root).unwrap();
                }
                "missing_metadata" => {
                    fs::remove_file(fixture.root.join(paths::packages_toml())).unwrap();
                }
                "missing_package" => fs::remove_dir_all(fixture.package_root()).unwrap(),
                "missing_library" => {
                    fs::remove_dir_all(fixture.package_root().join("lib")).unwrap();
                }
                "missing_config" => {
                    fs::remove_file(fixture.package_root().join(paths::project_config())).unwrap();
                }
                "wrong_package" | "transitive_config_requirement" => {
                    let mut config = ProjectConfig::load(&fixture.package_root()).unwrap();
                    if case == "wrong_package" {
                        config.name = "test/wrong".parse().unwrap();
                    } else {
                        config.dependencies.push(Dependency {
                            name: "test/transitive".parse().unwrap(),
                            version: "v1.0.0".into(),
                            source: Platform::Github,
                        });
                    }
                    config.save(&fixture.package_root()).unwrap();
                }
                "malformed_lock" => {}
                _ => unreachable!(),
            }
            if case == "malformed_lock" {
                fs::write(fixture.root.join(paths::manifest()), "not valid toml").unwrap();
            } else {
                manifest.save(&fixture.root).unwrap();
            }
            let before = snapshot(&fixture.root);

            let error = load_read_only(&fixture.root, &fixture.config).unwrap_err();

            let message = format!(
                "{} {}",
                error,
                error
                    .help()
                    .map(|help| help.to_string())
                    .unwrap_or_default()
            );
            assert!(message.contains("aiken build"), "{case}: {message}");
            assert_eq!(snapshot(&fixture.root), before, "{case}");
        }
    }
}
