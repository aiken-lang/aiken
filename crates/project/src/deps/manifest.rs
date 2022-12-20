use std::{fs, path::Path};

use aiken_lang::ast::Span;
use miette::NamedSource;
use serde::{Deserialize, Serialize};

use crate::{
    config::{Config, Dependency, PackageName, Platform},
    error::Error,
    paths,
    telemetry::{Event, EventListener},
};

use super::UseManifest;

#[derive(Deserialize, Serialize)]
pub struct Manifest {
    pub requirements: Vec<Dependency>,
    pub packages: Vec<Package>,
}

impl Manifest {
    pub fn load<T>(
        runtime: tokio::runtime::Handle,
        event_listener: &T,
        config: &Config,
        use_manifest: UseManifest,
        root_path: &Path,
    ) -> Result<(Self, bool), Error>
    where
        T: EventListener,
    {
        let manifest_path = root_path.join(paths::manifest());

        // If there's no manifest (or we have been asked not to use it) then resolve
        // the versions anew
        let should_resolve = match use_manifest {
            _ if !manifest_path.exists() => true,
            UseManifest::No => true,
            UseManifest::Yes => false,
        };

        if should_resolve {
            let manifest = resolve_versions(runtime, config, None, event_listener)?;

            return Ok((manifest, true));
        }

        let toml = fs::read_to_string(&manifest_path)?;

        let manifest: Self = toml::from_str(&toml).map_err(|e| Error::TomlLoading {
            path: manifest_path.clone(),
            src: toml.clone(),
            named: NamedSource::new(manifest_path.display().to_string(), toml),
            // this isn't actually a legit way to get the span
            location: e.line_col().map(|(line, col)| Span {
                start: line,
                end: col,
            }),
            help: e.to_string(),
        })?;

        // If the config has unchanged since the manifest was written then it is up
        // to date so we can return it unmodified.
        if manifest.requirements == config.dependencies {
            Ok((manifest, false))
        } else {
            let manifest = resolve_versions(runtime, config, Some(&manifest), event_listener)?;

            Ok((manifest, true))
        }
    }

    pub fn save(&self, root_path: &Path) -> Result<(), Error> {
        let manifest_path = root_path.join(paths::manifest());

        let mut toml = toml::to_string(&self).expect("aiken.lock serialization");

        toml.insert_str(
            0,
            "# This file was generated by Aiken\n# You typically do not need to edit this file\n\n",
        );

        fs::write(manifest_path, toml)?;

        Ok(())
    }
}

#[derive(Deserialize, Serialize)]
pub struct Package {
    pub name: PackageName,
    pub version: String,
    pub requirements: Vec<String>,
    pub source: Platform,
}

fn resolve_versions<T>(
    _runtime: tokio::runtime::Handle,
    config: &Config,
    _manifest: Option<&Manifest>,
    event_listener: &T,
) -> Result<Manifest, Error>
where
    T: EventListener,
{
    event_listener.handle_event(Event::ResolvingVersions);

    // let resolved = hex::resolve_versions(
    //     PackageFetcher::boxed(runtime.clone()),
    //     mode,
    //     config,
    //     manifest,
    // )?;

    // let packages = runtime.block_on(future::try_join_all(
    //     resolved
    //         .into_iter()
    //         .map(|(name, version)| lookup_package(name, version)),
    // ))?;

    let manifest = Manifest {
        packages: config
            .dependencies
            .iter()
            .map(|dep| Package {
                name: dep.name.clone(),
                version: dep.version.clone(),
                requirements: vec![],
                source: dep.source,
            })
            .collect(),
        requirements: config.dependencies.clone(),
    };

    Ok(manifest)
}
