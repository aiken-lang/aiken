use crate::{
    deps::manifest::{Manifest, Package},
    error::Error,
};
use aiken_lang::ast::Span;
use miette::NamedSource;
use serde::{de::Visitor, Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    fs,
    path::PathBuf,
};

#[derive(Deserialize)]
pub struct Config {
    pub name: PackageName,
    pub version: String,
    #[serde(default)]
    pub description: String,
    pub repository: Option<Repository>,
    #[serde(default)]
    pub dependencies: Vec<Dependency>,
}

#[derive(Deserialize)]
pub struct Repository {
    pub user: String,
    pub project: String,
    pub platform: Platform,
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum Platform {
    Github,
    Gitlab,
    Bitbucket,
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone)]
pub struct Dependency {
    pub name: PackageName,
    pub version: String,
    pub source: Platform,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct PackageName {
    pub owner: String,
    pub repo: String,
}

impl Display for PackageName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.owner, self.repo)
    }
}

impl Serialize for PackageName {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for PackageName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct PackageNameVisitor;

        impl<'de> Visitor<'de> for PackageNameVisitor {
            type Value = PackageName;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter
                    .write_str("a string representing an owner and repo, ex: aiken-lang/stdlib")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                let mut name = v.split('/');

                let owner = name.next().ok_or_else(|| {
                    serde::de::Error::invalid_value(serde::de::Unexpected::Str(v), &self)
                })?;

                let repo = name.next().ok_or_else(|| {
                    serde::de::Error::invalid_value(serde::de::Unexpected::Str(v), &self)
                })?;

                Ok(PackageName {
                    owner: owner.to_string(),
                    repo: repo.to_string(),
                })
            }
        }

        deserializer.deserialize_str(PackageNameVisitor)
    }
}

impl Display for Platform {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
        match *self {
            Platform::Github => f.write_str("github"),
            Platform::Gitlab => f.write_str("gitlab"),
            Platform::Bitbucket => f.write_str("bitbucket"),
        }
    }
}

impl Config {
    pub fn load(dir: PathBuf) -> Result<Config, Error> {
        let config_path = dir.join("aiken.toml");
        let raw_config = fs::read_to_string(&config_path)
            .map_err(|_| Error::MissingManifest { path: dir.clone() })?;

        let result: Self = toml::from_str(&raw_config).map_err(|e| Error::TomlLoading {
            path: config_path.clone(),
            src: raw_config.clone(),
            named: NamedSource::new(config_path.display().to_string(), raw_config),
            // this isn't actually a legit way to get the span
            location: e.line_col().map(|(line, col)| Span {
                start: line,
                end: col,
            }),
            help: e.to_string(),
        })?;

        Ok(result)
    }

    /// Get the locked packages for the current config and a given (optional)
    /// manifest of previously locked packages.
    ///
    /// If a package is removed or the specified required version range for it
    /// changes then it is not considered locked. This also goes for any child
    /// packages of the package which have no other parents.
    ///
    /// This function should be used each time resolution is performed so that
    /// outdated deps are removed from the manifest and not locked to the
    /// previously selected versions.
    ///
    pub fn locked(&self, manifest: Option<&Manifest>) -> Result<Vec<Package>, Error> {
        Ok(match manifest {
            None => vec![],
            Some(manifest) => StalePackageRemover::fresh_and_locked(&self.dependencies, manifest),
        })
    }
}

#[derive(Debug)]
struct StalePackageRemover<'a> {
    // These are the packages for which the requirement or their parents
    // requirement has not changed.
    fresh: HashSet<String>,
    locked: HashMap<String, &'a Vec<String>>,
}

impl<'a> StalePackageRemover<'a> {
    pub fn fresh_and_locked(
        requirements: &'a Vec<Dependency>,
        manifest: &'a Manifest,
    ) -> Vec<Package> {
        let locked = manifest
            .packages
            .iter()
            .map(|p| (p.name.to_string(), &p.requirements))
            .collect();

        Self {
            fresh: HashSet::new(),
            locked,
        }
        .run(requirements, manifest)
    }

    fn run(&mut self, requirements: &'a Vec<Dependency>, manifest: &'a Manifest) -> Vec<Package> {
        // Record all the requirements that have not changed
        for dep in requirements {
            if manifest.requirements.iter().find(|d| d.name == dep.name) != Some(dep) {
                continue; // This package has changed, don't record it
            }

            // Recursively record the package and its deps as being fresh
            self.record_tree_fresh(dep.name.to_string());
        }

        // Return all the previously resolved packages that have not been
        // recorded as fresh
        manifest
            .packages
            .iter()
            .filter(|package| self.fresh.contains(package.name.to_string().as_str()))
            .cloned()
            .collect()
    }

    fn record_tree_fresh(&mut self, name: String) {
        let deps = self
            .locked
            .get(&name)
            .expect("Package fresh but not in manifest");

        // Record the top level package
        let _ = self.fresh.insert(name);

        // Record each of its deps recursively
        for package in *deps {
            self.record_tree_fresh(package.clone());
        }
    }
}
