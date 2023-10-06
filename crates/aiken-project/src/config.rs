use crate::{github::repo::LatestRelease, package_name::PackageName, paths, Error};
use aiken_lang::ast::Span;
use miette::NamedSource;
use serde::{Deserialize, Serialize};
use std::{fmt::Display, fs, io, path::Path};

#[derive(Deserialize, Serialize, Clone)]
pub struct Config {
    pub name: PackageName,
    pub version: String,
    pub license: Option<String>,
    #[serde(default)]
    pub description: String,
    pub repository: Option<Repository>,
    #[serde(default)]
    pub dependencies: Vec<Dependency>,
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Repository {
    pub user: String,
    pub project: String,
    pub platform: Platform,
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone, Copy, Debug)]
#[serde(rename_all = "lowercase")]
pub enum Platform {
    Github,
    Gitlab,
    Bitbucket,
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone, Debug)]
pub struct Dependency {
    pub name: PackageName,
    pub version: String,
    pub source: Platform,
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
    pub fn default(name: &PackageName) -> Self {
        Config {
            name: name.clone(),
            version: "0.0.0".to_string(),
            license: Some("Apache-2.0".to_string()),
            description: format!("Aiken contracts for project '{name}'"),
            repository: Some(Repository {
                user: name.owner.clone(),
                project: name.repo.clone(),
                platform: Platform::Github,
            }),
            dependencies: vec![Dependency {
                name: PackageName {
                    owner: "aiken-lang".to_string(),
                    repo: "stdlib".to_string(),
                },
                version: match LatestRelease::of("aiken-lang/stdlib") {
                    Ok(stdlib) => stdlib.tag_name,
                    _ => "1.5.0".to_string(),
                },
                source: Platform::Github,
            }],
        }
    }

    pub fn save(&self, dir: &Path) -> Result<(), io::Error> {
        let aiken_toml_path = dir.join(paths::project_config());
        let aiken_toml = toml::to_string_pretty(self).unwrap();
        fs::write(aiken_toml_path, aiken_toml)
    }

    pub fn load(dir: &Path) -> Result<Config, Error> {
        let config_path = dir.join(paths::project_config());
        let raw_config = fs::read_to_string(&config_path).map_err(|_| Error::MissingManifest {
            path: dir.to_path_buf(),
        })?;

        let result: Self = toml::from_str(&raw_config).map_err(|e| Error::TomlLoading {
            path: config_path.clone(),
            src: raw_config.clone(),
            named: NamedSource::new(config_path.display().to_string(), raw_config).into(),
            // this isn't actually a legit way to get the span
            location: e.span().map(|range| Span {
                start: range.start,
                end: range.end,
            }),
            help: e.to_string(),
        })?;

        Ok(result)
    }

    pub fn insert(mut self, dependency: &Dependency, and_replace: bool) -> Option<Self> {
        for existing in self.dependencies.iter_mut() {
            if existing.name == dependency.name {
                return if and_replace {
                    existing.version = dependency.version.clone();
                    Some(self)
                } else {
                    None
                };
            }
        }
        self.dependencies.push(dependency.clone());
        Some(self)
    }
}

mod built_info {
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}

pub fn compiler_version(include_commit_hash: bool) -> String {
    if include_commit_hash {
        format!(
            "v{} {}",
            built_info::PKG_VERSION_MAJOR,
            built_info::GIT_COMMIT_HASH_SHORT.unwrap_or("unknown")
        )
    } else {
        format!("v{}", built_info::PKG_VERSION_MAJOR,)
    }
}
