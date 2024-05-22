use std::{fmt::Display, fs, io, path::Path};

use crate::{github::repo::LatestRelease, package_name::PackageName, paths, Error};
use aiken_lang::ast::Span;
use semver::Version;

use miette::NamedSource;
use serde::{Deserialize, Serialize};

pub use aiken_lang::plutus_version::PlutusVersion;

#[derive(Deserialize, Serialize, Clone)]
pub struct Config {
    pub name: PackageName,
    pub version: String,
    #[serde(
        deserialize_with = "deserialize_version",
        serialize_with = "serialize_version",
        default = "default_version"
    )]
    pub compiler: Version,
    #[serde(default)]
    pub plutus: PlutusVersion,
    pub license: Option<String>,
    #[serde(default)]
    pub description: String,
    pub repository: Option<Repository>,
    #[serde(default)]
    pub dependencies: Vec<Dependency>,
}

fn deserialize_version<'de, D>(deserializer: D) -> Result<Version, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let buf = String::deserialize(deserializer)?.replace('v', "");

    Version::parse(&buf).map_err(serde::de::Error::custom)
}

fn serialize_version<S>(version: &Version, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let version = format!("v{}", version);

    serializer.serialize_str(&version)
}

fn default_version() -> Version {
    Version::parse(built_info::PKG_VERSION).unwrap()
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
            compiler: default_version(),
            plutus: PlutusVersion::default(),
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
                    existing.version.clone_from(&dependency.version);
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
            "v{}+{}",
            built_info::PKG_VERSION,
            built_info::GIT_COMMIT_HASH_SHORT.unwrap_or("unknown")
        )
    } else {
        format!("v{}", built_info::PKG_VERSION,)
    }
}

pub fn compiler_info() -> String {
    format!(
        r#"
Operating System: {}
Architecture:     {}
Version:          {}"#,
        built_info::CFG_OS,
        built_info::CFG_TARGET_ARCH,
        compiler_version(true),
    )
}
