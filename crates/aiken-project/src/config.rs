use crate::{package_name::PackageName, Error};
use aiken_lang::ast::Span;
use miette::NamedSource;
use serde::{Deserialize, Serialize};
use std::{
    fmt::Display,
    fs, io,
    path::{Path, PathBuf},
};

#[derive(Deserialize, Serialize)]
pub struct Config {
    pub name: PackageName,
    pub version: String,
    pub license: String,
    #[serde(default)]
    pub description: String,
    pub repository: Option<Repository>,
    #[serde(default)]
    pub dependencies: Vec<Dependency>,
}

#[derive(Deserialize, Serialize)]
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
            license: "Apache-2.0".to_string(),
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
                version: "main".to_string(),
                source: Platform::Github,
            }],
        }
    }

    pub fn save(&self, dir: &Path) -> Result<(), io::Error> {
        let aiken_toml_path = dir.join("aiken.toml");
        let aiken_toml = toml::to_string_pretty(self).unwrap();
        fs::write(aiken_toml_path, aiken_toml)
    }

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
}
