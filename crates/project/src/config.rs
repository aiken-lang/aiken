use std::{fmt::Display, fs, path::PathBuf};

use aiken_lang::ast::Span;
use miette::NamedSource;
use serde::{de::Visitor, Deserialize, Serialize};

use crate::error::Error;

#[derive(Deserialize)]
pub struct Config {
    pub name: PackageName,
    pub version: String,
    #[serde(default)]
    pub description: String,
    pub repository: Option<Repository>,
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
        let raw_config = fs::read_to_string(&config_path)?;

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
