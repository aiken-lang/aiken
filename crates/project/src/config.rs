use std::{fmt::Display, fs, io, path::PathBuf};

use serde::{de::Visitor, Deserialize, Serialize};

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

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Platform {
    Github,
    Gitlab,
    Bitbucket,
}

#[derive(Deserialize, Serialize)]
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
    pub fn load(dir: PathBuf) -> io::Result<Config> {
        let raw_config = fs::read_to_string(dir.join("aiken.toml"))?;

        let config = toml::from_str(&raw_config).unwrap();

        Ok(config)
    }
}
