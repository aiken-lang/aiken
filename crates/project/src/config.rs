use std::{fmt::Display, fs, io, path::PathBuf};

use serde::Deserialize;

#[derive(Deserialize)]
pub struct Config {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub description: String,
    pub repository: Option<Repository>,
}

#[derive(Deserialize)]
pub struct Repository {
    pub user: String,
    pub project: String,
    pub platform: Platform,
}

#[derive(Deserialize)]
pub enum Platform {
    Github,
    Gitlab,
    Bitbucket,
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
