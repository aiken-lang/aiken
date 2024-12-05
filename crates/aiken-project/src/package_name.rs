use owo_colors::{OwoColorize, Stream::Stdout};
use serde::{de::Visitor, Deserialize, Serialize};
use std::{
    fmt::{self, Display},
    str::FromStr,
};
use thiserror::Error;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct PackageName {
    pub owner: String,
    pub repo: String,
}

impl PackageName {
    fn validate(&self) -> Result<(), Error> {
        let r = regex::Regex::new("^[a-z0-9_-]+$").expect("regex could not be compiled");

        if !(r.is_match(&self.owner) && r.is_match(&self.repo)) {
            return Err(Error::InvalidProjectName {
                reason: InvalidProjectNameReason::Format,
                name: self.to_string(),
            });
        }

        Ok(())
    }
}

impl FromStr for PackageName {
    type Err = Error;

    fn from_str(name: &str) -> Result<Self, Error> {
        let mut name_split = name.split('/');
        let owner = name_split
            .next()
            .ok_or_else(|| Error::InvalidProjectName {
                name: name.to_string(),
                reason: InvalidProjectNameReason::Format,
            })?
            .to_string();
        let repo = name_split
            .next()
            .ok_or_else(|| Error::InvalidProjectName {
                name: name.to_string(),
                reason: InvalidProjectNameReason::Format,
            })?
            .to_string();
        let package_name = PackageName { owner, repo };
        package_name.validate()?;
        Ok(package_name)
    }
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

        impl Visitor<'_> for PackageNameVisitor {
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

#[derive(Debug, Error, miette::Diagnostic)]
pub enum Error {
    #[error(
        "{} is not a valid project name: {}",
        name.if_supports_color(Stdout, |s| s.red()),
        reason.to_string()
    )]
    InvalidProjectName {
        name: String,
        reason: InvalidProjectNameReason,
    },
    #[error(
        "A project named {} already exists.",
        name.if_supports_color(Stdout, |s| s.red())
    )]
    ProjectExists { name: String },
}

#[derive(Debug, Clone, Copy)]
pub enum InvalidProjectNameReason {
    Reserved,
    Format,
}

impl fmt::Display for InvalidProjectNameReason {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InvalidProjectNameReason::Reserved => write!(f, "It's a reserved word in Aiken."),
            InvalidProjectNameReason::Format => write!(
                f,
                "It is malformed.\n\nProjects must be named as:\n\n\t\
                {}/{}\n\nEach part must start with a lowercase letter \
                and may only contain lowercase letters, numbers, hyphens or underscores.\
                \nFor example,\n\n\t{}",
                "{owner}".if_supports_color(Stdout, |s| s.bright_blue()),
                "{project}".if_supports_color(Stdout, |s| s.bright_blue()),
                "aiken-lang/stdlib".if_supports_color(Stdout, |s| s.bright_blue()),
            ),
        }
    }
}
