use serde::{Deserialize, Serialize};
use std::{fmt, str::FromStr};

/// A portable, repository-relative path to an Aiken package.
///
/// Use forward slashes between components. Paths are deliberately canonical:
/// absolute paths, empty components, `.` and `..` are not accepted.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct PackageSubdir(String);

impl PackageSubdir {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl FromStr for PackageSubdir {
    type Err = &'static str;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        if value.contains(['\\', ':'])
            || value.chars().any(char::is_control)
            || value.split('/').any(|part| matches!(part, "" | "." | ".."))
        {
            return Err(
                "subdir must be a relative path with forward slashes and no empty, '.' or '..' components, backslashes, colons or control characters",
            );
        }

        Ok(Self(value.to_owned()))
    }
}

impl TryFrom<String> for PackageSubdir {
    type Error = &'static str;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl From<PackageSubdir> for String {
    fn from(value: PackageSubdir) -> Self {
        value.0
    }
}

impl fmt::Display for PackageSubdir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_relative_paths() {
        for subdir in ["onchain", "packages/on-chain", "a/b/c"] {
            assert_eq!(subdir.parse::<PackageSubdir>().unwrap().as_str(), subdir);
        }
    }

    #[test]
    fn rejects_paths_that_are_not_canonical_or_portable() {
        for subdir in [
            "",
            ".",
            "..",
            "../outside",
            "onchain/../other",
            "./onchain",
            "onchain/",
            "onchain//lib",
            "/absolute",
            "C:/absolute",
            r"onchain\lib",
            "bad\0path",
        ] {
            assert!(
                subdir.parse::<PackageSubdir>().is_err(),
                "accepted {subdir:?}"
            );
        }
    }
}
