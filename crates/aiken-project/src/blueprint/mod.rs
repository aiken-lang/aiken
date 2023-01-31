pub mod error;
pub mod schema;
pub mod validator;

use crate::{config::Config, module::CheckedModules};
use aiken_lang::uplc::CodeGenerator;
use error::Error;
use schema::Schema;
use std::fmt::{self, Debug, Display};
use validator::Validator;

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Blueprint<T> {
    pub preamble: Preamble,
    pub validators: Vec<validator::Validator<T>>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Preamble {
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub version: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
}

impl Blueprint<Schema> {
    pub fn new(
        config: &Config,
        modules: &CheckedModules,
        generator: &mut CodeGenerator,
    ) -> Result<Self, Error> {
        let preamble = config.into();

        let validators: Result<Vec<_>, Error> = modules
            .validators()
            .map(|(validator, def)| {
                Validator::from_checked_module(modules, generator, validator, def)
            })
            .collect();

        Ok(Blueprint {
            preamble,
            validators: validators?,
        })
    }
}

impl Display for Blueprint<Schema> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = serde_json::to_string_pretty(self).map_err(|_| fmt::Error)?;
        f.write_str(&s)
    }
}

impl From<&Config> for Preamble {
    fn from(config: &Config) -> Self {
        Preamble {
            title: config.name.to_string(),
            description: if config.description.is_empty() {
                None
            } else {
                Some(config.description.clone())
            },
            version: config.version.clone(),
            license: config.license.clone(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use serde_json::{self, json};

    #[test]
    fn serialize_no_description() {
        let blueprint: Blueprint<Schema> = Blueprint {
            preamble: Preamble {
                title: "Foo".to_string(),
                description: None,
                version: "1.0.0".to_string(),
                license: Some("Apache-2.0".to_string()),
            },
            validators: vec![],
        };
        assert_eq!(
            serde_json::to_value(&blueprint).unwrap(),
            json!({
                "preamble": {
                    "title": "Foo",
                    "version": "1.0.0",
                    "license": "Apache-2.0"
                },
                "validators": []
            }),
        );
    }

    #[test]
    fn serialize_with_description() {
        let blueprint: Blueprint<Schema> = Blueprint {
            preamble: Preamble {
                title: "Foo".to_string(),
                description: Some("Lorem ipsum".to_string()),
                version: "1.0.0".to_string(),
                license: None,
            },
            validators: vec![],
        };
        assert_eq!(
            serde_json::to_value(&blueprint).unwrap(),
            json!({
                "preamble": {
                    "title": "Foo",
                    "description": "Lorem ipsum",
                    "version": "1.0.0",
                },
                "validators": []
            }),
        );
    }
}
