pub mod definitions;
pub mod error;
mod memo_program;
pub mod parameter;
pub mod schema;
pub mod validator;

pub use error::Error;

use crate::{
    config::{self, Config, PlutusVersion},
    module::CheckedModules,
};
use aiken_lang::gen_uplc::CodeGenerator;
use definitions::Definitions;
use schema::{Annotated, Schema};
use std::fmt::Debug;
use validator::Validator;

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Blueprint {
    pub preamble: Preamble,
    pub validators: Vec<Validator>,
    #[serde(skip_serializing_if = "Definitions::is_empty", default)]
    pub definitions: Definitions<Annotated<Schema>>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Preamble {
    pub title: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    pub version: String,

    pub plutus_version: PlutusVersion,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub compiler: Option<Compiler>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Compiler {
    pub name: String,
    pub version: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LookupResult<'a, T> {
    One(&'a T),
    Many,
}

impl Blueprint {
    pub fn new(
        config: &Config,
        modules: &CheckedModules,
        generator: &mut CodeGenerator,
    ) -> Result<Self, Error> {
        let preamble = config.into();

        let mut definitions = Definitions::new();

        let validators: Result<Vec<_>, Error> = modules
            .validators()
            .flat_map(|(validator, def)| {
                Validator::from_checked_module(modules, generator, validator, def)
                    .into_iter()
                    .map(|result| {
                        result.map(|mut schema| {
                            definitions.merge(&mut schema.definitions);
                            schema.definitions = Definitions::new();
                            schema
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .collect();

        Ok(Blueprint {
            preamble,
            validators: validators?,
            definitions,
        })
    }
}

impl Blueprint {
    pub fn lookup(&self, title: Option<&String>) -> Option<LookupResult<Validator>> {
        let mut validator = None;

        for v in self.validators.iter() {
            let match_title = Some(&v.title) == title.or(Some(&v.title));
            if match_title {
                validator = Some(if validator.is_none() {
                    LookupResult::One(v)
                } else {
                    LookupResult::Many
                })
            }
        }

        validator
    }

    pub fn with_validator<F, A, E>(
        &self,
        title: Option<&String>,
        when_too_many: fn(Vec<String>) -> E,
        when_missing: fn(Vec<String>) -> E,
        action: F,
    ) -> Result<A, E>
    where
        F: Fn(Validator) -> Result<A, E>,
    {
        match self.lookup(title) {
            Some(LookupResult::One(validator)) => action(validator.to_owned()),
            Some(LookupResult::Many) => Err(when_too_many(
                self.validators.iter().map(|v| v.title.clone()).collect(),
            )),
            None => Err(when_missing(
                self.validators.iter().map(|v| v.title.clone()).collect(),
            )),
        }
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
            compiler: Some(Compiler {
                name: "Aiken".to_string(),
                version: config::compiler_version(true),
            }),
            plutus_version: config.plutus,
            version: config.version.clone(),
            license: config.license.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use aiken_lang::builtins;
    use schema::{Data, Declaration, Items, Schema};
    use serde_json::{self, json};
    use std::collections::HashMap;

    #[test]
    fn serialize_no_description() {
        let blueprint = Blueprint {
            preamble: Preamble {
                title: "Foo".to_string(),
                description: None,
                version: "1.0.0".to_string(),
                plutus_version: PlutusVersion::V2,
                compiler: Some(Compiler {
                    name: "Aiken".to_string(),
                    version: "1.0.0".to_string(),
                }),
                license: Some("Apache-2.0".to_string()),
            },
            validators: vec![],
            definitions: Definitions::new(),
        };
        assert_eq!(
            serde_json::to_value(&blueprint).unwrap(),
            json!({
                "preamble": {
                    "title": "Foo",
                    "version": "1.0.0",
                    "plutusVersion": "v2",
                    "compiler": {
                        "name": "Aiken",
                        "version": "1.0.0"
                    },
                    "license": "Apache-2.0"
                },
                "validators": []
            }),
        );
    }

    #[test]
    fn serialize_with_description() {
        let blueprint = Blueprint {
            preamble: Preamble {
                title: "Foo".to_string(),
                description: Some("Lorem ipsum".to_string()),
                version: "1.0.0".to_string(),
                plutus_version: PlutusVersion::V2,
                compiler: None,
                license: None,
            },
            validators: vec![],
            definitions: Definitions::new(),
        };
        assert_eq!(
            serde_json::to_value(&blueprint).unwrap(),
            json!({
                "preamble": {
                    "title": "Foo",
                    "description": "Lorem ipsum",
                    "version": "1.0.0",
                    "plutusVersion": "v2"
                },
                "validators": []
            }),
        );
    }

    #[test]
    fn serialize_with_definitions() {
        let mut definitions = Definitions::new();
        definitions
            .register::<_, Error>(&builtins::int(), &HashMap::new(), |_| {
                Ok(Schema::Data(Data::Integer).into())
            })
            .unwrap();
        definitions
            .register::<_, Error>(
                &builtins::list(builtins::byte_array()),
                &HashMap::new(),
                |definitions| {
                    let ref_bytes = definitions.register::<_, Error>(
                        &builtins::byte_array(),
                        &HashMap::new(),
                        |_| Ok(Schema::Data(Data::Bytes).into()),
                    )?;
                    Ok(
                        Schema::Data(Data::List(Items::One(Declaration::Referenced(ref_bytes))))
                            .into(),
                    )
                },
            )
            .unwrap();

        let blueprint = Blueprint {
            preamble: Preamble {
                title: "Foo".to_string(),
                description: None,
                version: "1.0.0".to_string(),
                plutus_version: PlutusVersion::V2,
                compiler: None,
                license: None,
            },
            validators: vec![],
            definitions,
        };
        assert_eq!(
            serde_json::to_value(&blueprint).unwrap(),
            json!({
                "preamble": {
                    "title": "Foo",
                    "version": "1.0.0",
                    "plutusVersion": "v2"
                },
                "validators": [],
                "definitions": {
                    "ByteArray": {
                        "dataType": "bytes"
                    },
                    "Int": {
                        "dataType": "integer"
                    },
                    "List$ByteArray": {
                        "dataType": "list",
                        "items": {
                            "$ref": "#/definitions/ByteArray"
                        }
                    }
                }
            }),
        );
    }
}
