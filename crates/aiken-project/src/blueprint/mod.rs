pub mod definitions;
pub mod error;
mod memo_program;
pub mod parameter;
pub mod schema;
pub mod source_map;
pub mod validator;

use crate::{
    config::{self, PlutusVersion, ProjectConfig},
    module::CheckedModules,
};
use aiken_lang::gen_uplc::CodeGenerator;
use definitions::Definitions;
pub use error::Error;
use pallas_addresses::ScriptHash;
use schema::{Annotated, Schema};
use std::{
    collections::{BTreeSet, HashMap},
    fmt::Debug,
};
use uplc::{PlutusData, ast::SerializableProgram, tx::script_context::PlutusScript};
use validator::Validator;

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Blueprint {
    pub preamble: Preamble,
    pub validators: Vec<Validator<SerializableProgram>>,
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
    One(String, &'a T),
    Many,
}

impl Blueprint {
    pub fn new(
        config: &ProjectConfig,
        modules: &CheckedModules,
        generator: &mut CodeGenerator,
        source_map_mode: &crate::options::SourceMapMode,
        module_sources: &indexmap::IndexMap<&str, &(String, aiken_lang::line_numbers::LineNumbers)>,
    ) -> Result<Self, Error> {
        let preamble = config.into();

        let mut definitions = Definitions::new();

        let validators: Result<Vec<_>, Error> = modules
            .validators()
            .map(|(validator, def)| {
                Ok(Validator::from_checked_module(
                    modules,
                    generator,
                    validator,
                    def,
                    &config.plutus,
                    source_map_mode,
                    module_sources,
                )?
                .into_iter()
                .map(|mut schema| {
                    definitions.merge(&mut schema.definitions);
                    schema.definitions = Definitions::new();
                    schema
                })
                .collect::<Vec<_>>())
            })
            .collect();

        Ok(Blueprint {
            preamble,
            validators: validators?.into_iter().flatten().collect(),
            definitions,
        })
    }
}

impl Blueprint {
    pub fn lookup(
        &self,
        want_module_name: Option<&str>,
        want_validator_name: Option<&str>,
    ) -> Option<LookupResult<'_, Validator<SerializableProgram>>> {
        let mut validator = None;

        for v in self.validators.iter() {
            let (known_module_name, known_validator_name) = v.get_module_and_name();

            let is_target = match (want_module_name, want_validator_name) {
                (None, None) => true,
                (Some(want_module_name), None) => want_module_name == known_module_name,
                (None, Some(want_validator_name)) => want_validator_name == known_validator_name,
                (Some(want_module_name), Some(want_validator_name)) => {
                    want_module_name == known_module_name
                        && want_validator_name == known_validator_name
                }
            };

            let title = format!("{known_module_name}.{known_validator_name}");

            if is_target {
                match validator {
                    Some(LookupResult::Many) => (),
                    None => {
                        validator = Some(LookupResult::One(title, v));
                    }
                    Some(LookupResult::One(ref known_title, _)) => {
                        if title.as_str() != known_title {
                            validator = Some(LookupResult::Many)
                        }
                    }
                }
            }
        }

        validator
    }

    pub fn construct_parameter_incrementally<F>(
        &self,
        module_name: Option<&str>,
        validator_name: Option<&str>,
        ask: F,
    ) -> Result<PlutusData, Error>
    where
        F: Fn(&Annotated<Schema>, &Definitions<Annotated<Schema>>) -> Result<PlutusData, Error>,
    {
        // Construct parameter
        let when_too_many =
            |known_validators| Error::MoreThanOneValidatorFound { known_validators };
        let when_missing = |known_validators| Error::NoValidatorNotFound { known_validators };

        self.with_validator(
            module_name,
            validator_name,
            when_too_many,
            when_missing,
            |validator| validator.ask_next_parameter(&self.definitions, &ask),
        )
    }

    pub fn apply_parameter(
        &mut self,
        module_name: Option<&str>,
        validator_name: Option<&str>,
        param: &PlutusData,
    ) -> Result<(), Error> {
        let when_too_many =
            |known_validators| Error::MoreThanOneValidatorFound { known_validators };
        let when_missing = |known_validators| Error::NoValidatorNotFound { known_validators };

        let applied_validator = self.with_validator(
            module_name,
            validator_name,
            when_too_many,
            when_missing,
            |validator| validator.clone().apply(&self.definitions, param),
        )?;

        // Overwrite validator
        let prefix = |v: &str| v.split('.').take(2).collect::<Vec<&str>>().join(".");
        for validator in &mut self.validators {
            if prefix(&applied_validator.title) == prefix(&validator.title) {
                validator.program = applied_validator.program.clone();
                validator.parameters = applied_validator.parameters.clone();
            }
        }

        Ok(())
    }

    pub fn with_validator<F, A, E>(
        &self,
        module_name: Option<&str>,
        validator_name: Option<&str>,
        when_too_many: fn(BTreeSet<(String, String, bool)>) -> E,
        when_missing: fn(BTreeSet<(String, String, bool)>) -> E,
        action: F,
    ) -> Result<A, E>
    where
        F: Fn(&Validator<SerializableProgram>) -> Result<A, E>,
    {
        match self.lookup(module_name, validator_name) {
            Some(LookupResult::One(_, validator)) => action(validator),
            Some(LookupResult::Many) => Err(when_too_many(
                self.validators
                    .iter()
                    .filter_map(|v| {
                        let (l, r) = v.get_module_and_name();

                        if let Some(module_name) = module_name {
                            if l != module_name {
                                return None;
                            }
                        }

                        if let Some(validator_name) = validator_name {
                            if r != validator_name {
                                return None;
                            }
                        }

                        Some((l.to_string(), r.to_string(), !v.parameters.is_empty()))
                    })
                    .collect(),
            )),
            None => Err(when_missing(
                self.validators
                    .iter()
                    .map(|v| {
                        let (l, r) = v.get_module_and_name();
                        (l.to_string(), r.to_string(), !v.parameters.is_empty())
                    })
                    .collect(),
            )),
        }
    }
}

impl From<Blueprint> for HashMap<ScriptHash, PlutusScript> {
    fn from(blueprint: Blueprint) -> Self {
        blueprint
            .validators
            .iter()
            .map(|validator| validator.program.compiled_code_and_hash())
            .collect()
    }
}
impl From<&ProjectConfig> for Preamble {
    fn from(config: &ProjectConfig) -> Self {
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
    use aiken_lang::tipo::Type;
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
            .register::<_, Error>(&Type::int(), &HashMap::new(), |_| {
                Ok(Schema::Data(Data::Integer).into())
            })
            .unwrap();
        definitions
            .register::<_, Error>(
                &Type::list(Type::byte_array()),
                &HashMap::new(),
                |definitions| {
                    let ref_bytes = definitions.register::<_, Error>(
                        &Type::byte_array(),
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
                    "List<ByteArray>": {
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
