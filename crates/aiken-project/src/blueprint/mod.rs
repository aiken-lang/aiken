pub mod error;
pub mod schema;
pub mod validator;

use crate::{config::Config, module::CheckedModules};
use aiken_lang::uplc::CodeGenerator;
use error::*;
use schema::Schema;
use std::fmt::Debug;
use validator::{Purpose, Validator};

#[derive(Debug, PartialEq, Clone, serde::Serialize)]
pub struct Blueprint {
    pub preamble: Preamble,
    pub validators: Vec<validator::Validator>,
}

impl Blueprint {
    pub fn new(
        config: &Config,
        modules: &CheckedModules,
        generator: &mut CodeGenerator,
    ) -> Result<Self, Error> {
        let mut validators = Vec::new();

        for (validator, def) in modules.validators() {
            let purpose: Purpose = def.name.clone().into();

            assert_return_bool(validator, def)?;
            assert_min_arity(validator, def, purpose.min_arity())?;

            let mut args = def.arguments.iter().rev();
            let (_, redeemer, datum) = (args.next(), args.next().unwrap(), args.next());

            validators.push(Validator {
                description: None,
                purpose,
                datum: datum
                    .map(|datum| {
                        Schema::from_type(modules.into(), &datum.arg_name.get_label(), &datum.tipo)
                            .map_err(Error::Schema)
                    })
                    .transpose()?,
                redeemer: Schema::from_type(
                    modules.into(),
                    &redeemer.arg_name.get_label(),
                    &redeemer.tipo,
                )
                .map_err(Error::Schema)?,
                program: generator
                    .generate(&def.body, &def.arguments, true)
                    .try_into()
                    .unwrap(),
            });
        }

        Ok(Blueprint {
            preamble: Preamble::from_config(config),
            validators,
        })
    }
}

#[derive(Debug, PartialEq, Clone, serde::Serialize)]
pub struct Preamble {
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub version: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
}

impl Preamble {
    pub fn from_config(config: &Config) -> Self {
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
        let blueprint = Blueprint {
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
        let blueprint = Blueprint {
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
