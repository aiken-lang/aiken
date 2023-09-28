pub mod definitions;
pub mod parameter;
pub mod schema;
pub mod validator;

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
    pub license: Option<String>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum PlutusVersion {
    V1,
    V2,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LookupResult<'a, T> {
    One(&'a T),
    Many,
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
