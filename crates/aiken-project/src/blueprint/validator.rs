use super::{
    error::{assert_min_arity, assert_return_bool, Error},
    schema::{Annotated, Schema},
};
use crate::module::{CheckedModule, CheckedModules};
use aiken_lang::{ast::TypedFunction, uplc::CodeGenerator};
use pallas::ledger::primitives::babbage as cardano;
use pallas_traverse::ComputeHash;
use serde::{
    self,
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::fmt::{self, Display};
use uplc::ast::{NamedDeBruijn, Program};

#[derive(Debug, PartialEq, Clone)]
pub struct Validator {
    pub title: String,
    pub purpose: Purpose,
    pub description: Option<String>,
    pub datum: Option<Annotated<Schema>>,
    pub redeemer: Annotated<Schema>,
    pub program: Program<NamedDeBruijn>,
}

#[derive(Debug, PartialEq, Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Purpose {
    Spend,
    Mint,
    Withdraw,
    Publish,
}

impl Serialize for Validator {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let cbor = self.program.to_cbor().unwrap();

        let source_code = hex::encode(&cbor);

        let hash = cardano::PlutusV2Script(cbor.into()).compute_hash();

        let fields = 5
            + self.description.as_ref().map(|_| 1).unwrap_or_default()
            + self.datum.as_ref().map(|_| 1).unwrap_or_default();

        let mut s = serializer.serialize_struct("Validator", fields)?;
        s.serialize_field("title", &self.title)?;
        s.serialize_field("purpose", &self.purpose)?;
        s.serialize_field("hash", &hash)?;
        if let Some { .. } = self.description {
            s.serialize_field("description", &self.description)?;
        }
        if let Some { .. } = self.datum {
            s.serialize_field("datum", &self.datum)?;
        }
        s.serialize_field("redeemer", &self.redeemer)?;
        s.serialize_field("compiledCode", &source_code)?;
        s.end()
    }
}

impl Validator {
    pub fn from_checked_module(
        modules: &CheckedModules,
        generator: &mut CodeGenerator,
        validator: &CheckedModule,
        def: &TypedFunction,
    ) -> Result<Validator, Error> {
        let purpose: Purpose = def.name.clone().into();

        assert_return_bool(validator, def)?;
        assert_min_arity(validator, def, purpose.min_arity())?;

        let mut args = def.arguments.iter().rev();
        let (_, redeemer, datum) = (args.next(), args.next().unwrap(), args.next());

        Ok(Validator {
            title: validator.name.clone(),
            description: None,
            purpose,
            datum: datum
                .map(|datum| {
                    Annotated::from_type(modules.into(), &datum.tipo).map_err(Error::Schema)
                })
                .transpose()?,
            redeemer: Annotated::from_type(modules.into(), &redeemer.tipo)
                .map_err(Error::Schema)?,
            program: generator
                .generate(&def.body, &def.arguments, true)
                .try_into()
                .unwrap(),
        })
    }
}

impl Purpose {
    pub fn min_arity(&self) -> u8 {
        match self {
            Purpose::Spend => 3,
            Purpose::Mint | Purpose::Withdraw | Purpose::Publish => 2,
        }
    }
}

impl Display for Purpose {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Purpose::Spend => "spend",
            Purpose::Mint => "mint",
            Purpose::Withdraw => "withdraw",
            Purpose::Publish => "publish",
        })
    }
}

impl From<String> for Purpose {
    fn from(purpose: String) -> Purpose {
        match &purpose[..] {
            "spend" => Purpose::Spend,
            "mint" => Purpose::Mint,
            "withdraw" => Purpose::Withdraw,
            "publish" => Purpose::Publish,
            unexpected => panic!("Can't turn '{}' into any Purpose", unexpected),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::schema::{Constructor, Data, Schema};
    use super::*;
    use serde_json::{self, json};
    use uplc::parser;

    #[test]
    fn serialize() {
        let program = parser::program("(program 1.0.0 (con integer 42))")
            .unwrap()
            .try_into()
            .unwrap();
        let validator = Validator {
            title: "foo".to_string(),
            description: Some("Lorem ipsum".to_string()),
            purpose: Purpose::Spend,
            datum: None,
            redeemer: Annotated {
                title: Some("Bar".to_string()),
                description: None,
                annotated: Schema::Data(Some(Data::AnyOf(vec![Constructor {
                    index: 0,
                    fields: vec![Data::Bytes.into()],
                }
                .into()]))),
            },
            program,
        };
        assert_eq!(
            serde_json::to_value(&validator).unwrap(),
            json!({
                "title": "foo",
                "purpose": "spend",
                "hash": "27dc8e44c17b4ae5f4b9286ab599fffe70e61b49dec61eaca1fc5898",
                "description": "Lorem ipsum",
                "redeemer": {
                    "title": "Bar",
                    "anyOf": [{
                        "dataType": "constructor",
                        "index": 0,
                        "fields": [{
                            "dataType": "bytes"
                        }]
                    }],
                },
                "compiledCode": "46010000481501"
            }),
        );
    }
}
