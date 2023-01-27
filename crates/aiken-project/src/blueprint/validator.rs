use super::schema::NamedSchema;
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
    pub datum: Option<NamedSchema>,
    pub redeemer: NamedSchema,
    pub program: Program<NamedDeBruijn>,
}

impl Serialize for Validator {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let cbor = self.program.to_cbor().unwrap();
        let source_code = hex::encode(&cbor);
        let mut s = serializer.serialize_struct("Validator", 5)?;
        s.serialize_field("title", &self.title)?;
        s.serialize_field("purpose", &self.purpose)?;
        let hash = cardano::PlutusV2Script(cbor.into()).compute_hash();
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

#[derive(Debug, PartialEq, Clone, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Purpose {
    Spend,
    Mint,
    Withdraw,
    Publish,
}

impl Purpose {
    pub fn min_arity(&self) -> u8 {
        match self {
            Purpose::Spend => 3,
            Purpose::Mint | Purpose::Withdraw | Purpose::Publish => 2,
        }
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

#[cfg(test)]
mod test {
    use super::super::schema::{Constructor, Schema};
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
            redeemer: NamedSchema {
                title: "Bar".to_string(),
                description: None,
                schema: Schema::AnyOf(vec![Constructor {
                    index: 0,
                    fields: vec![Schema::Bytes],
                }]),
            },
            program,
        };
        assert_eq!(
            serde_json::to_value(&validator).unwrap(),
            json!({
                "title": "foo",
                "description": "Lorem ipsum",
                "purpose": "spend",
                "redeemer": {
                    "title": "Bar",
                    "dataType": "constructor",
                    "index": 0,
                    "fields": [{
                        "dataType": "bytes"
                    }]
                },
                "compiledCode": "46010000481501",
                "hash": "27dc8e44c17b4ae5f4b9286ab599fffe70e61b49dec61eaca1fc5898"
            }),
        );
    }
}
