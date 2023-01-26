use serde::ser::{Serialize, SerializeStruct, Serializer};
use serde_json;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Schema {
    Integer,
    Bytes,
    List(Item<Box<Schema>>),
    Map((Box<Schema>, Box<Schema>)),
    Constructor(usize, Vec<Schema>),
}

impl Display for Schema {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = serde_json::to_string_pretty(self).map_err(|_| fmt::Error)?;
        f.write_str(&s)
    }
}

impl Serialize for Schema {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Schema::Integer => {
                let mut s = serializer.serialize_struct("Integer", 1)?;
                s.serialize_field("dataType", "integer")?;
                s.end()
            }
            Schema::Bytes => {
                let mut s = serializer.serialize_struct("Bytes", 1)?;
                s.serialize_field("dataType", "bytes")?;
                s.end()
            }
            Schema::List(items) => {
                let mut s = serializer.serialize_struct("List", 2)?;
                s.serialize_field("dataType", "list")?;
                s.serialize_field("items", &items)?;
                s.end()
            }
            Schema::Map(elements) => {
                let mut s = serializer.serialize_struct("Map", 2)?;
                s.serialize_field("dataType", "map")?;
                s.serialize_field("elements", &elements)?;
                s.end()
            }
            _ => {
                todo!()
            }
        }
    }
}

// Represent a items list in a JSON schema. Can be either a singleton (i.e. a single schema) when
// all elements in the list are uniform or a list of schemas.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Item<T> {
    Singleton(T),
    Many(Vec<T>),
}

impl<T: Serialize> Serialize for Item<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Item::Singleton(elem) => Serialize::serialize(elem, serializer),
            Item::Many(elems) => Serialize::serialize(elems, serializer),
        }
    }
}

pub mod test {
    use super::*;
    #[allow(unused_imports)]
    use serde_json::{self, json, Value};

    pub fn assert_json(schema: &Schema, expected: Value) {
        assert_eq!(serde_json::to_value(schema).unwrap(), expected);
    }

    #[test]
    fn serialize_integer() {
        let schema = Schema::Integer;
        assert_json(
            &schema,
            json!({
                "dataType": "integer"
            }),
        );
    }

    #[test]
    fn serialize_bytes() {
        let schema = Schema::Bytes;
        assert_json(
            &schema,
            json!({
                "dataType": "bytes"
            }),
        );
    }

    #[test]
    fn serialize_list_1() {
        let schema = Schema::List(Item::Many(vec![]));
        assert_json(
            &schema,
            json!({
                "dataType": "list",
                "items": []
            }),
        );
    }

    #[test]
    fn serialize_list_2() {
        let schema = Schema::List(Item::Singleton(Box::new(Schema::Integer)));
        assert_json(
            &schema,
            json!({
                "dataType": "list",
                "items": {
                    "dataType": "integer"
                }
            }),
        );
    }

    #[test]
    fn serialize_list_3() {
        let schema = Schema::List(Item::Many(vec![
            Box::new(Schema::Bytes),
            Box::new(Schema::List(Item::Singleton(Box::new(Schema::Integer)))),
        ]));
        assert_json(
            &schema,
            json!({
                "dataType": "list",
                "items": [
                    {
                        "dataType": "bytes"
                    },
                    {
                        "dataType": "list",
                        "items": { "dataType": "integer" }
                    }
                ]
            }),
        );
    }

    #[test]
    fn serialize_map_1() {
        let schema = Schema::Map((Box::new(Schema::Integer), Box::new(Schema::Bytes)));
        assert_json(
            &schema,
            json!({
                "dataType": "map",
                "elements": [
                    {
                        "dataType": "integer"
                    },
                    {
                        "dataType": "bytes"
                    }
                ]
            }),
        )
    }

    #[test]
    fn serialize_map_2() {
        let schema = Schema::Map((
            Box::new(Schema::Bytes),
            Box::new(Schema::List(Item::Singleton(Box::new(Schema::Integer)))),
        ));
        assert_json(
            &schema,
            json!({
                "dataType": "map",
                "elements": [
                    {
                        "dataType": "bytes"
                    },
                    {
                        "dataType": "list",
                        "items": { "dataType": "integer" }
                    }
                ]
            }),
        )
    }
}
