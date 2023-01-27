use crate::CheckedModule;
use aiken_lang::{
    ast::{DataType, Definition, TypedDefinition},
    tipo::Type,
};
use miette::Diagnostic;
use serde::{
    self,
    ser::{Serialize, SerializeStruct, Serializer},
};
use serde_json;
use std::{
    collections::HashMap,
    fmt::{self, Display},
    sync::Arc,
};

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize)]
pub struct NamedSchema {
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(flatten)]
    pub schema: Schema,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Schema {
    Integer,
    Bytes,
    List(Item<Box<Schema>>),
    Map((Box<Schema>, Box<Schema>)),
    AnyOf(Vec<Constructor>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Constructor {
    pub index: usize,
    pub fields: Vec<NamedSchema>,
}

impl NamedSchema {
    pub fn from_type(
        modules: &HashMap<String, CheckedModule>,
        name: &str,
        type_info: &Type,
    ) -> Result<Self, Error> {
        match type_info {
            Type::App {
                module: module_name,
                name: type_name,
                ..
            } if module_name.is_empty() => match &type_name[..] {
                "ByteArray" => Ok(NamedSchema {
                    title: name.to_string(),
                    description: None,
                    schema: Schema::Bytes,
                }),
                "Integer" => Ok(NamedSchema {
                    title: name.to_string(),
                    description: None,
                    schema: Schema::Bytes,
                }),
                _ => Err(Error::UnsupportedPrimitiveType {
                    type_name: type_name.clone(),
                }),
            },
            Type::App {
                module: module_name,
                name: type_name,
                ..
            } => {
                let module = modules.get(module_name).unwrap();
                let constructor = find_definition(type_name, &module.ast.definitions).unwrap();
                let schema = Schema::from_data_type(modules, constructor)?;
                Ok(NamedSchema {
                    title: constructor.name.clone(),
                    description: constructor.doc.clone().map(|s| s.trim().to_string()),
                    schema,
                })
            }
            Type::Fn { .. } | Type::Var { .. } | Type::Tuple { .. } => {
                Err(Error::UnsupportedKind {
                    arg_or_field_name: name.to_string(),
                    type_info: type_info.clone(),
                })
            }
        }
    }
}

impl Schema {
    pub fn from_data_type(
        modules: &HashMap<String, CheckedModule>,
        data_type: &DataType<Arc<Type>>,
    ) -> Result<Self, Error> {
        let mut variants = vec![];
        for (index, constructor) in data_type.constructors.iter().enumerate() {
            let mut fields = vec![];
            for field in constructor.arguments.iter() {
                let mut schema = NamedSchema::from_type(
                    modules,
                    &field.label.clone().unwrap_or_default(),
                    &field.tipo,
                )?;
                schema.description = field.doc.clone().map(|s| s.trim().to_string());
                fields.push(schema);
            }
            variants.push(Constructor { index, fields });
        }
        Ok(Schema::AnyOf(variants))
    }
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
            Schema::AnyOf(constructors) => match &constructors[..] {
                [constructor] => constructor.serialize(serializer),
                _ => {
                    let mut s = serializer.serialize_struct("AnyOf", 1)?;
                    s.serialize_field("anyOf", &constructors)?;
                    s.end()
                }
            },
        }
    }
}
impl Serialize for Constructor {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut s = serializer.serialize_struct("Constructor", 3)?;
        s.serialize_field("dataType", "constructor")?;
        s.serialize_field("index", &self.index)?;
        s.serialize_field("fields", &self.fields)?;
        s.end()
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

#[derive(Debug, PartialEq, Clone, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("I stumble upon an unsupported kind in a datum or redeemer definition.\n")]
    UnsupportedKind {
        arg_or_field_name: String,
        type_info: Type,
    },
    #[error("I discovered an unexpected primitive in a datum or redeemer definition.\n")]
    UnsupportedPrimitiveType { type_name: String },
}

fn find_definition<'a>(
    name: &str,
    definitions: &'a Vec<TypedDefinition>,
) -> Option<&'a DataType<Arc<Type>>> {
    for def in definitions {
        match def {
            Definition::DataType(data_type) if name == data_type.name => return Some(data_type),
            Definition::Fn { .. }
            | Definition::DataType { .. }
            | Definition::TypeAlias { .. }
            | Definition::Use { .. }
            | Definition::ModuleConstant { .. }
            | Definition::Test { .. } => continue,
        }
    }
    None
}

#[cfg(test)]
pub mod test {
    use super::*;
    use serde_json::{self, json, Value};

    pub fn assert_json(schema: &impl Serialize, expected: Value) {
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

    #[test]
    fn serialize_constr_1() {
        let schema = Schema::AnyOf(vec![Constructor {
            index: 0,
            fields: vec![],
        }]);
        assert_json(
            &schema,
            json!({
                "dataType": "constructor",
                "index": 0,
                "fields": []
            }),
        )
    }

    #[test]
    fn serialize_constr_2() {
        let schema = Schema::AnyOf(vec![
            Constructor {
                index: 0,
                fields: vec![Schema::Integer],
            },
            Constructor {
                index: 1,
                fields: vec![Schema::Bytes],
            },
        ]);
        assert_json(
            &schema,
            json!({
                "anyOf": [
                    {
                        "dataType": "constructor",
                        "index": 0,
                        "fields": [{ "dataType": "integer" }]
                    },
                    {
                        "dataType": "constructor",
                        "index": 1,
                        "fields": [{ "dataType": "bytes" }]
                    }
                ]
            }),
        )
    }

    #[test]
    fn serialize_named_no_description() {
        let schema = NamedSchema {
            title: "foo".to_string(),
            description: None,
            schema: Schema::Integer,
        };
        assert_json(
            &schema,
            json!({
                "title": "foo",
                "dataType": "integer"
            }),
        )
    }

    #[test]
    fn serialize_named_description() {
        let schema = NamedSchema {
            title: "foo".to_string(),
            description: Some("Lorem Ipsum".to_string()),
            schema: Schema::Integer,
        };
        assert_json(
            &schema,
            json!({
                "title": "foo",
                "description": "Lorem Ipsum",
                "dataType": "integer"
            }),
        )
    }
}
