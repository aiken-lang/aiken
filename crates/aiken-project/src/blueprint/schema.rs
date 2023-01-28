use crate::CheckedModule;
use aiken_lang::{
    ast::{DataType, Definition, TypedDefinition},
    tipo::{pretty, Type, TypeVar},
};
use miette::Diagnostic;
use owo_colors::OwoColorize;
use serde::{
    self,
    ser::{Serialize, SerializeStruct, Serializer},
};
use serde_json;
use std::ops::Deref;
use std::{
    collections::HashMap,
    fmt::{self, Display},
    sync::Arc,
};

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize)]
pub struct Annotated<T> {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(flatten)]
    pub annotated: T,
}

/// A schema for low-level UPLC primitives.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Schema {
    Unit,
    Boolean,
    Integer,
    Bytes,
    String,
    Pair(Box<Data>, Box<Data>),
    List(Box<Data>),
    Data(Option<Data>),
}

/// A schema for Plutus' Data.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Data {
    Integer,
    Bytes,
    List(Box<Data>),
    Map(Box<Data>, Box<Data>),
    AnyOf(Vec<Annotated<Constructor>>),
}

/// Captures a single UPLC constructor with its
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Constructor {
    pub index: usize,
    pub fields: Vec<Annotated<Data>>,
}

impl<T> From<T> for Annotated<T> {
    fn from(annotated: T) -> Self {
        Annotated {
            title: None,
            description: None,
            annotated,
        }
    }
}

impl Annotated<Schema> {
    pub fn from_type(
        modules: &HashMap<String, CheckedModule>,
        type_info: &Type,
    ) -> Result<Self, Error> {
        match type_info {
            Type::App {
                module: module_name,
                name: type_name,
                args,
                ..
            } if module_name.is_empty() => match &type_name[..] {
                "Data" => Ok(Annotated {
                    title: Some("Data".to_string()),
                    description: Some("Any Plutus data.".to_string()),
                    annotated: Schema::Data(None),
                }),
                "ByteArray" => Ok(Annotated {
                    title: None,
                    description: None,
                    annotated: Schema::Data(Some(Data::Bytes)),
                }),
                "Int" => Ok(Annotated {
                    title: None,
                    description: None,
                    annotated: Schema::Data(Some(Data::Integer)),
                }),
                // TODO: Check whether this matches with the UPLC code generation as there are two
                // options here since there's technically speaking a `unit` constant constructor in
                // the UPLC primitives.
                "Void" => Ok(Annotated {
                    title: Some("Unit".to_string()),
                    description: Some("The nullary constructor.".to_string()),
                    annotated: Schema::Data(Some(Data::AnyOf(vec![Annotated {
                        title: None,
                        description: None,
                        annotated: Constructor {
                            index: 0,
                            fields: vec![],
                        },
                    }]))),
                }),
                // TODO: Also check here whether this matches with the UPLC code generation.
                "Bool" => Ok(Annotated {
                    title: Some("Bool".to_string()),
                    description: None,
                    annotated: Schema::Data(Some(Data::AnyOf(vec![
                        Annotated {
                            title: Some("False".to_string()),
                            description: None,
                            annotated: Constructor {
                                index: 0,
                                fields: vec![],
                            },
                        },
                        Annotated {
                            title: Some("True".to_string()),
                            description: None,
                            annotated: Constructor {
                                index: 1,
                                fields: vec![],
                            },
                        },
                    ]))),
                }),
                "Option" => {
                    let generic = Annotated::from_type(modules, args.get(0).unwrap())
                        .and_then(|s| s.into_data(type_info))?;
                    Ok(Annotated {
                        title: Some("Optional".to_string()),
                        description: None,
                        annotated: Schema::Data(Some(Data::AnyOf(vec![
                            Annotated {
                                title: Some("Some".to_string()),
                                description: Some("An optional value.".to_string()),
                                annotated: Constructor {
                                    index: 0,
                                    fields: vec![generic],
                                },
                            },
                            Annotated {
                                title: Some("None".to_string()),
                                description: Some("Nothing.".to_string()),
                                annotated: Constructor {
                                    index: 1,
                                    fields: vec![],
                                },
                            },
                        ]))),
                    })
                }
                "List" => {
                    let generic = Annotated::from_type(modules, args.get(0).unwrap())
                        .and_then(|s| s.into_data(type_info))?;
                    Ok(Annotated {
                        title: None,
                        description: None,
                        annotated: Schema::Data(Some(Data::List(Box::new(generic.annotated)))),
                    })
                }
                _ => Err(Error::UnsupportedType {
                    type_info: type_info.clone(),
                }),
            },
            Type::App {
                module: module_name,
                name: type_name,
                ..
            } => {
                let module = modules.get(module_name).unwrap();
                let constructor = find_definition(type_name, &module.ast.definitions).unwrap();
                let annotated = Schema::Data(Some(Data::from_data_type(modules, constructor)?));

                Ok(Annotated {
                    title: Some(constructor.name.clone()),
                    description: constructor.doc.clone().map(|s| s.trim().to_string()),
                    annotated,
                })
            }
            Type::Var { tipo } => match tipo.borrow().deref() {
                TypeVar::Link { tipo } => Annotated::from_type(modules, tipo),
                TypeVar::Generic { .. } => todo!(),
                TypeVar::Unbound { .. } => Err(Error::UnsupportedType {
                    type_info: type_info.clone(),
                }),
            },
            Type::Tuple { .. } => todo!(),
            Type::Fn { .. } => Err(Error::UnsupportedType {
                type_info: type_info.clone(),
            }),
        }
    }

    fn into_data(self, type_info: &Type) -> Result<Annotated<Data>, Error> {
        match self {
            Annotated {
                title,
                description,
                annotated: Schema::Data(Some(data)),
            } => Ok(Annotated {
                title,
                description,
                annotated: data,
            }),
            _ => Err(Error::UnsupportedType {
                type_info: type_info.to_owned(),
            }),
        }
    }
}

impl Data {
    pub fn from_data_type(
        modules: &HashMap<String, CheckedModule>,
        data_type: &DataType<Arc<Type>>,
    ) -> Result<Self, Error> {
        let mut variants = vec![];
        for (index, constructor) in data_type.constructors.iter().enumerate() {
            let mut fields = vec![];
            for field in constructor.arguments.iter() {
                let mut schema = Annotated::from_type(modules, &field.tipo)
                    .and_then(|t| t.into_data(&field.tipo))?;

                if field.label.is_some() {
                    schema.title = field.label.clone();
                }

                if field.doc.is_some() {
                    schema.description = field.doc.clone().map(|s| s.trim().to_string());
                }

                fields.push(schema);
            }

            let variant = Annotated {
                title: Some(constructor.name.clone()),
                description: constructor.doc.clone(),
                annotated: Constructor { index, fields },
            };

            variants.push(variant);
        }
        Ok(Data::AnyOf(variants))
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
            Schema::Unit => {
                let mut s = serializer.serialize_struct("Unit", 1)?;
                s.serialize_field("dataType", "#unit")?;
                s.end()
            }
            Schema::Boolean => {
                let mut s = serializer.serialize_struct("Integer", 1)?;
                s.serialize_field("dataType", "#integer")?;
                s.end()
            }
            Schema::Integer => {
                let mut s = serializer.serialize_struct("Integer", 1)?;
                s.serialize_field("dataType", "#integer")?;
                s.end()
            }
            Schema::Bytes => {
                let mut s = serializer.serialize_struct("Bytes", 1)?;
                s.serialize_field("dataType", "#bytes")?;
                s.end()
            }
            Schema::String => {
                let mut s = serializer.serialize_struct("String", 1)?;
                s.serialize_field("dataType", "#string")?;
                s.end()
            }
            Schema::Pair(left, right) => {
                let mut s = serializer.serialize_struct("Pair", 3)?;
                s.serialize_field("dataType", "#pair")?;
                s.serialize_field("left", &left)?;
                s.serialize_field("right", &right)?;
                s.end()
            }
            Schema::List(elements) => {
                let mut s = serializer.serialize_struct("List", 2)?;
                s.serialize_field("dataType", "#list")?;
                s.serialize_field("elements", &elements)?;
                s.end()
            }
            Schema::Data(None) => {
                let s = serializer.serialize_struct("Data", 0)?;
                s.end()
            }
            Schema::Data(Some(data)) => data.serialize(serializer),
        }
    }
}

impl Serialize for Data {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Data::Integer => {
                let mut s = serializer.serialize_struct("Integer", 1)?;
                s.serialize_field("dataType", "integer")?;
                s.end()
            }
            Data::Bytes => {
                let mut s = serializer.serialize_struct("Bytes", 1)?;
                s.serialize_field("dataType", "bytes")?;
                s.end()
            }
            Data::List(items) => {
                let mut s = serializer.serialize_struct("List", 2)?;
                s.serialize_field("dataType", "list")?;
                s.serialize_field("items", &items)?;
                s.end()
            }
            Data::Map(keys, values) => {
                let mut s = serializer.serialize_struct("Map", 3)?;
                s.serialize_field("dataType", "map")?;
                s.serialize_field("keys", &keys)?;
                s.serialize_field("values", &values)?;
                s.end()
            }
            Data::AnyOf(constructors) => {
                // TODO: Avoid 'anyOf' applicator when there's only one constructor
                //
                // match &constructors[..] {
                // [constructor] => constructor.serialize(serializer),
                // _ => {
                let mut s = serializer.serialize_struct("AnyOf", 1)?;
                s.serialize_field("anyOf", &constructors)?;
                s.end()
            }
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

#[derive(Debug, PartialEq, Clone, thiserror::Error, Diagnostic)]
pub enum Error {
    #[error("I stumbled upon an unsupported type in a datum or redeemer definition.")]
    #[diagnostic(help(
        r#"I do not know how to generate a portable Plutus specification for the following type:

╰─▶ {type_signature}
        "#
        , type_signature = pretty::Printer::new().print(type_info).to_pretty_string(70).bright_blue()
    ))]
    UnsupportedType { type_info: Type },
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
    fn serialize_data_integer() {
        let schema = Schema::Data(Some(Data::Integer));
        assert_json(
            &schema,
            json!({
                "dataType": "integer"
            }),
        );
    }

    #[test]
    fn serialize_data_bytes() {
        let schema = Schema::Data(Some(Data::Bytes));
        assert_json(
            &schema,
            json!({
                "dataType": "bytes"
            }),
        );
    }

    #[test]
    fn serialize_data_list_1() {
        let schema = Schema::Data(Some(Data::List(Box::new(Data::Integer))));
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
    fn serialize_data_list_2() {
        let schema = Schema::Data(Some(Data::List(Box::new(Data::List(Box::new(
            Data::Integer,
        ))))));
        assert_json(
            &schema,
            json!({
                "dataType": "list",
                "items":
                    {
                        "dataType": "list",
                        "items": { "dataType": "integer" }
                    }
            }),
        );
    }

    #[test]
    fn serialize_data_map_1() {
        let schema = Schema::Data(Some(Data::Map(
            Box::new(Data::Integer),
            Box::new(Data::Bytes),
        )));
        assert_json(
            &schema,
            json!({
                "dataType": "map",
                "keys": {
                    "dataType": "integer"
                },
                "values": {
                    "dataType": "bytes"
                }
            }),
        )
    }

    #[test]
    fn serialize_data_map_2() {
        let schema = Schema::Data(Some(Data::Map(
            Box::new(Data::Bytes),
            Box::new(Data::List(Box::new(Data::Integer))),
        )));
        assert_json(
            &schema,
            json!({
                "dataType": "map",
                "keys": {
                    "dataType": "bytes"
                },
                "values": {
                    "dataType": "list",
                    "items": { "dataType": "integer" }
                }
            }),
        )
    }

    #[test]
    fn serialize_data_constr_1() {
        let schema = Schema::Data(Some(Data::AnyOf(vec![Constructor {
            index: 0,
            fields: vec![],
        }
        .into()])));
        assert_json(
            &schema,
            json!({
                "anyOf": [{
                    "dataType": "constructor",
                    "index": 0,
                    "fields": []
                }]
            }),
        )
    }

    #[test]
    fn serialize_data_constr_2() {
        let schema = Schema::Data(Some(Data::AnyOf(vec![
            Constructor {
                index: 0,
                fields: vec![Data::Integer.into()],
            }
            .into(),
            Constructor {
                index: 1,
                fields: vec![Data::Bytes.into()],
            }
            .into(),
        ])));
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
    fn serialize_empty_data() {
        let schema = Schema::Data(None);
        assert_json(&schema, json!({}))
    }

    #[test]
    fn serialize_annotated_1() {
        let schema = Annotated {
            title: Some("foo".to_string()),
            description: None,
            annotated: Schema::Integer,
        };
        assert_json(
            &schema,
            json!({
                "title": "foo",
                "dataType": "#integer"
            }),
        )
    }

    #[test]
    fn serialize_annotated_2() {
        let schema = Annotated {
            title: Some("foo".to_string()),
            description: Some("Lorem Ipsum".to_string()),
            annotated: Schema::String,
        };
        assert_json(
            &schema,
            json!({
                "title": "foo",
                "description": "Lorem Ipsum",
                "dataType": "#string"
            }),
        )
    }
}
