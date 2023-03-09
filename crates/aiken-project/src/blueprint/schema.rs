use crate::CheckedModule;
use aiken_lang::{
    ast::{DataType, Definition, TypedDefinition},
    tipo::{pretty, Type, TypeVar},
};
use owo_colors::{OwoColorize, Stream::Stdout};
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

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
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
    Pair(Data, Data),
    List(Vec<Data>),
    Data(Data),
}

/// A schema for Plutus' Data.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Data {
    Integer,
    Bytes,
    List(Items<Data>),
    Map(Box<Data>, Box<Data>),
    AnyOf(Vec<Annotated<Constructor>>),
    Opaque,
}

/// A structure that represents either one or many elements.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Items<T> {
    One(Box<T>),
    Many(Vec<T>),
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
        type_parameters: &HashMap<u64, &Arc<Type>>,
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
                    annotated: Schema::Data(Data::Opaque),
                }),

                "ByteArray" => Ok(Schema::Data(Data::Bytes).into()),

                "Int" => Ok(Schema::Data(Data::Integer).into()),

                "String" => Ok(Schema::String.into()),

                "Void" => Ok(Annotated {
                    title: Some("Unit".to_string()),
                    description: Some("The nullary constructor.".to_string()),
                    annotated: Schema::Data(Data::AnyOf(vec![Annotated {
                        title: None,
                        description: None,
                        annotated: Constructor {
                            index: 0,
                            fields: vec![],
                        },
                    }])),
                }),

                "Bool" => Ok(Annotated {
                    title: Some("Bool".to_string()),
                    description: None,
                    annotated: Schema::Data(Data::AnyOf(vec![
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
                    ])),
                }),

                "Ordering" => Ok(Annotated {
                    title: Some("Ordering".to_string()),
                    description: None,
                    annotated: Schema::Data(Data::AnyOf(vec![
                        Annotated {
                            title: Some("Less".to_string()),
                            description: None,
                            annotated: Constructor {
                                index: 0,
                                fields: vec![],
                            },
                        },
                        Annotated {
                            title: Some("Equal".to_string()),
                            description: None,
                            annotated: Constructor {
                                index: 1,
                                fields: vec![],
                            },
                        },
                        Annotated {
                            title: Some("Greater".to_string()),
                            description: None,
                            annotated: Constructor {
                                index: 2,
                                fields: vec![],
                            },
                        },
                    ])),
                }),

                "Option" => {
                    let generic =
                        Annotated::from_type(modules, args.get(0).unwrap(), type_parameters)
                            .and_then(|s| s.into_data(type_info))?;
                    Ok(Annotated {
                        title: Some("Optional".to_string()),
                        description: None,
                        annotated: Schema::Data(Data::AnyOf(vec![
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
                        ])),
                    })
                }

                "List" => {
                    let generic =
                        Annotated::from_type(modules, args.get(0).unwrap(), type_parameters)?;

                    // NOTE: Lists of 2-tuples are treated as Maps. This is an oddity we inherit
                    // from the PlutusTx / LedgerApi Haskell codebase, which encodes some elements
                    // as such. We don't have a concept of language maps in Aiken, so we simply
                    // make all types abide by this convention.
                    let data = match generic.annotated {
                        Schema::Data(Data::List(Items::Many(xs))) if xs.len() == 2 => Data::Map(
                            Box::new(xs.first().unwrap().to_owned()),
                            Box::new(xs.last().unwrap().to_owned()),
                        ),
                        _ => {
                            let inner = generic.into_data(type_info)?.annotated;
                            Data::List(Items::One(Box::new(inner)))
                        }
                    };

                    Ok(Schema::Data(data).into())
                }

                _ => Err(Error::new(ErrorContext::UnsupportedType, type_info)),
            },
            Type::App {
                module: module_name,
                name: type_name,
                args,
                ..
            } => {
                let module = modules.get(module_name).unwrap();
                let constructor = find_definition(type_name, &module.ast.definitions).unwrap();
                let type_parameters = collect_type_parameters(&constructor.typed_parameters, args);
                let annotated = Schema::Data(
                    Data::from_data_type(modules, constructor, &type_parameters)
                        .map_err(|e| e.backtrack(type_info))?,
                );

                Ok(Annotated {
                    title: Some(constructor.name.clone()),
                    description: constructor.doc.clone().map(|s| s.trim().to_string()),
                    annotated,
                })
            }
            Type::Var { tipo } => match tipo.borrow().deref() {
                TypeVar::Link { tipo } => Annotated::from_type(modules, tipo, type_parameters),
                TypeVar::Generic { id } => {
                    let tipo = type_parameters
                        .get(id)
                        .ok_or_else(|| Error::new(ErrorContext::FreeTypeVariable, type_info))?;
                    Annotated::from_type(modules, tipo, type_parameters)
                }
                TypeVar::Unbound { .. } => {
                    Err(Error::new(ErrorContext::UnboundTypeVariable, type_info))
                }
            },
            Type::Tuple { elems } => {
                let elems = elems
                    .iter()
                    .map(|e| {
                        Annotated::from_type(modules, e, type_parameters)
                            .and_then(|s| s.into_data(e).map(|s| s.annotated))
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(|e| e.backtrack(type_info))?;

                Ok(Annotated {
                    title: Some("Tuple".to_owned()),
                    description: None,
                    annotated: Schema::Data(Data::List(Items::Many(elems))),
                })
            }
            Type::Fn { .. } => Err(Error::new(ErrorContext::UnexpectedFunction, type_info)),
        }
    }

    fn into_data(self, type_info: &Type) -> Result<Annotated<Data>, Error> {
        match self {
            Annotated {
                title,
                description,
                annotated: Schema::Data(data),
            } => Ok(Annotated {
                title,
                description,
                annotated: data,
            }),
            _ => Err(Error::new(ErrorContext::ExpectedData, type_info)),
        }
    }
}

impl Data {
    pub fn from_data_type(
        modules: &HashMap<String, CheckedModule>,
        data_type: &DataType<Arc<Type>>,
        type_parameters: &HashMap<u64, &Arc<Type>>,
    ) -> Result<Self, Error> {
        let mut variants = vec![];

        for (index, constructor) in data_type.constructors.iter().enumerate() {
            let mut fields = vec![];
            for field in constructor.arguments.iter() {
                let mut schema = Annotated::from_type(modules, &field.tipo, type_parameters)
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
                description: constructor.doc.clone().map(|s| s.trim().to_string()),
                annotated: Constructor { index, fields },
            };

            variants.push(variant);
        }

        // NOTE: Opaque data-types with a single variant and a single field are transparent, they
        // are erased completely at compilation time.
        if data_type.opaque {
            if let [variant] = &variants[..] {
                if let [field] = &variant.annotated.fields[..] {
                    return Ok(field.annotated.clone());
                }
            }
        }

        Ok(Data::AnyOf(variants))
    }
}

impl Default for Schema {
    fn default() -> Self {
        Schema::Unit
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
                let mut s = serializer.serialize_struct("Boolean", 1)?;
                s.serialize_field("dataType", "#boolean")?;
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
            Schema::List(items) => {
                let mut s = serializer.serialize_struct("List", 2)?;
                s.serialize_field("dataType", "#list")?;
                s.serialize_field("items", &items)?;
                s.end()
            }
            Schema::Data(data) => data.serialize(serializer),
        }
    }
}

impl Serialize for Data {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Data::Opaque => {
                let s = serializer.serialize_struct("Opaque", 0)?;
                s.end()
            }
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
            Data::List(Items::One(item)) => {
                let mut s = serializer.serialize_struct("List", 2)?;
                s.serialize_field("dataType", "list")?;
                s.serialize_field("items", &item)?;
                s.end()
            }
            Data::List(Items::Many(items)) => {
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

#[derive(Debug, PartialEq, Clone, thiserror::Error)]
#[error("{}", context)]
pub struct Error {
    context: ErrorContext,
    breadcrumbs: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone, thiserror::Error)]
pub enum ErrorContext {
    #[error("I failed at my own job and couldn't figure out how to generate a specification for a type.")]
    UnsupportedType,

    #[error("I discovered a type hole where I would expect a concrete type.")]
    UnboundTypeVariable,

    #[error("I caught a free variable in the contract's interface boundary.")]
    FreeTypeVariable,

    #[error("I had the misfortune to find an invalid type in an interface boundary.")]
    ExpectedData,

    #[error("I figured you tried to export a function in your contract's binary interface.")]
    UnexpectedFunction,
}

impl Error {
    pub fn new(context: ErrorContext, type_info: &Type) -> Self {
        Error {
            context,
            breadcrumbs: vec![type_info.clone()],
        }
    }

    pub fn backtrack(self, type_info: &Type) -> Self {
        let mut breadcrumbs = vec![type_info.clone()];
        breadcrumbs.extend(self.breadcrumbs);
        Error {
            context: self.context,
            breadcrumbs,
        }
    }

    pub fn help(&self) -> String {
        match self.context {
            ErrorContext::UnsupportedType => format!(
                r#"I do not know how to generate a portable Plutus specification for the following type:

╰─▶ {signature}

This is likely a bug. I should know. May you be kind enough and report this on <https://github.com/aiken-lang/aiken>."#,
                signature = Error::fmt_breadcrumbs(&[self.breadcrumbs.last().unwrap().to_owned()]),
            ),

            ErrorContext::FreeTypeVariable => format!(
                r#"There can't be any free type variable at the contract's boundary (i.e. in types used as datum and/or redeemer). Indeed, the validator can only be invoked with (very) concrete types. Since there's no reflexion possible inside a validator, it simply isn't possible to have any remaining free type variable in any of the datum or redeemer.

I got there when trying to generate a blueprint specification of the following type:

╰─▶ {breadcrumbs}"#,
                breadcrumbs = Error::fmt_breadcrumbs(&self.breadcrumbs)
            ),

            ErrorContext::UnboundTypeVariable => format!(
                r#"There cannot be any unbound type variable at the contract's boundary (i.e. in types used as datum and/or redeemer). Indeed, in order to generate an outward-facing specification of the contract's interface, I need to know what concrete representations will the datum and/or the redeemer have.

If your contract doesn't need datum or redeemer, you can always give them the type {type_Void} to indicate this. It is very concrete and will help me progress forward."#,
                type_Void = "Void"
                    .if_supports_color(Stdout, |s| s.bright_blue())
                    .if_supports_color(Stdout, |s| s.bold())
            ),

            ErrorContext::ExpectedData => format!(
                r#"While figuring out the outward-facing specification for your contract, I found a type that cannot actually be represented as valid Untyped Plutus Core (the low-level language Cardano uses to execute smart-contracts. For example, it isn't possible to have a list or a tuple of {type_String} because the underlying execution engine doesn't allow it.

There are few restrictions like this one. In this instance, here's the types I followed and that led me to this problem:

╰─▶ {breadcrumbs}"#,
                type_String = "String"
                    .if_supports_color(Stdout, |s| s.bright_blue())
                    .if_supports_color(Stdout, |s| s.bold()),
                breadcrumbs = Error::fmt_breadcrumbs(&self.breadcrumbs)
            ),

            ErrorContext::UnexpectedFunction => format!(
                r#"I can't allow that. Functions aren't serializable as data on-chain and thus cannot be used within your datum and/or redeemer types.

Here's the types I followed and that led me to this problem:

╰─▶ {breadcrumbs}"#,
                breadcrumbs = Error::fmt_breadcrumbs(&self.breadcrumbs)
            ),
        }
    }

    fn fmt_breadcrumbs(breadcrumbs: &[Type]) -> String {
        breadcrumbs
            .iter()
            .map(|type_info| {
                pretty::Printer::new()
                    .print(type_info)
                    .to_pretty_string(70)
                    .if_supports_color(Stdout, |s| s.bright_blue())
                    .if_supports_color(Stdout, |s| s.bold())
                    .to_string()
            })
            .collect::<Vec<_>>()
            .join(" → ")
    }
}

fn collect_type_parameters<'a>(
    generics: &'a [Arc<Type>],
    applications: &'a [Arc<Type>],
) -> HashMap<u64, &'a Arc<Type>> {
    let mut type_parameters = HashMap::new();

    for (index, generic) in generics.iter().enumerate() {
        match &**generic {
            Type::Var { tipo } => match *tipo.borrow() {
                TypeVar::Generic { id } => {
                    type_parameters.insert(id, applications.get(index).unwrap());
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    type_parameters
}

fn find_definition<'a>(
    name: &str,
    definitions: &'a Vec<TypedDefinition>,
) -> Option<&'a DataType<Arc<Type>>> {
    for def in definitions {
        match def {
            Definition::DataType(data_type) if name == data_type.name => return Some(data_type),
            Definition::Fn { .. }
            | Definition::Validator { .. }
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
        let schema = Schema::Data(Data::Integer);
        assert_json(
            &schema,
            json!({
                "dataType": "integer"
            }),
        );
    }

    #[test]
    fn serialize_data_bytes() {
        let schema = Schema::Data(Data::Bytes);
        assert_json(
            &schema,
            json!({
                "dataType": "bytes"
            }),
        );
    }

    #[test]
    fn serialize_data_list_1() {
        let schema = Schema::Data(Data::List(Items::One(Box::new(Data::Integer))));
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
        let schema = Schema::Data(Data::List(Items::One(Box::new(Data::List(Items::One(
            Box::new(Data::Integer),
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
    fn serialize_data_list_3() {
        let schema = Schema::Data(Data::List(Items::Many(vec![Data::Integer, Data::Bytes])));
        assert_json(
            &schema,
            json!({
                "dataType": "list",
                "items": [
                    { "dataType": "integer" },
                    { "dataType": "bytes" },
                ]
            }),
        );
    }

    #[test]
    fn serialize_data_map_1() {
        let schema = Schema::Data(Data::Map(Box::new(Data::Integer), Box::new(Data::Bytes)));
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
        let schema = Schema::Data(Data::Map(
            Box::new(Data::Bytes),
            Box::new(Data::List(Items::One(Box::new(Data::Integer)))),
        ));
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
        let schema = Schema::Data(Data::AnyOf(vec![Constructor {
            index: 0,
            fields: vec![],
        }
        .into()]));
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
        let schema = Schema::Data(Data::AnyOf(vec![
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
        ]));
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
        let schema = Schema::Data(Data::Opaque);
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
