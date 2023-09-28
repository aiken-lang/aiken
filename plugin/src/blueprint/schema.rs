use crate::blueprint::definitions::{Definitions, Reference};
use serde::{
    self,
    de::{self, Deserialize, Deserializer, MapAccess, Visitor},
    ser::{Serialize, SerializeStruct, Serializer},
};
use serde_json as json;
use std::{fmt, ops::Deref};

// NOTE: Can be anything BUT 0

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Annotated<T> {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(flatten)]
    pub annotated: T,
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum Declaration<T> {
    Referenced(Reference),
    Inline(Box<T>),
}

impl<'a, T> Declaration<T> {
    pub fn reference(&'a self) -> Option<&'a Reference> {
        match self {
            Declaration::Referenced(reference) => Some(reference),
            Declaration::Inline(..) => None,
        }
    }

    fn try_schema(
        &'a self,
        definitions: &'a Definitions<Annotated<Schema>>,
        cast: fn(&'a Schema) -> Option<&'a T>,
    ) -> Option<&'a T> {
        match self {
            Declaration::Inline(inner) => Some(inner.deref()),
            Declaration::Referenced(reference) => definitions
                .lookup(reference)
                .and_then(|s| cast(&s.annotated)),
        }
    }
}

impl<'a> Declaration<Data> {
    pub fn schema(&'a self, definitions: &'a Definitions<Annotated<Schema>>) -> Option<&'a Data> {
        self.try_schema(definitions, |s| match s {
            Schema::Data(data) => Some(data),
            _ => None,
        })
    }
}

impl<'a> Declaration<Schema> {
    pub fn schema(&'a self, definitions: &'a Definitions<Annotated<Schema>>) -> Option<&'a Schema> {
        self.try_schema(definitions, Some)
    }
}

/// A schema for low-level UPLC primitives.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Schema {
    Unit,
    Boolean,
    Integer,
    Bytes,
    String,
    Pair(Declaration<Schema>, Declaration<Schema>),
    List(Items<Schema>),
    Data(Data),
}

/// A schema for Plutus' Data.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Data {
    Integer,
    Bytes,
    List(Items<Data>),
    Map(Declaration<Data>, Declaration<Data>),
    AnyOf(Vec<Annotated<Constructor>>),
    Opaque,
}

/// A structure that represents either one or many elements.
#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum Items<T> {
    One(Declaration<T>),
    Many(Vec<Declaration<T>>),
}

/// Captures a single UPLC constructor with its
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Constructor {
    pub index: usize,
    pub fields: Vec<Annotated<Declaration<Data>>>,
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

// Needed because of Blueprint's default, but actually never used.
impl Default for Annotated<Schema> {
    fn default() -> Self {
        Schema::Data(Data::Opaque).into()
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

fn visit_schema<'a, V>(mut map: V) -> Result<Schema, V::Error>
where
    V: MapAccess<'a>,
{
    #[derive(serde::Deserialize)]
    #[serde(field_identifier, rename_all = "camelCase")]
    enum Field {
        DataType,
        Items,
        Keys,
        Values,
        Left,
        Right,
        AnyOf,
        OneOf,
    }

    let mut data_type: Option<String> = None;
    let mut items: Option<json::Value> = None; // defer items deserialization to later
    let mut keys = None;
    let mut left = None;
    let mut right = None;
    let mut values = None;
    let mut any_of = None;

    while let Some(key) = map.next_key()? {
        match key {
            Field::DataType => {
                if data_type.is_some() {
                    return Err(de::Error::duplicate_field("dataType"));
                }
                data_type = Some(map.next_value()?);
            }
            Field::Items => {
                if items.is_some() {
                    return Err(de::Error::duplicate_field("items"));
                }
                items = Some(map.next_value()?);
            }
            Field::Keys => {
                if keys.is_some() {
                    return Err(de::Error::duplicate_field("keys"));
                }
                keys = Some(map.next_value()?);
            }
            Field::Values => {
                if values.is_some() {
                    return Err(de::Error::duplicate_field("values"));
                }
                values = Some(map.next_value()?);
            }
            Field::Left => {
                if left.is_some() {
                    return Err(de::Error::duplicate_field("left"));
                }
                left = Some(map.next_value()?);
            }
            Field::Right => {
                if right.is_some() {
                    return Err(de::Error::duplicate_field("right"));
                }
                right = Some(map.next_value()?);
            }
            Field::AnyOf => {
                if any_of.is_some() {
                    return Err(de::Error::duplicate_field("anyOf/oneOf"));
                }
                any_of = Some(map.next_value()?);
            }
            Field::OneOf => {
                if any_of.is_some() {
                    return Err(de::Error::duplicate_field("anyOf/oneOf"));
                }
                any_of = Some(map.next_value()?);
            }
        }
    }

    let expect_data_items = || match &items {
        Some(items) => serde_json::from_value::<Items<Data>>(items.clone())
            .map_err(|e| de::Error::custom(e.to_string())),
        None => Err(de::Error::missing_field("items")),
    };

    let expect_schema_items = || match &items {
        Some(items) => serde_json::from_value::<Items<Schema>>(items.clone())
            .map_err(|e| de::Error::custom(e.to_string())),
        None => Err(de::Error::missing_field("items")),
    };

    let expect_no_items = || {
        if items.is_some() {
            return Err(de::Error::custom(
                "unexpected fields 'items' for non-list data-type",
            ));
        }
        Ok(())
    };

    let expect_no_keys = || {
        if keys.is_some() {
            return Err(de::Error::custom(
                "unexpected fields 'keys' for non-map data-type",
            ));
        }
        Ok(())
    };

    let expect_no_values = || {
        if values.is_some() {
            return Err(de::Error::custom(
                "unexpected fields 'values' for non-map data-type",
            ));
        }
        Ok(())
    };

    let expect_no_any_of = || {
        if any_of.is_some() {
            return Err(de::Error::custom(
                "unexpected fields 'anyOf' or 'oneOf'; applicators must singletons",
            ));
        }
        Ok(())
    };

    let expect_no_left_or_right = || {
        if left.is_some() || right.is_some() {
            return Err(de::Error::custom(
                "unexpected field(s) 'left' and/or 'right' for a non-pair data-type",
            ));
        }
        Ok(())
    };

    match data_type {
        None => {
            expect_no_items()?;
            expect_no_keys()?;
            expect_no_values()?;
            expect_no_left_or_right()?;
            match any_of {
                None => Ok(Schema::Data(Data::Opaque)),
                Some(constructors) => Ok(Schema::Data(Data::AnyOf(constructors))),
            }
        }
        Some(data_type) if data_type == "list" => {
            expect_no_keys()?;
            expect_no_values()?;
            expect_no_any_of()?;
            expect_no_left_or_right()?;
            let items = expect_data_items()?;
            Ok(Schema::Data(Data::List(items)))
        }
        Some(data_type) if data_type == "#list" => {
            expect_no_keys()?;
            expect_no_values()?;
            expect_no_any_of()?;
            expect_no_left_or_right()?;
            let items = expect_schema_items()?;
            Ok(Schema::List(items))
        }
        Some(data_type) if data_type == "map" => {
            expect_no_items()?;
            expect_no_any_of()?;
            expect_no_left_or_right()?;
            match (keys, values) {
                (Some(keys), Some(values)) => Ok(Schema::Data(Data::Map(keys, values))),
                (None, _) => Err(de::Error::missing_field("keys")),
                (Some(..), None) => Err(de::Error::missing_field("values")),
            }
        }
        Some(data_type) if data_type == "#pair" => {
            expect_no_items()?;
            expect_no_keys()?;
            expect_no_values()?;
            expect_no_any_of()?;
            match (left, right) {
                (Some(left), Some(right)) => Ok(Schema::Pair(left, right)),
                (None, _) => Err(de::Error::missing_field("left")),
                (Some(..), None) => Err(de::Error::missing_field("right")),
            }
        }
        Some(data_type) => {
            expect_no_items()?;
            expect_no_keys()?;
            expect_no_values()?;
            expect_no_any_of()?;
            expect_no_left_or_right()?;
            if data_type == "bytes" {
                Ok(Schema::Data(Data::Bytes))
            } else if data_type == "integer" {
                Ok(Schema::Data(Data::Integer))
            } else if data_type == "#unit" {
                Ok(Schema::Unit)
            } else if data_type == "#integer" {
                Ok(Schema::Integer)
            } else if data_type == "#bytes" {
                Ok(Schema::Bytes)
            } else if data_type == "#boolean" {
                Ok(Schema::Boolean)
            } else if data_type == "#string" {
                Ok(Schema::String)
            } else {
                Err(de::Error::custom("unknown data-type"))
            }
        }
    }
}

impl<'a> Deserialize<'a> for Schema {
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        struct SchemaVisitor;

        impl<'a> Visitor<'a> for SchemaVisitor {
            type Value = Schema;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Schema")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Schema, V::Error>
            where
                V: MapAccess<'a>,
            {
                visit_schema(&mut map)
            }
        }

        deserializer.deserialize_struct(
            "Schema",
            &[
                "dataType", "items", "keys", "values", "anyOf", "oneOf", "left", "right",
            ],
            SchemaVisitor,
        )
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
                let mut s = serializer.serialize_struct("AnyOf", 1)?;
                s.serialize_field("anyOf", &constructors)?;
                s.end()
            }
        }
    }
}

impl<'a> Deserialize<'a> for Data {
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        struct DataVisitor;

        impl<'a> Visitor<'a> for DataVisitor {
            type Value = Data;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Data")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Data, V::Error>
            where
                V: MapAccess<'a>,
            {
                let schema = visit_schema(&mut map)?;
                match schema {
                    Schema::Data(data) => Ok(data),
                    _ => Err(de::Error::custom("not a valid 'data'")),
                }
            }
        }

        deserializer.deserialize_struct(
            "Data",
            &["dataType", "items", "keys", "values", "anyOf", "oneOf"],
            DataVisitor,
        )
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

impl<'a> Deserialize<'a> for Constructor {
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        #[serde(field_identifier, rename_all = "camelCase")]
        enum Field {
            Index,
            Fields,
        }
        const FIELDS: &[&str] = &["index", "fields"];

        struct ConstructorVisitor;

        impl<'a> Visitor<'a> for ConstructorVisitor {
            type Value = Constructor;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Constructor")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Constructor, V::Error>
            where
                V: MapAccess<'a>,
            {
                let mut index = None;
                let mut fields = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Index => {
                            if index.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[0]));
                            }
                            index = Some(map.next_value()?);
                        }
                        Field::Fields => {
                            if fields.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[1]));
                            }
                            fields = Some(map.next_value()?);
                        }
                    }
                }

                Ok(Constructor {
                    index: index.ok_or_else(|| de::Error::missing_field(FIELDS[0]))?,
                    fields: fields.ok_or_else(|| de::Error::missing_field(FIELDS[1]))?,
                })
            }
        }

        deserializer.deserialize_struct("Constructor", FIELDS, ConstructorVisitor)
    }
}
