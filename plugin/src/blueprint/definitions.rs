use serde::{
    self,
    de::{self, Deserialize, Deserializer, MapAccess, Visitor},
    ser::{Serialize, SerializeStruct, Serializer},
};
use std::{
    collections::BTreeMap,
    fmt::{self, Display},
};

// ---------- Definitions

/// A map of definitions meant to be optionally registered and looked up.
#[derive(Debug, PartialEq, Eq, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct Definitions<T> {
    #[serde(flatten, default)]
    inner: BTreeMap<String, Option<T>>,
}

impl<T> Definitions<T> {
    /// Constructs a new empty definitions set.
    pub fn new() -> Self {
        Definitions {
            inner: BTreeMap::new(),
        }
    }

    /// True when there's no known definitions.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Retrieve a definition, if it exists; fail if not resolved
    pub fn lookup(&self, reference: &Reference) -> Option<&T> {
        self.inner
            .get(&reference.as_key())
            .map(|v| v
              .as_ref()
              .expect("All registered definitions are 'Some'. 'None' state is only transient during registration")
            )
    }

    /// Retrieve a definition, if it exists and is resolved.
    pub fn try_lookup(&self, reference: &Reference) -> Option<&T> {
        self.inner.get(&reference.as_key()).and_then(|v| v.as_ref())
    }

    /// Merge two set of definitions together. Prioritize callee.
    pub fn merge(&mut self, other: &mut Definitions<T>) {
        self.inner.append(&mut other.inner);
    }

    /// Erase a known definition. Does nothing if the reference is unknown.
    pub fn remove(&mut self, reference: &Reference) {
        self.inner.remove(&reference.as_key());
    }

    /// Insert a new definition
    pub fn insert(&mut self, reference: &Reference, schema: T) {
        self.inner.insert(reference.as_key(), Some(schema));
    }
}

// ---------- Reference

/// A URI pointer to an underlying data-type.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Default)]
pub struct Reference {
    inner: String,
}

impl Reference {
    /// Create a (possibly unsound) Reference from a string. This isn't the preferred way to create
    /// a reference. One should use: `into()` on a 'Type' instead.
    pub fn new(path: &str) -> Reference {
        Reference {
            inner: path.to_string(),
        }
    }

    /// Turn a reference into a key suitable for lookup.
    pub(crate) fn as_key(&self) -> String {
        self.inner.replace("~1", "/")
    }

    /// Turn a reference into a valid JSON pointer. Note that the JSON pointer specification
    /// indicates that '/' must be escaped as '~1' in pointer addresses (as they are otherwise
    /// treated as path delimiter in pointers paths).
    pub(crate) fn as_json_pointer(&self) -> String {
        format!("#/definitions/{}", self.as_key().replace('/', "~1"))
    }
}

impl Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.inner)
    }
}

impl Serialize for Reference {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut s = serializer.serialize_struct("$ref", 1)?;
        s.serialize_field("$ref", &self.as_json_pointer())?;
        s.end()
    }
}

impl<'a> Deserialize<'a> for Reference {
    fn deserialize<D: Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(serde::Deserialize)]
        enum Field {
            #[serde(rename = "$ref")]
            Ref,
        }
        const FIELDS: &[&str] = &["$ref"];

        struct ReferenceVisitor;

        impl<'a> Visitor<'a> for ReferenceVisitor {
            type Value = Reference;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("Reference")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Reference, V::Error>
            where
                V: MapAccess<'a>,
            {
                let mut inner = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Ref => {
                            if inner.is_some() {
                                return Err(de::Error::duplicate_field(FIELDS[0]));
                            }
                            inner = Some(map.next_value()?);
                        }
                    }
                }

                let inner: String = inner.ok_or_else(|| de::Error::missing_field(FIELDS[0]))?;

                match inner.strip_prefix("#/definitions/") {
                    Some(suffix) => Ok(Reference {
                        inner: suffix.to_string(),
                    }),
                    None => Err(de::Error::custom(
                        "Invalid reference; only local JSON pointer to #/definitions are allowed.",
                    )),
                }
            }
        }

        deserializer.deserialize_struct("Reference", FIELDS, ReferenceVisitor)
    }
}
