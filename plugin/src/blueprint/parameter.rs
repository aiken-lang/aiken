use super::definitions::Reference;

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Parameter {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,

    pub schema: Reference,
}

impl From<Reference> for Parameter {
    fn from(schema: Reference) -> Parameter {
        Parameter {
            title: None,
            schema,
        }
    }
}
