use serde::Deserialize;

#[derive(Deserialize)]
pub struct Tag {
    pub name: String,
}
