use serde::Deserialize;

#[derive(Deserialize)]
pub struct Release {
    pub tag_name: String,
}
