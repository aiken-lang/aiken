use serde::Deserialize;

#[derive(Deserialize)]
pub struct Repo {
    pub default_branch: String,
}
