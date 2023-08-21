use serde::Deserialize;

#[derive(Deserialize)]
pub struct Commit {
    pub sha: String,
}
