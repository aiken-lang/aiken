use super::commit::Commit;
use serde::Deserialize;

#[derive(Deserialize)]
pub struct Branch {
    pub name: String,
    pub commit: Commit,
}
