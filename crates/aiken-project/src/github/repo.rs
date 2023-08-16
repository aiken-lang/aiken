use reqwest::{blocking::Client, header::USER_AGENT, Error};
use serde::Deserialize;

#[derive(Deserialize)]
pub struct LatestRelease {
    pub tag_name: String,
}

impl LatestRelease {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Self, Error> {
        Ok({
            Client::new()
                .get(format!(
                    "https://api.github.com/repos/{}/releases/latest",
                    repo.as_ref()
                ))
                .header(USER_AGENT, "aiken")
                .send()?
                .json::<Self>()?
        })
    }
}
