use reqwest::{
    blocking::{Client, Response},
    header::USER_AGENT,
    Error,
};
use serde::Deserialize;

enum Get {
    LatestRelease,
    Releases,
    Tags,
    Branches,
    MainBranch,
}

// #region Github repo's RELEASES
#[derive(Deserialize)]
pub struct Release {
    pub tag_name: String,
}

pub struct LatestRelease {}
impl LatestRelease {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Release, Error> {
        http_get(repo, Get::LatestRelease)?.json::<Release>()
    }
}

pub struct Releases {}
impl Releases {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Vec<Release>, Error> {
        http_get(repo, Get::Releases)?.json::<Vec<Release>>()
    }
}
// #endregion

// #region Github repo's TAGS
#[derive(Deserialize)]
pub struct Tag {
    pub name: String,
}
pub struct Tags {}
impl Tags {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Vec<Tag>, Error> {
        http_get(repo, Get::Tags)?.json::<Vec<Tag>>()
    }
}
// #endregion

// #region Github repo's BRANCHES
#[derive(Deserialize)]
pub struct Branch {
    pub name: String,
}

pub struct MainBranch {}
impl MainBranch {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Branch, Error> {
        http_get(repo, Get::MainBranch)?.json::<Branch>()
    }
}

pub struct Branches {}
impl Branches {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Vec<Branch>, Error> {
        http_get(repo, Get::Branches)?.json::<Vec<Branch>>()
    }
}
// #endregion

pub fn default_version_of<Repo: AsRef<str>>(repo: Repo) -> Option<String> {
    if let Ok(release) = LatestRelease::of(&repo) {
        return Some(release.tag_name);
    }

    if let Ok(releases) = Releases::of(&repo) {
        if let Some(release) = releases.first() {
            return Some(release.tag_name.clone());
        }
    }

    if let Ok(branch) = MainBranch::of(&repo) {
        return Some(branch.name);
    }

    if let Ok(branches) = Branches::of(&repo) {
        if let Some(branch) = branches.first() {
            return Some(branch.name.clone());
        }
    }

    if let Ok(tags) = Tags::of(&repo) {
        if let Some(tag) = tags.first() {
            return Some(tag.name.clone());
        }
    }

    None
}

fn http_get<Repo: AsRef<str>>(repo: Repo, get: Get) -> Result<Response, Error> {
    let mut url = format!("https://api.github.com/repos/{}/", repo.as_ref());
    url.push_str(match get {
        Get::LatestRelease => "releases/latest",
        Get::Releases => "releases",
        Get::Tags => "tags",
        Get::Branches => "branches",
        Get::MainBranch => "branches/master", // Github will try to redirect this to main
    });

    Client::new().get(url).header(USER_AGENT, "aiken").send()
}
