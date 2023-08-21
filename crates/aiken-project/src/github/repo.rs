use super::payload::{branch::Branch, release::Release, repo::Repo as RepoInfo, tag::Tag};
use reqwest::{
    blocking::{Client, Response},
    header::USER_AGENT,
    Error,
};
use std::time::Duration;

enum Get {
    Releases,
    Tags,
    Branches,
    Info,
}

enum Query<T> {
    Param(T),
    All,
}

const ALL: Query<String> = Query::All;

// #region Github repo's RELEASES
pub struct LatestRelease {}
impl LatestRelease {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Release, Error> {
        http_get(repo, Get::Releases, Query::Param("latest"))?.json::<Release>()
    }
}

pub struct Releases {}
impl Releases {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Vec<Release>, Error> {
        http_get(repo, Get::Releases, ALL)?.json::<Vec<Release>>()
    }
}
// #endregion

// #region Github repo's TAGS
pub struct Tags {}
impl Tags {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Vec<Tag>, Error> {
        http_get(repo, Get::Tags, ALL)?.json::<Vec<Tag>>()
    }
}
// #endregion

// #region Github repo's BRANCHES
pub struct MainBranch {}
impl MainBranch {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Branch, Error> {
        http_get(
            repo,
            Get::Branches,
            Query::Param("master"), // Github will try to redirect this to `main`
        )?
        .json::<Branch>()
    }
}

pub struct DefaultBranch {}
impl DefaultBranch {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Branch, Error> {
        http_get(
            &repo,
            Get::Branches,
            Query::Param(Info::of(&repo)?.default_branch),
        )?
        .json::<Branch>()
    }
}

pub struct Branches {}
impl Branches {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<Vec<Branch>, Error> {
        http_get(repo, Get::Branches, ALL)?.json::<Vec<Branch>>()
    }
}
// #endregion

// #region Github repo's INFO
pub struct Info {}
impl Info {
    pub fn of<Repo: AsRef<str>>(repo: Repo) -> Result<RepoInfo, Error> {
        http_get(repo, Get::Info, ALL)?.json::<RepoInfo>()
    }
}
// #endregion

/// Sends an HTTP GET Request to Github API.
///
/// # Parameters
///
/// - `repo` must be in this format `owner/repo`.
///   It accepts `str`, `String`, or `impl AsRef<str>`.
///
/// - `param` is used to query a certain `release`, `tag`, or `branch`.
///
/// # Example
///
/// `http_get("owner/repo", Get::Branches, Query::Param("branch_name"))`
/// means it queries for the branch `branch_name`.
fn http_get<Repo: AsRef<str>, Param: AsRef<str>>(
    repo: Repo,
    get: Get,
    param: Query<Param>,
) -> Result<Response, Error> {
    let mut url = format!("https://api.github.com/repos/{}", repo.as_ref());
    let path;
    url.push_str(match get {
        Get::Releases => {
            path = format_path("/releases", param);
            path.as_ref()
        }
        Get::Tags => {
            path = match param {
                Query::All => format_path("/tags", Query::All),
                param => format_path("/releases/tags", param),
            };
            path.as_ref()
        }
        Get::Branches => {
            path = format_path("/branches", param);
            path.as_ref()
        }
        Get::Info => "",
    });

    Client::builder()
        .timeout(Duration::from_secs(5)) // it's not cool waiting for too long
        .build()?
        .get(url)
        .header(USER_AGENT, "aiken-lang")
        .send()
}

fn format_path<Path: AsRef<str>, Param: AsRef<str>>(
    path: Path,
    param: Query<Param>,
) -> impl AsRef<str> {
    match param {
        Query::Param(param) => format!("{}/{}", path.as_ref(), param.as_ref()),
        _ => format!("{}", path.as_ref()),
    }
}
