use crate::{
    config::{Config, Platform},
    CheckedModule,
};
use aiken_lang::{ast::Span, line_numbers::LineNumbers};
use camino::{Utf8Component, Utf8Path};
use std::path::Path;

pub struct SourceLinker {
    line_numbers: LineNumbers,
    url_pattern: Option<(String, String)>,
}

impl SourceLinker {
    pub fn new(root: &Path, config: &Config, module: &CheckedModule) -> Self {
        let utf8_path = <&Utf8Path>::try_from(
            module
                .input_path
                .as_path()
                .strip_prefix(root)
                .expect("root path isn't a prefix of project modules' paths!"),
        )
        .expect("module path contains non UTF-8 characters!");

        let path_in_repo = to_url_path(utf8_path).unwrap_or_default();

        let url_pattern = config
            .repository
            .as_ref()
            .map(|repository| match repository.platform {
                Platform::Github => (
                    format!(
                        "https://github.com/{}/{}/blob/{}/{}#L",
                        repository.user, repository.project, config.version, path_in_repo
                    ),
                    "-L".into(),
                ),
                Platform::Gitlab => (
                    format!(
                        "https://gitlab.com/{}/{}/-/blob/{}/{}#L",
                        repository.user, repository.project, config.version, path_in_repo
                    ),
                    "-".into(),
                ),
                Platform::Bitbucket => (
                    format!(
                        "https://bitbucket.com/{}/{}/src/{}/{}#lines-",
                        repository.user, repository.project, config.version, path_in_repo
                    ),
                    ":".into(),
                ),
            });

        SourceLinker {
            line_numbers: LineNumbers::new(&module.code),
            url_pattern,
        }
    }

    pub fn url(&self, span: Span) -> String {
        match &self.url_pattern {
            Some((base, line_sep)) => {
                match (
                    self.line_numbers.line_number(span.start),
                    self.line_numbers.line_number(span.end),
                ) {
                    (Some(start_line), Some(end_line)) => {
                        format!("{base}{start_line}{line_sep}{end_line}")
                    }
                    (Some(start_line), None) => format!("{base}{start_line}"),
                    _ => base.to_string(),
                }
            }
            None => "".into(),
        }
    }
}

fn to_url_path(path: &Utf8Path) -> Option<String> {
    let mut buf = String::new();
    for c in path.components() {
        if let Utf8Component::Normal(s) = c {
            buf.push_str(s);
        }
        buf.push('/');
    }

    let _ = buf.pop();

    Some(buf)
}
