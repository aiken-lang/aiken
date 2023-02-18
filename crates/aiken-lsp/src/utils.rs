use std::path::PathBuf;

use lsp_types::TextEdit;

use crate::error::Error;

pub const COMPILING_PROGRESS_TOKEN: &str = "compiling-gleam";
pub const CREATE_COMPILING_PROGRESS_TOKEN: &str = "create-compiling-progress-token";

pub fn text_edit_replace(new_text: String) -> TextEdit {
    TextEdit {
        range: lsp_types::Range {
            start: lsp_types::Position {
                line: 0,
                character: 0,
            },
            end: lsp_types::Position {
                line: u32::MAX,
                character: 0,
            },
        },
        new_text,
    }
}

pub fn path_to_uri(path: PathBuf) -> Result<lsp_types::Url, Error> {
    let mut file: String = "file://".into();

    file.push_str(&path.as_os_str().to_string_lossy());

    let uri = lsp_types::Url::parse(&file)?;

    Ok(uri)
}
