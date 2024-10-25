use crate::error::Error;
use aiken_lang::{ast::Span, line_numbers::LineNumbers};
use itertools::Itertools;
use lsp_types::TextEdit;
use std::path::{Path, PathBuf};
use urlencoding::decode;

pub const COMPILING_PROGRESS_TOKEN: &str = "compiling-aiken";
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

#[allow(clippy::result_large_err)]
pub fn path_to_uri(path: PathBuf) -> Result<lsp_types::Url, Error> {
    let mut file: String = "file://".into();

    file.push_str(&path.as_os_str().to_string_lossy());

    let uri = lsp_types::Url::parse(&file)?;

    Ok(uri)
}

pub fn span_to_lsp_range(location: Span, line_numbers: &LineNumbers) -> lsp_types::Range {
    let start = line_numbers
        .line_and_column_number(location.start)
        .expect("Spans are within bounds");
    let end = line_numbers
        .line_and_column_number(location.end)
        .expect("Spans are within bounds");

    lsp_types::Range {
        start: lsp_types::Position {
            line: start.line as u32 - 1,
            character: start.column as u32 - 1,
        },
        end: lsp_types::Position {
            line: end.line as u32 - 1,
            character: end.column as u32 - 1,
        },
    }
}

pub fn uri_to_module_name(uri: &url::Url, root: &Path) -> Option<String> {
    let path = if cfg!(target_os = "windows") {
        let mut uri_path = decode(&uri.path().replace('/', "\\"))
            .expect("Invalid formatting")
            .to_string();

        if uri_path.starts_with('\\') {
            uri_path = uri_path
                .strip_prefix('\\')
                .expect("Failed to remove \"\\\" prefix")
                .to_string();
        }

        PathBuf::from(uri_path)
    } else {
        PathBuf::from(uri.path())
    };

    let components = path
        .strip_prefix(root)
        .ok()?
        .components()
        .skip(1)
        .map(|c| c.as_os_str().to_string_lossy().replace('-', "_"));

    let module_name = Itertools::intersperse(components, "/".into())
        .collect::<String>()
        .strip_suffix(".ak")?
        .to_string();

    Some(module_name)
}
