use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

use uplc::{
    ast::{Name, NamedDeBruijn, Program},
    parser,
};
use walkdir::WalkDir;

const PARSE_ERROR: &str = "parse error";
const EVALUATION_FAILURE: &str = "evaluation failure";

fn expected_to_program(expected_file: &PathBuf) -> Result<Program<Name>, String> {
    let code = fs::read_to_string(expected_file).expect("Failed to read .uplc.expected file");

    if code.contains(PARSE_ERROR) {
        Err(PARSE_ERROR.to_string())
    } else if code.contains(EVALUATION_FAILURE) {
        Err(EVALUATION_FAILURE.to_string())
    } else {
        parser::program(&code).map_err(|_| code)
    }
}

fn actual_evaluation_result(file: &Path) -> Result<Program<Name>, String> {
    let code = fs::read_to_string(file).expect("Failed to read .uplc file");

    let program = parser::program(&code).map_err(|_| PARSE_ERROR.to_string())?;

    let program: Program<NamedDeBruijn> = program
        .try_into()
        .map_err(|_| EVALUATION_FAILURE.to_string())?;

    let version = program.version;

    let term = program
        .eval(Default::default())
        .result()
        .map_err(|_| EVALUATION_FAILURE.to_string())?;

    let program = Program { version, term };

    Ok(program.try_into().unwrap())
}

#[test]
fn evaluation() {
    let root = "test_data/conformance/evaluation";

    for entry in WalkDir::new(root).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();

        if path.extension().and_then(OsStr::to_str) == Some("uplc") {
            let expected_file = path.with_extension("uplc.expected");

            let actual = actual_evaluation_result(path);
            let expected = expected_to_program(&expected_file);

            pretty_assertions::assert_eq!(expected, actual, "{}", path.display());
        }
    }
}
