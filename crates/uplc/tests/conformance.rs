use pallas_primitives::conway::Language;
use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};
use uplc::{
    ast::{Name, NamedDeBruijn, Program},
    machine::cost_model::ExBudget,
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

fn file_to_budget(expected_budget_file: &PathBuf) -> Result<ExBudget, String> {
    fs::read_to_string(expected_budget_file)
        .map_err(|e| format!("failed to read .uplc.budget.expected file: {e:?}"))
        .and_then(|src| budget::ex_budget(&src).map_err(|e| format!("{e:?}")))
}

peg::parser! {
    grammar budget() for str {
        pub rule ex_budget() -> ExBudget
          = "({" _* "cpu" _* ":" _* cpu:decimal() _* "|" _* "mem" _* ":" _* mem:decimal() _* "})" _* {
              ExBudget { cpu, mem }
          }

        rule decimal() -> i64
          = n:$(['0'..='9']+) {? n.parse().or(Err("decimal")) }

        rule _ = [' ' | '\n' | '\r' | '\t'] / "--" $([^ '\n']*) "\n"
    }
}

fn actual_evaluation_result(
    file: &Path,
    language: &Language,
) -> Result<(Program<Name>, ExBudget), String> {
    let code = fs::read_to_string(file).expect("Failed to read .uplc file");

    let program = parser::program(&code).map_err(|_| PARSE_ERROR.to_string())?;

    let program: Program<NamedDeBruijn> = program
        .try_into()
        .map_err(|_| EVALUATION_FAILURE.to_string())?;

    let version = program.version;

    let eval = program.eval_version(Default::default(), language);

    let cost = eval.cost();

    let term = eval.result().map_err(|_| EVALUATION_FAILURE.to_string())?;

    let program = Program { version, term };

    Ok((program.try_into().unwrap(), cost))
}

fn plutus_conformance_tests(language: Language) {
    let root = format!(
        "test_data/conformance/{}",
        match language {
            Language::PlutusV1 => "v1",
            Language::PlutusV2 => "v2",
            Language::PlutusV3 => "v3",
        }
    );

    for entry in WalkDir::new(root).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();

        if path.extension().and_then(OsStr::to_str) == Some("uplc") {
            let expected_file = path.with_extension("uplc.expected");
            let expected_budget_file = path.with_extension("uplc.budget.expected");

            let eval = actual_evaluation_result(path, &language);
            let expected = expected_to_program(&expected_file);

            match eval {
                Ok((actual, cost)) => {
                    pretty_assertions::assert_eq!(expected, Ok(actual), "{}", path.display());
                    match language {
                        Language::PlutusV1 | Language::PlutusV2 => {}
                        Language::PlutusV3 => {
                            if let Ok(budget) = file_to_budget(&expected_budget_file) {
                                pretty_assertions::assert_eq!(budget, cost, "{}", path.display());
                            }
                        }
                    }
                }
                Err(err) => pretty_assertions::assert_eq!(expected, Err(err), "{}", path.display()),
            }
        }
    }
}

#[test]
fn plutus_conformance_tests_v2() {
    plutus_conformance_tests(Language::PlutusV2)
}

#[test]
fn plutus_conformance_tests_v3() {
    plutus_conformance_tests(Language::PlutusV3)
}
