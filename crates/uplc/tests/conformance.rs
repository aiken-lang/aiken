use pallas_primitives::conway::Language;
use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};
use uplc::{
    ast::{Name, NamedDeBruijn, Program},
    machine::{cost_model::ExBudget, runtime::VAN_ROSSEM_PROTOCOL_VERSION},
    parser,
};
use walkdir::WalkDir;

const PARSE_ERROR: &str = "parse error";
const EVALUATION_FAILURE: &str = "evaluation failure";

// CIP-0153 fixtures are copied byte-for-byte from IntersectMBO/plutus revision
// 5f785edeac0d1d89622d44344fdda07ef48e8c73 under
// plutus-conformance/test-cases/uplc/evaluation/builtin/{constant/value,semantics/<builtin>}.
// At that revision, regenerate this PlutusV3 parameter array with:
// cabal run dump-cost-model-parameters -- -V 3 --untagged
//
// Upstream disables its generated unValueData coefficient test over a suspected top-coefficient
// rounding discrepancy. These imported budget fixtures still exercise unValueData against that
// exact dumped model. Run all 98 Value cases with `cargo test -p uplc --test conformance`.
const V3_PV11_COSTS: &[i64] = &[
    100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100,
    16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32,
    132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 123203, 7305, -900,
    1716, 960, 57, 85848, 0, 1, 1, 1000, 42921, 4, 2, 30623, 28755, 75, 1, 898148, 27279, 1, 51775,
    558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10,
    28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32,
    7243, 32, 7391, 32, 11546, 32, 85848, 123203, 7305, -900, 1716, 960, 57, 85848, 0, 1, 90434,
    519, 0, 1, 74433, 32, 85848, 123203, 7305, -900, 1716, 960, 57, 85848, 0, 1, 1, 85848, 123203,
    7305, -900, 1716, 960, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566,
    4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32,
    20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10, 16000,
    100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055, 3756, 18, 267929, 18, 76433006,
    8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022, 1, 166917843, 4307, 36, 284546, 36,
    158221314, 26549, 36, 74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72, 2261318, 64571,
    4, 207616, 8310, 4, 1293828, 28716, 63, 0, 1, 1006041, 43623, 251, 0, 1, 100181, 726, 719, 0,
    1, 100181, 726, 719, 0, 1, 100181, 726, 719, 0, 1, 107878, 680, 0, 1, 95336, 1, 281145, 18848,
    0, 1, 180194, 159, 1, 1, 158519, 8942, 0, 1, 159378, 8813, 0, 1, 107490, 3298, 1, 106057, 655,
    1, 1964219, 24520, 3, 607153, 231697, 53144, 0, 1, 116711, 1957, 4, 231883, 10, 1000, 24838, 7,
    1, 232010, 32, 321837444, 25087669, 18, 617887431, 67302824, 36, 356924, 18413, 45, 21, 219951,
    9444, 1, 1000, 172116, 183150, 6, 24, 21, 213283, 618401, 1998, 28258, 1, 1000, 38159, 2, 22,
    1000, 95933, 1, 1, 11, 1000, 277577, 12, 21,
];

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

fn expected_budget(expected_file: &Path) -> Result<ExBudget, String> {
    let code = fs::read_to_string(expected_file)
        .unwrap_or_else(|err| panic!("failed to read {}: {err}", expected_file.display()));
    let code = code.strip_suffix('\n').unwrap_or(&code);

    match code {
        PARSE_ERROR => Err(PARSE_ERROR.to_string()),
        EVALUATION_FAILURE => Err(EVALUATION_FAILURE.to_string()),
        _ => {
            let fields = code
                .strip_prefix("({cpu: ")
                .and_then(|fields| fields.strip_suffix("})"))
                .unwrap_or_else(|| {
                    panic!(
                        "invalid budget fixture syntax in {}: {code:?}",
                        expected_file.display()
                    )
                });
            let (cpu, mem) = fields.split_once("\n| mem: ").unwrap_or_else(|| {
                panic!(
                    "invalid budget fixture syntax in {}: {code:?}",
                    expected_file.display()
                )
            });
            let cpu = cpu.parse().unwrap_or_else(|err| {
                panic!("invalid CPU budget in {}: {err}", expected_file.display())
            });
            let mem = mem.parse().unwrap_or_else(|err| {
                panic!(
                    "invalid memory budget in {}: {err}",
                    expected_file.display()
                )
            });

            Ok(ExBudget { cpu, mem })
        }
    }
}

#[derive(Debug)]
struct ConformanceResult {
    result: Result<Program<NamedDeBruijn>, String>,
    budget: Result<ExBudget, String>,
}

fn actual_evaluation_result(
    file: &Path,
    language: &Language,
    protocol_major_version: u16,
    costs: &[i64],
) -> ConformanceResult {
    let code = fs::read_to_string(file).expect("Failed to read .uplc file");

    let program = match parser::program(&code) {
        Ok(program) => program,
        Err(_) => {
            return ConformanceResult {
                result: Err(PARSE_ERROR.to_string()),
                budget: Err(PARSE_ERROR.to_string()),
            };
        }
    };

    let program: Program<NamedDeBruijn> = match program.try_into() {
        Ok(program) => program,
        Err(_) => {
            return ConformanceResult {
                result: Err(EVALUATION_FAILURE.to_string()),
                budget: Err(EVALUATION_FAILURE.to_string()),
            };
        }
    };

    let version = program.version;
    let eval = program.eval_as_with_protocol(language, protocol_major_version, costs, None);
    let cost = eval.cost();

    match eval.result() {
        Ok(term) => ConformanceResult {
            result: Ok(Program { version, term }),
            budget: Ok(cost),
        },
        Err(_) => ConformanceResult {
            result: Err(EVALUATION_FAILURE.to_string()),
            budget: Err(EVALUATION_FAILURE.to_string()),
        },
    }
}

fn plutus_conformance_tests(
    root: &str,
    language: Language,
    protocol_major_version: u16,
    costs: &[i64],
    compare_budgets: bool,
) {
    for entry in WalkDir::new(root).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();

        if path.extension().and_then(OsStr::to_str) == Some("uplc") {
            let expected_file = path.with_extension("uplc.expected");

            let actual = actual_evaluation_result(path, &language, protocol_major_version, costs);
            let expected = expected_to_program(&expected_file)
                .map(|program| Program::<NamedDeBruijn>::try_from(program).unwrap());

            pretty_assertions::assert_eq!(
                expected,
                actual.result,
                "result fixture for {}",
                path.display()
            );
            if compare_budgets {
                let budget_file = path.with_extension("uplc.budget.expected");
                let expected_cost = expected_budget(&budget_file);

                pretty_assertions::assert_eq!(
                    expected_cost,
                    actual.budget,
                    "budget fixture {} for {}",
                    budget_file.display(),
                    path.display()
                );
            }
        }
    }
}

#[test]
fn plutus_conformance_tests_v2() {
    plutus_conformance_tests(
        "test_data/conformance/v2",
        Language::PlutusV2,
        11,
        &[
            100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4,
            16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100,
            16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769,
            4, 2, 85848, 228465, 122, 0, 1, 1, 1000, 42921, 4, 2, 30623, 28755, 75, 1, 898148,
            27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32,
            76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541,
            1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 228465, 122,
            0, 1, 1, 90434, 519, 0, 1, 74433, 32, 85848, 228465, 122, 0, 1, 1, 85848, 228465, 122,
            0, 1, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0,
            141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32,
            25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10, 1293828,
            28716, 63, 0, 1, 1006041, 43623, 251, 0, 1, 16000, 100, 16000, 100, 962335, 18,
            2780678, 6, 442008, 1, 52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122,
            18, 1995836, 36, 3227919, 12, 901022, 1, 166917843, 4307, 36, 284546, 36, 158221314,
            26549, 36, 74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72, 2261318, 64571, 4,
            207616, 8310, 4, 100181, 726, 719, 0, 1, 100181, 726, 719, 0, 1, 100181, 726, 719, 0,
            1, 107878, 680, 0, 1, 95336, 1, 281145, 18848, 0, 1, 180194, 159, 1, 1, 158519, 8942,
            0, 1, 159378, 8813, 0, 1, 107490, 3298, 1, 106057, 655, 1, 1964219, 24520, 3, 607153,
            231697, 53144, 0, 1, 116711, 1957, 4, 231883, 10, 1000, 24838, 7, 1, 232010, 32,
            321837444, 25087669, 18, 617887431, 67302824, 36, 356924, 18413, 45, 21, 219951, 9444,
            1, 1000, 172116, 183150, 6, 24, 21, 213283, 618401, 1998, 28258, 1, 1000, 38159, 2, 22,
            1000, 95933, 1, 1, 11, 1000, 277577, 12, 21,
        ],
        // This root combines budget fixtures from incompatible upstream cost-model revisions
        // (legacy BLS fixtures and newer CEK/builtin fixtures). Result conformance remains valid;
        // budget conformance is enabled for the homogeneous V3 PV11 root below.
        false,
    )
}

#[test]
fn plutus_conformance_tests_v3() {
    plutus_conformance_tests(
        "test_data/conformance/v3",
        Language::PlutusV3,
        VAN_ROSSEM_PROTOCOL_VERSION,
        V3_PV11_COSTS,
        true,
    )
}
