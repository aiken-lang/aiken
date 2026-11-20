// Fuzz test for Aiken's bls12_381 G1 multi-scalar multiplication builtin.
// Compares Aiken's MSM output against py_ecc reference values.
//
// Reference values computed via py_ecc.bls12_381 + custom bls12-381 G1
// compression function (top byte = 0x80 | sort_flag<<5 | x[0] without
// its top bit; infinity point encoded as 0xc0 || zeros).
//
// See /tmp/aiken-fuzz-1349/fuzz_msm_g1.py for the reference generator.

use super::TestProject;
use crate::module::CheckedModules;
use aiken_lang::ast::{Definition, TraceLevel, Tracing};
use pallas_primitives::conway::Language;
use uplc::{
    ast::{DeBruijn, Program},
    machine::cost_model::ExBudget,
};

// Reference values from py_ecc (see generator script)
// G_HEX     = 97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb
// G2G_HEX   = a572cbea904d67468808c8eb50a9450c9721db309128012543902d0ac358a62ae28f75bb8f1c7c42c39a8c5529bf0f4e

const CASES: &[(&str, &str)] = &[
    // msm_basic: MSM([1, 2], [G, 2G]) = 5G
    ("msm_basic", r##"
        use aiken/builtin
        test msm_basic() {
          let g =
            #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
          let two_g =
            #<Bls12_381, G1>"a572cbea904d67468808c8eb50a9450c9721db309128012543902d0ac358a62ae28f75bb8f1c7c42c39a8c5529bf0f4e"
          let expected =
            #<Bls12_381, G1>"b0e7791fb972fe014159aa33a98622da3cdc98ff707965e536d8636b5fcc5ac7a91a8c46e59a00dca575af0f18fb13dc"
          builtin.bls12_381_g1_equal(
            builtin.bls12_381_g1_multi_scalar_mul([1, 2], [g, two_g]),
            expected,
          )
        }
    "##),

    // msm_zero_scalar: MSM([0, 5], [G, 2G]) = 10G (first term contributes nothing)
    ("msm_zero_scalar", r##"
        use aiken/builtin
        test msm_zero_scalar() {
          let g =
            #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
          let two_g =
            #<Bls12_381, G1>"a572cbea904d67468808c8eb50a9450c9721db309128012543902d0ac358a62ae28f75bb8f1c7c42c39a8c5529bf0f4e"
          let expected =
            #<Bls12_381, G1>"af81da25ecf1c84b577fefbedd61077a81dc43b00304015b2b596ab67f00e41c86bb00ebd0f90d4b125eb0539891aeed"
          builtin.bls12_381_g1_equal(
            builtin.bls12_381_g1_multi_scalar_mul([0, 5], [g, two_g]),
            expected,
          )
        }
    "##),

    // msm_all_zero: MSM([0, 0], [G, 2G]) = identity
    ("msm_all_zero", r##"
        use aiken/builtin
        test msm_all_zero() {
          let g =
            #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
          let two_g =
            #<Bls12_381, G1>"a572cbea904d67468808c8eb50a9450c9721db309128012543902d0ac358a62ae28f75bb8f1c7c42c39a8c5529bf0f4e"
          let expected =
            #<Bls12_381, G1>"c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
          builtin.bls12_381_g1_equal(
            builtin.bls12_381_g1_multi_scalar_mul([0, 0], [g, two_g]),
            expected,
          )
        }
    "##),

    // msm_neg_wrap: MSM([-1, 1], [G, 2G]) = G
    ("msm_neg_wrap", r##"
        use aiken/builtin
        test msm_neg_wrap() {
          let g =
            #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
          let two_g =
            #<Bls12_381, G1>"a572cbea904d67468808c8eb50a9450c9721db309128012543902d0ac358a62ae28f75bb8f1c7c42c39a8c5529bf0f4e"
          let expected =
            #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
          builtin.bls12_381_g1_equal(
            builtin.bls12_381_g1_multi_scalar_mul([-1, 1], [g, two_g]),
            expected,
          )
        }
    "##),

    // msm_three_terms: MSM([3, 7, 11], [G, 2G, G]) = 14G + 14G (wait, that's 3+11=14G + 7*2G = 28G total)
    ("msm_three_terms", r##"
        use aiken/builtin
        test msm_three_terms() {
          let g =
            #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
          let two_g =
            #<Bls12_381, G1>"a572cbea904d67468808c8eb50a9450c9721db309128012543902d0ac358a62ae28f75bb8f1c7c42c39a8c5529bf0f4e"
          let expected =
            #<Bls12_381, G1>"b6ad11e5d15f77c1143b1697344911b9c590110fdd8dd09df2e58bfd757269169deefe8be3544d4e049fb3776fb0bcfb"
          builtin.bls12_381_g1_equal(
            builtin.bls12_381_g1_multi_scalar_mul([3, 7, 11], [g, two_g, g]),
            expected,
          )
        }
    "##),

    // msm_large_scalar: scalar > 2^64 (BLS12-381 scalar field order is ~2^255)
    ("msm_large_scalar", r##"
        use aiken/builtin
        test msm_large_scalar() {
          let g =
            #<Bls12_381, G1>"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"
          let two_g =
            #<Bls12_381, G1>"a572cbea904d67468808c8eb50a9450c9721db309128012543902d0ac358a62ae28f75bb8f1c7c42c39a8c5529bf0f4e"
          let expected =
            #<Bls12_381, G1>"b207360de2e6b5a82c7616595fb9cd5654d2c0c0a8e006667108a87459d1f64d7c9d31e2940e24ecb3a8864ff4b325f5"
          builtin.bls12_381_g1_equal(
            builtin.bls12_381_g1_multi_scalar_mul([340282366920938463463374607431768211457, 1], [g, two_g]),
            expected,
          )
        }
    "##),
];

fn assert_aiken_test_passes(name: &str, src: &str) -> Result<(), String> {
    let mut project = TestProject::new();

    let parsed = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        project.parse(src)
    })) {
        Ok(p) => p,
        Err(_) => return Err(format!("{}: parse panicked", name)),
    };

    let modules = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        CheckedModules::singleton(project.check(parsed))
    })) {
        Ok(m) => m,
        Err(_) => return Err(format!("{}: type-check panicked", name)),
    };

    let Some(checked_module) = modules.values().next() else {
        return Err(format!("{}: no checked module", name));
    };

    let mut generator = project.new_generator(Tracing::All(TraceLevel::Silent));

    let test = checked_module
        .ast
        .definitions()
        .find_map(|def| match def {
            Definition::Test(func) => Some(func.clone()),
            _ => None,
        })
        .expect("expected a test definition");

    let program = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        generator.generate_raw(&test.body, &[], &checked_module.name)
    })) {
        Ok(p) => p,
        Err(_) => return Err(format!("{}: codegen panicked", name)),
    };

    let debruijn: Program<DeBruijn> = program.try_into().unwrap();
    let eval = debruijn.eval(ExBudget::max());

    if eval.failed(true, &Language::PlutusV3) {
        return Err(format!(
            "{}: eval FAILED (logs: {:?}, result: {:?})",
            name,
            eval.logs(),
            eval.result()
        ));
    }

    Ok(())
}

#[test]
fn bls_fuzz_msm_g1_matches_py_ecc() {
    let mut panics = Vec::new();

    for (name, src) in CASES {
        match assert_aiken_test_passes(name, src) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("{}", e);
                panics.push(e);
            }
        }
    }

    println!(
        "bls_msm_g1 fuzz: {}/{} passed, {} failures",
        CASES.len() - panics.len(),
        CASES.len(),
        panics.len()
    );

    if !panics.is_empty() {
        panic!(
            "bls_msm_g1 fuzz caught failures (deviation from py_ecc reference): {:?}",
            panics
        );
    }
}
