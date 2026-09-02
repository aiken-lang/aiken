// Auto-generated fuzz test for aiken-lang/aiken PR #1386 (ConstrData overflow).
// See /tmp/aiken-fuzz-1386/fuzz_constr_data.py for the case generator.

use num_bigint::BigInt;
use uplc::builtins::DefaultFunction;
use uplc::machine::runtime::BuiltinSemantics;
use uplc::machine::value::Value;
use uplc::ast::Type;

fn integer_value(s: &str) -> Value {
    Value::integer(s.parse::<BigInt>().unwrap())
}

fn empty_data_list() -> Value {
    Value::list(Type::Data, vec![])
}

#[test]
fn fuzz_constr_data_constructor_index_overflow() {
    // Error enum is private; verify by Debug-format match instead of variant pattern.

    let cases: &[(&str, &str, &str)] = &[
        ("zero", "0", "Ok"),
        ("one", "1", "Ok"),
        ("small_positive", "42", "Ok"),
        ("u64_max_boundary", "18446744073709551615", "Ok"),
        ("u64_max_plus_one", "18446744073709551616", "Err"),
        ("u64_max_plus_two", "18446744073709551617", "Err"),
        ("u64_max_squared", "340282366920938463463374607431768211455", "Err"),
        ("i64_max", "9223372036854775807", "Ok"),
        ("i64_min", "-9223372036854775808", "Err"),
        ("minus_one", "-1", "Err"),
        ("two_pow_64", "18446744073709551616", "Err"),
        ("two_pow_127", "170141183460469231731687303715884105728", "Err"),
        ("two_pow_255", "57896044618658097711785492504343953926634992332820282019728792003956564819967", "Err")
    ];

    let mut panics = Vec::new();
    let mut unexpected = Vec::new();

    for (name, idx_str, expected) in cases {
        let idx = integer_value(idx_str);
        let args = [idx, empty_data_list()];

        let result = std::panic::catch_unwind(|| {
            DefaultFunction::ConstrData.call(
                BuiltinSemantics::C,
                &args,
                &mut vec![],
            )
        });

        match result {
            Ok(Ok(_)) => {
                if *expected != "Ok" {
                    unexpected.push(format!("{}: expected {}, got Ok", name, expected));
                }
            }
            Ok(Err(other)) => {
                let dbg = format!("{:?}", other);
                if *expected != "Err" {
                    unexpected.push(format!("{}: expected Ok, got Err: {}", name, dbg));
                    continue;
                }
                if !dbg.contains("ConstrTagOutOfRange") {
                    unexpected.push(format!("{}: expected ConstrTagOutOfRange, got {}", name, dbg));
                    continue;
                }
                // Informative-message check: flag if error doesn't include the bad value.
                // PR #1386 currently uses TryFromBigIntError::to_string() which is generic;
                // #1373-style would format!() the value in.
                if !idx_str.starts_with('-') && !dbg.contains(idx_str) {
                    println!("note: {}: error msg '{}' does not include input value — improvement opportunity", name, dbg);
                }
            }
            Err(_) => {
                panics.push(name.to_string());
            }
        }
    }

    if !panics.is_empty() {
        panic!("ORIGIN-MAIN BUG: panics on cases: {:?}", panics);
    }
    if !unexpected.is_empty() {
        panic!("UNEXPECTED: {:?}", unexpected);
    }
}

#[test]
fn fuzz_index_bytestring_overflow() {
    // Covered by #1373 (not #1386). Verifies that #1373's fix handles
    // the same boundary cases that #1386 handles for ConstrData.


    let byte_string = Value::byte_string(vec![0u8; 10]);
    let cases: &[(&str, &str, &str)] = &[
        ("zero", "0", "Ok"),
        ("positive_in_range", "5", "Ok"),
        ("negative", "-1", "Ok"),
        ("i64_max", "9223372036854775807", "Ok"),
        ("two_pow_127", "170141183460469231731687303715884105728", "Ok")
    ];

    let mut panics = Vec::new();

    for (name, idx_str, _expected) in cases {
        let idx = integer_value(idx_str);
        let args = [byte_string.clone(), idx];

        let result = std::panic::catch_unwind(|| {
            DefaultFunction::IndexByteString.call(
                BuiltinSemantics::C,
                &args,
                &mut vec![],
            )
        });

        match result {
            Ok(_) => { /* OK or expected Err — we just verify no panic */ }
            Err(_) => {
                panics.push(name.to_string());
            }
        }
    }

    if !panics.is_empty() {
        panic!("#1373-style fix missing: panics on IndexByteString cases: {:?}", panics);
    }
}

#[test]
fn fuzz_slice_bytestring_overflow() {
    // Covered by #1373 (not #1386).


    let byte_string = Value::byte_string(vec![0u8; 10]);
    let cases: &[(&str, &str, &str)] = &[
        ("zero_skip_zero_take", "0", "0"),
        ("negative_skip", "-1", "5"),
        ("negative_take", "0", "-1"),
        ("u64_max_skip", "18446744073709551616", "5"),
        ("u64_max_take", "0", "18446744073709551616")
    ];

    let mut panics = Vec::new();

    for (name, skip_str, take_str) in cases {
        let skip = integer_value(skip_str);
        let take = integer_value(take_str);
        let args = [skip, take, byte_string.clone()];

        let result = std::panic::catch_unwind(|| {
            DefaultFunction::SliceByteString.call(
                BuiltinSemantics::C,
                &args,
                &mut vec![],
            )
        });

        match result {
            Ok(_) => { /* OK or expected Err */ }
            Err(_) => {
                panics.push(name.to_string());
            }
        }
    }

    if !panics.is_empty() {
        panic!("#1373-style fix missing: panics on SliceByteString cases: {:?}", panics);
    }
}
