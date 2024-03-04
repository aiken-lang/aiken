use aiken/cbor.{diagnostic, serialise}

test cbor_serialise_large_num() {
  serialise(18446744073709551615) == #"1bffffffffffffffff"
}

test cbor_diagnostic_large_num() {
  diagnostic(18446744073709551615) == @"18446744073709551615"
}
