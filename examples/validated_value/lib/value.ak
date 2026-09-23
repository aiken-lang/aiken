use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use cardano/assets.{AssetName, PolicyId, Value}

/// Check both map levels without sorting or dropping entries. Fails on
/// nonascending or duplicate keys, empty asset maps, or zero quantities.
/// The empty outer map and signed nonzero quantities are valid Values.
/// This does not check ledger key widths, ADA namespace rules, or positivity.
pub fn is_canonical_value(xs: Pairs<PolicyId, Pairs<AssetName, Int>>) -> Value {
  xs
    |> list.map(
        fn(Pair(policy, entries)) {
          Pair(policy, dict.from_ascending_pairs(entries))
        },
      )
    |> assets.from_ascending_pairs
}

/// Only use after the exact field's invariants have been established, either
/// locally or by authenticating state whose minting and continuing transitions
/// enforce them. A token's presence alone does not establish this trust.
/// The compiler checks representation compatibility; this performs no checks.
pub fn unsafe_coerce_value(xs: Pairs<PolicyId, Pairs<AssetName, Int>>) -> Value {
  builtin.unsafe_coerce(xs)
}

test checked_and_trusted_values_preserve_data() {
  let xs = [
    Pair(#"", [Pair(#"", 12_000_000)]),
    Pair(
      #"11111111111111111111111111111111111111111111111111111111",
      [Pair("alpha", 5), Pair("beta", 7)],
    ),
  ]
  let checked = is_canonical_value(xs)
  let trusted = unsafe_coerce_value(xs)
  and {
    checked == trusted,
    as_data(xs) == as_data(trusted),
    assets.lovelace_of(trusted) == 12_000_000,
  }
}

test checked_empty_value() {
  is_canonical_value([]) == assets.zero
}

test checked_signed_quantity() {
  is_canonical_value([Pair("policy", [Pair("alpha", -1)])]) == assets.from_asset(
    "policy",
    "alpha",
    -1,
  )
}

// A successful conversion always makes these rejection tests return True,
// so expected-failure tests cannot pass merely because an equality is False.
fn checked_size(xs: Pairs<PolicyId, Pairs<AssetName, Int>>) -> Int {
  xs |> is_canonical_value |> assets.to_pairs |> list.length
}

test checked_rejects_policy_order() fail {
  checked_size([Pair("z", [Pair("alpha", 1)]), Pair("a", [Pair("beta", 1)])]) >= 0
}

test checked_rejects_duplicate_policy() fail {
  checked_size([Pair("a", [Pair("alpha", 1)]), Pair("a", [Pair("beta", 1)])]) >= 0
}

test checked_rejects_asset_order() fail {
  checked_size([Pair("a", [Pair("beta", 1), Pair("alpha", 1)])]) >= 0
}

test checked_rejects_duplicate_asset() fail {
  checked_size([Pair("a", [Pair("alpha", 1), Pair("alpha", 1)])]) >= 0
}

test checked_rejects_zero() fail {
  checked_size([Pair("a", [Pair("alpha", 0)])]) >= 0
}

test checked_rejects_empty_assets() fail {
  checked_size([Pair("a", [])]) >= 0
}
