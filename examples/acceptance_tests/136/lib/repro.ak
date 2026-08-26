use aiken/fuzz
use cardano/assets.{AssetName, PolicyId}
use cardano/assets
use cardano/fuzz as cardano_fuzz

const asset_name_fuzzer = fn(_policy: PolicyId) { cardano_fuzz.asset_name() }

const quantity_fuzzer = fn(_policy: PolicyId, _asset: AssetName) {
  fuzz.int_at_least(1)
}

test issue_1309_const_function_fuzzers(
  value via cardano_fuzz.value_with(
    assets.zero,
    fuzz.list(cardano_fuzz.policy_id()),
    asset_name_fuzzer,
    quantity_fuzzer,
  ),
) {
  value == value
}
