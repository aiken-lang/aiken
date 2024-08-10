use aiken/collection/dict.{Dict}
use aiken/collection/list
use cardano/assets.{PolicyId, Value}
use cardano/credential
use cardano/transaction.{
  DatumHash, Input, Mint, Minting, NoDatum, Output, OutputReference,
  ScriptContext, ScriptInfo, ScriptPurpose,
}

const null28 = #"00000000000000000000000000000000000000000000000000000000"

const null32 =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const void_hash =
  #"923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"

validator {
  fn mint_1(_tmp2: Void, ctx: ScriptContext) {
    let our_policy_id = assert_script_info(ctx.info)
    let other_policy_id = assert_redeemers(ctx.transaction.redeemers)
    assert_outputs(ctx.transaction.outputs, our_policy_id, other_policy_id)
    assert_mint(ctx.transaction.mint, our_policy_id, other_policy_id)
    assert_reference_inputs(ctx.transaction.reference_inputs)
    assert_datums(ctx.transaction.datums)
    True
  }
}

fn assert_reference_inputs(inputs: List<Input>) -> Void {
  expect
    [
      Input {
        output_reference: OutputReference {
          transaction_id: null32,
          output_index: 0,
        },
        output: Output {
          address: credential.from_verification_key(null28),
          value: assets.from_lovelace(1_000_000),
          datum: NoDatum,
          reference_script: None,
        },
      },
    ] == inputs
  Void
}

fn assert_script_info(info: ScriptInfo) -> PolicyId {
  expect Minting(policy_id) = info
  policy_id
}

fn assert_redeemers(redeemers: Pairs<ScriptPurpose, Data>) -> PolicyId {
  expect [Pair(Mint(other_policy_id), data), _] = redeemers
  expect Void = data
  other_policy_id
}

fn assert_datums(datums: Dict<ByteArray, Data>) -> Void {
  let void: Data = Void
  expect [Pair(void_hash, void)] == dict.to_pairs(datums)
  Void
}

fn assert_outputs(
  outputs: List<Output>,
  our_policy_id: PolicyId,
  other_policy_id: PolicyId,
) {
  expect list.length(outputs) == 3

  expect
    Some(
      Output {
        address: credential.from_verification_key(null28),
        value: assets.from_lovelace(1_000_000),
        datum: DatumHash(void_hash),
        reference_script: None,
      },
    ) == list.at(outputs, 0)

  expect
    Some(
      Output {
        address: credential.from_verification_key(null28)
          |> credential.with_delegation_key(null28),
        value: assets.from_lovelace(1_000_000)
          |> assets.add(our_policy_id, "tuna", 100000000000000)
          |> assets.add(other_policy_id, "aiken", 42),
        datum: NoDatum,
        reference_script: None,
      },
    ) == list.at(outputs, 1)

  expect
    Some(
      Output {
        address: credential.from_script(null28)
          |> credential.with_delegation_key(null28),
        value: assets.from_lovelace(1_000_000)
          |> assets.add(other_policy_id, "cardano", 1),
        datum: NoDatum,
        reference_script: Some(
          #"68ad54b3a8124d9fe5caaaf2011a85d72096e696a2fb3d7f86c41717",
        ),
      },
    ) == list.at(outputs, 2)

  Void
}

fn assert_mint(mint: Value, our_policy_id: PolicyId, other_policy_id: PolicyId) {
  expect
    [
      (other_policy_id, "aiken", -14),
      (other_policy_id, "cardano", 1),
      (our_policy_id, "tuna", 100000000000000),
    ] == assets.flatten(mint)
  Void
}

validator {
  fn mint_2(_tmp2: Void, _ctx: ScriptContext) {
    trace @"_____mint_2_____"
    True
  }
}
