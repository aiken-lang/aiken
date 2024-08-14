use aiken/collection/list
use aiken/interval
use cardano/credential.{Address, Inline, Pointer, Script, VerificationKey}
use cardano/transaction.{ScriptContext, Withdrawing}

const only0s = #"00000000000000000000000000000000000000000000000000000000"

const only1s = #"11111111111111111111111111111111111111111111111111111111"

validator {
  fn spend(_tmp0: Void, _tmp1: Void, _ctx: Data) {
    True
  }

  fn withdraw(_tmp0: Void, ctx: ScriptContext) {
    expect Withdrawing(Script(my_script_hash)) = ctx.info

    expect
      interval.entirely_before(1596059092000) == ctx.transaction.validity_range

    let addresses = list.map(ctx.transaction.outputs, fn(out) { out.address })

    expect
      Some(
        Address {
          payment_credential: VerificationKey(only0s),
          stake_credential: Some(Inline(Script(only1s))),
        },
      ) == list.at(addresses, 0)

    expect
      Some(
        Address {
          payment_credential: VerificationKey(only0s),
          stake_credential: Some(Pointer(2498243, 27, 3)),
        },
      ) == list.at(addresses, 1)

    expect
      Some(
        Address {
          payment_credential: Script(only1s),
          stake_credential: Some(Pointer(2498243, 27, 3)),
        },
      ) == list.at(addresses, 2)

    expect
      [Pair(Script(my_script_hash), 0), Pair(VerificationKey(only0s), 14)] == ctx.transaction.withdrawals

    True
  }
}
