use aiken/collection/list
use aiken/interval
use cardano/address.{
  Address, Credential, Inline, Pointer, Script, VerificationKey,
}
use cardano/transaction.{Transaction}

const only0s = #"00000000000000000000000000000000000000000000000000000000"

const only1s = #"11111111111111111111111111111111111111111111111111111111"

validator thing {
  spend(_datum: Option<Void>, _rdmr: Void, _oref: Data, _tx: Data) {
    True
  }

  withdraw(_rdmr: Void, credential: Credential, transaction: Transaction) {
    expect Script(my_script_hash) = credential

    expect interval.entirely_before(1596059092000) == transaction.validity_range

    let addresses = list.map(transaction.outputs, fn(out) { out.address })

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
      [Pair(Script(my_script_hash), 0), Pair(VerificationKey(only0s), 14)] == transaction.withdrawals

    True
  }

  else(_ctx) {
    fail
  }
}
