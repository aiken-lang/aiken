use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use cardano/transaction.{OutputReference, Transaction}

pub type Datum {
  owner: VerificationKeyHash,
}

pub type Redeemer {
  msg: ByteArray,
}

validator hello_world {
  spend(
    datum: Option<Datum>,
    redeemer: Redeemer,
    _: OutputReference,
    transaction: Transaction,
  ) {
    let must_say_hello = redeemer.msg == "Hello, World!"

    expect Some(Datum { owner }) = datum

    let must_be_signed = list.has(transaction.extra_signatories, owner)

    must_say_hello && must_be_signed
  }

  else(_) {
    fail
  }
}
