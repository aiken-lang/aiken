use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{VerificationKey}

pub type Datum {
  owner: Hash<Blake2b_224, VerificationKey>,
}

pub type Redeemer {
  msg: ByteArray,
}

validator hello_world {
  spend(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
    let must_say_hello = redeemer.msg == "Hello, World!"

    let must_be_signed =
      list.has(context.transaction.extra_signatories, datum.owner)

    must_say_hello && must_be_signed
  }

  else(_ctx) {
    fail
  }
}
