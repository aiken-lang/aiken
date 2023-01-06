use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/string
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{VerificationKey}

pub type Datum {
  owner: Hash<Blake2b_224, VerificationKey>,
}

pub type Redeemer {
  msg: ByteArray,
}

pub fn spend(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
  let must_say_hello = string.from_bytearray(redeemer.msg) == "Hello, World!"

  let must_be_signed =
    list.has(context.transaction.extra_signatories, datum.owner)

  must_say_hello && must_be_signed
}
