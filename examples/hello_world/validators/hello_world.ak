use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{VerificationKey}

type Datum {
  owner: Hash<Blake2b_224, VerificationKey>,
}

type Redeemer {
  msg: ByteArray,
}

validator {
  fn spend(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
    let must_say_hello = redeemer.msg == "Hello, World!"

    let must_be_signed =
      list.has(context.transaction.extra_signatories, datum.owner)

    must_say_hello && must_be_signed
  }
}

type ABC {
  a: ByteArray,
  b: Int,
  c: ByteArray,
}

type XYZ {
  a: ByteArray,
  b: ByteArray,
  c: ByteArray,
  d: Int,
  e: ABC,
}

fn recursive(a: ByteArray, b: Int, c: XYZ, d: Int, e: Int) -> ByteArray {
  if c.e.a == "a" {
    "d"
  } else if b == 0 {
    a
  } else {
    recursive(a, b - 1, c, d, e)
  }
}

test hah() {
  expect "a" == recursive("a", 30, XYZ("", "", "", 1, ABC("", 1, "")), 2, 5)
  True
}
