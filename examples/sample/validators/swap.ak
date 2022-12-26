use aiken/list
use aiken/string
use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{VerificationKey}

pub type Datum {
  owner: Hash<Blake2b_224, VerificationKey>,
}

pub type Redeemer {
  msg: ByteArray,
}

pub type Dummy {
  Mannequin {
    hands: ByteArray,
    feet: Int,
  }
  Doll {
    hands: ByteArray,
    datum: Datum,
    feet: Int,
  }
  Puppet {
    hands: ByteArray,
    dummy: Dummy,
  }
  Statue {
    hands: ByteArray,
    boots: ByteArray,
  }
}
 
pub fn spend(datum: Datum, redeemer: Redeemer, context: Dummy) -> Bool {
  let must_say_hello = string.from_bytearray(redeemer.msg) == "Hello, World!"
  let must_be_signed = #(1, datum, #(redeemer.msg, context ))
    // context.transaction.extra_signatories
    // |> list.any(fn(vk) { vk == datum.owner })

  when must_be_signed is {
    #(a, b, #(c, Mannequin{ feet, ..})) -> feet == 2
    _ -> False
    
  }
 
}