pub type Signer {
  hash: ByteArray,
}

pub type ScriptContext {
  signer: Signer,
}

pub type Redeem {
  Buy { tipo: ByteArray, fin: Int }
  Sell { twice: ByteArray, find: Int }
  Hold(Int)
}

pub type Datum {
  fin: Int,
  sc: ScriptContext,
  rdmr: Redeem,
}

pub fn eqIntPlusOne(a: Int, b: Int) {
  a + 1 == b
}

pub fn eqString(a: ByteArray, b: ByteArray) {
  a == b
}

pub type Thing {
  Some
  None
}

pub type Other {
  Wow
  Yes
}

pub fn incrementor(counter: Int, target: Int) -> Int {
  if counter == target {
    counter
  } else if counter > target {
    counter - target
  } else {
    incrementor(counter + 1, target)
  }
}

pub const big_a = 5
