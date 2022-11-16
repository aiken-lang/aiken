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

pub fn eqInt(a: Int, b: Int) {
  a == b
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
