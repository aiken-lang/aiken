
pub type Signer {
    hash: Int
}


pub type ScriptContext {
    signer: Signer
}

pub type Redeem {
  Buy 
  Sell 
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
