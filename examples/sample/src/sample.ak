pub type ScriptContext {
    thing: ByteArray
}

pub type Datum {
    something: ScriptContext, 
    fin: Int  
}

pub fn eqInt(a: Int, b: Int) {
    a == b
}

pub fn eqString(a: ByteArray, b: ByteArray) {
    a == b
}

