pub type ScriptContext {
    thing: String
}

pub type Datum {
  Datum2{
    something: String,
    fin: Int
  }
  None{}
}

pub fn eqInt(a: Int, b: Int) {
    a == b
}

pub fn eqString(a: String, b: String) {
    a == b
}
