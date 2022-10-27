pub type ScriptContext {
    thing: String
}

pub type Datum {
  something: String,
}

pub fn thing(a: Datum, b){
    a.something == b
}
