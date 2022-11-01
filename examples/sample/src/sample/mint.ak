use sample/context





pub type Mint { currency_symbol: ByteArray }

pub type ScriptContext =
  context.ScriptContext(Mint)
