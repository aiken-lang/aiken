# Data

`Data` is the equivalent to `BuiltinData` in plutus. In Aiken,
it's a way to generically represent [custom types](./custom-types.md). In a way, it's
almost like `any` in TypeScript but restricted to user-defined [custom types](./custom-types.md).
As a type it won't match Int, Bool, ByteArray, and String.

It's useful for interacting with builtins that operate on `PlutusData'.

```aiken
type Datum {
  count: Int,
}

let datum = Datum { count: 1 }

// fn(Data) -> ByteArray
builtin.serialize_data(datum) // some bytearray
```

It's also possible to cast `Data` to a [custom type](./custom-types.md).

```aiken
pub fn do_cast(datum: Data) -> Datum {
    let d: Datum = datum

    d
}
```
