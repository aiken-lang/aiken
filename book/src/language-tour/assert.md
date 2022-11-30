# Assert

```aiken
type Datum {
  n: Int
}

fn do_something(datum: Data) -> Bool {
  assert d: Datum = datum

  d.n == 0
}
```

Causes the script to fail if the raw `Data` doesn't match the structure of `Datum`.

Primarily for validating input datums / redeemers.

You can also assert patterns.

```aiken
let optional_int = Some(1)

assert Some(x) = optional_int
```
