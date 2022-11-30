# Check

`check` uses more budget than assert but has stronger guarantees.

# Assert

```aiken
type Datum {
  n: Int
}

fn do_something(datum: Data) -> Bool {
  check d: Datum = datum

  d.n == 0
}
```

Causes the script to fail if the raw `Data` doesn't match the structure of `Datum`.
