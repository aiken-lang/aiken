# Assert

```gleam
assert rawdata = SomeType
```

Causes the script to fail if the rawdata doesn't match the structure of datumtype
Otherwise, returns a value of SomeType

Primarily for validating input datums / redeemers.

You can unpack (1-match) data in the assertion

```gleam
assert Some(x) = Option(Int)
```