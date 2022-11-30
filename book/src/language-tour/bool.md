# Bool

A Bool can be either True or False.

Aiken defines a handful of operators that work with Bools.

```aiken
False && False // => False
False && True  // => False
True && False  // => False
True && True   // => True

False || False // => False
False || True  // => True
True || False  // => True
True || True   // => True
```

These are implemented using the plutus `ifThenElse` builtin.

```aiken
a || b // => if a {True} else {b} -- ifThenElse(a, True, b)
a && b // => if a {b} else {False} -- ifThenElse(a, b, False)
```

An if statement decides on a boolean value.

```aiken
fn negate(b: Bool) -> Bool {
    if b {
        False
    } else {
        True
    }
}
```
