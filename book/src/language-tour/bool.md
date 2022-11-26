# Bool

Bools (short for booleans) are True or False. They correspond to the plutus bool primitive type.
There are logical disjunctions (True || False) or conjunctions (True && True).
```gleam
False || False -- -> False
True || False -- -> True
False || True -- -> True
True || True -- -> True

False && False -- -> False
True && False -- -> False
False && True -- -> False
True && True -- -> True
```

These are implemented using the plutus ifThenElse primitive.
```gleam
a || b -- if a {True} else {b} -- ifThenElse(a, True, b)
a && b -- if a {b} else {False} -- ifThenElse(a, b, False)
```

An if statement decides on a boolean value.
```gleam
fn negate(b: Bool) -> Bool {
    if b {
        False
    }else{
        True
    }
}
```

The && operator in a function
```gleam
fn and(b: Bool, c: Bool, d: Bool) -> Bool{
    b && c && d
}
```