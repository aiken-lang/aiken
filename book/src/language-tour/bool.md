# Bool

Bools are True or False

There are logical conjunctions (True && True) or disjunctions (True || False).

```gleam
fn negate(b: Bool)->Bool {
    if b {
        False
    }else{
        True
    }
}

fn and(b: Bool, c: Bool, d: Bool)->Bool{
    b && c && d
}
```