# Check

Check is faster than assert but has weaker guarantees.
You can unpack (1-match) data in a check.

```gleam
check Some(x) = Option(Int)
```