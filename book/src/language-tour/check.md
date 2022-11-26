# Check

Check is slower than assert but has stronger guarantees.
You can unpack (1-match) data in a check.

```gleam
check Some(x) = Option(Int)
```