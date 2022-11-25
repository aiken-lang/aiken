# List

Aiken lists are plutus linked lists. Accessing by index is O(n). Appending or accessing head is O(1). Grabbing tail is O(1).

There is no builtin syntax for accessing by index as this is implemented by standard libs.

Accessing head, tail, or preceding elements can be done by pattern matching.

```gleam
// this function checks if a list has a sequence of 1 then 2 contained within it.
fn listStuff(a: List(Int)){
    when a is {
        [1,2, ..tail] -> True
        []->False
        _ -> listStuff([2, ..tail])
    }
}
```

Helper functions for safely accessing head, tail, are provided in standard lib but are implemented using comprehensions.
It is usually best to use your own comprehensions for efficiency (until the optimiser is better).