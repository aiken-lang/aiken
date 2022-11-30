# List

Lists are ordered collections of values. They're one of the most common data structures in Aiken.

All the elements of a List must be of the same type. Attempting to make a list using multiple
different types will result in a type error.

```aiken
[1, 2, 3, 4]  // List(Int)

["text", 3, 4]  // Type error!
```

Inserting at the front of a list is very fast, and is the preferred way to add new values.

```aiken
[1, ..[2, 3]] // => [1, 2, 3]
```

Note that all data structures in Aiken are immutable so prepending to a list does not change the original list. Instead it efficiently creates a new list with the new additional element.

```
let x = [2, 3]
let y = [1, ..x]

x // => [2, 3]
y // => [1, 2, 3]
```

Accessing head, tail, or preceding elements can be done by pattern matching.

```aiken
// this function checks if a list has a sequence of 1 then 2 contained within it.
fn list_stuff(a: List(Int)){
    when a is {
        [1, 2, ..tail] -> True
        [] -> False
        _ -> list_stuff([2, ..tail])
    }
}
```
