# Blocks
Let bindings with blocks

```gleam
let x = 3

let z = {
    let y = 2
    x + y
}
```

A block can be thought of as calling an anonymous function with no arguments. They can be used anywhere a value is.
Since everything is secretly a function, the last statement in any block is implicitly its return.


Where If with blocks

```gleam
let name: Option(String) = someFunction()
let suffix = ""
when name is {
    Some(s)->{
        let combined = s + suffix
        Some(combined)
    }
    None->None
}
```