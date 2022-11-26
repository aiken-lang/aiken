# Functions

Functions can be instantiated like so:
```gleam
fn f(x) {

}
```
Functions in aiken are pure without side effect, so all functions also must return some value.
A function with no return value (like above) will error.

Providing arg types:
```gleam
fn f(x: Int) {

}
```

and return types:

```gleam
fn f(x: Int) -> Int {

}
```

and the last value is implicitly returned:

```gleam
fn f(x: Int) -> Int {
    x + 1
}
```

Functions can be made public in modules so they can be accessed by others

```gleam
pub fn myFunction(a: List(a)) {
    // do something useful...
}
```

A