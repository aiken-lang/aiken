# Variables

Aiken has let bindings for variables. A value can be given a name using let.
Names can be reused by later let bindings, but the values contained are immutable.

```aiken
let x = 3
let v = x
let x = 7

x // => 2
y // => 1
```

Function variables with types

```
fn add(a: Int, b: Int) -> Int {
    a + b
}
```

Let bindings

```
fn something() {
    let a = 3
    let b = 5
    a + b
}
```
