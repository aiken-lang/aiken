# Type aliases

A type alias lets you create a name which is identical to another type, without any additional information.
We like type names (including type alias names) to be PascalCase.

```gleam
type MyNumber = Integer
```

I imagine them like variables for types. You could use this to simplify your type signatures for tuples.

```gleam
type Person = (String, Integer)

fn createPerson(name: String, age: Integer) -> Person {
    (name, age)
}
```

If you want the type-alias to be accessible as a module, you should pub it.

```
pub type MyVector3 = (Integer, Integer, Integer)
```