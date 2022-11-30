# Type aliases

A type alias lets you create a name which is identical to
another type, without any additional information.

```aiken
type MyNumber = Integer
```

They are most useful for simplifying type signatures.

```aiken
type Person = #(String, Integer)

fn create_person(name: String, age: Integer) -> Person {
    #(name, age)
}
```

If you want the type alias to be accessible from a module, you can define it as public.

```
pub type MyVector3 = #(Integer, Integer, Integer)
```
