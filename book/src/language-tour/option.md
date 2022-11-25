# Option

We define option as a generic type like so

```
pub type Option(a) {
    None
    Some(a)
}
```

Then, functions which fail may safely return an optional value.

```
pub fn getHead(a: List(a))->a {
    when a is {
        [a, .._]->Some(a)
        []->None
    }
}
```

An unsafe variant of this function might instead assert that there is a head, and return it.
