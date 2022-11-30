# ByteArray

ByteArrays are exactly what they seem, an array of bytes. In plutus
they are called ByteStrings but we decided ByteArray is a more accurate name.

Aiken supports ByteArray literals.

```aiken
#[10, 255]
#[1, 256] // => results in a parse error because 256 is bigger than 1 byte
```

It's important to mention that variables and patterns are not supported in
ByteArray literals. This is due to how ByteString literals work under the
hood in [UPLC](../uplc.md).

```aiken
let x = 10
#[x, 243] // => not allowed
```
