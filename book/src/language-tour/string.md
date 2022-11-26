# String

There are Strings and ByteArrays (plutus bytestrings)

The default representation using double quotes is a string.
```
let mystring = "Hello World!"
```
Strings may be appended and compared.

For char operations, ByteArrays must be used.
ByteArrays have efficient indexing, slicing, and can be compared or concatenated.
ByteArrays can also be useful for non-text data (hence why we call them arrays not strings.)

Due to their fixed byte width, you may find them not suitable for complex structures.