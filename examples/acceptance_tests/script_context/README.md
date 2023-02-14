# Script Context Tests

This project contains a few handcrafted validators and transactions whose sole
purpose is to test the interpretation of the `ScriptContext` from within an
Aiken's validators.

So validators are meant to work hand-in-hand with an associated context.
Because we can't have fully static context (since they contain the validator
and its hash), we define _templates_.

Everything is a bit clunky, but steps have been captured in a `test.sh` script
for convenience.

## How to use

```
./test.sh [VALIDATOR_TITLE]
```
