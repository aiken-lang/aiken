# Untyped Plutus Core

## Usage

For now the command line application can only encode/decode Untyped Plutus Core
to/from it's on chain format. See the roadmap below for a list of planned features and goals.

```sh
# help
aiken help

# compile an untyped plutus core program to flat
aiken uplc flat program.uplc

aiken uplc flat program.uplc --print

# output
00001011 00010110 00100001 01001000
00000101 10000001

aiken uplc flat program.uplc --out=something.flat

# decode an untyped plutus core program from flat
aiken uplc unflat program.flat

aiken uplc unflat program.flat --print

# output
(program
  11.22.33
  (con integer 11)
)

aiken uplc unflat program.flat --out=something.uplc
```
