# Command-line utilities

## Evaluation

Let's consider the following basic program:

<p align="right"><strong>program_1.uplc</strong></p>

```
(program
  2.0.0
  [ [ (builtin addInteger) (con integer 16) ] (con integer 26) ]
)
```

We can evaluate this program using Aiken's cli via:

```console
$ aiken uplc eval program_1.uplc
```

```
Result
------
(con integer 42)

Costs
-----
cpu: 321577
memory: 602

Budget
------
cpu: 9999678423
memory: 13999398
```

The output indicates the result of the evaluation (`42`) as well as the
execution cost of that program, both in terms of CPU and memory usage.

Note that the command also accepts arguments. So, for example, if we modify our
program into a function that accepts an argument as follows:

<p align="right"><strong>program_2.uplc</strong></p>

```
(program
  2.0.0
  (lam x [ [ (builtin addInteger) (con integer 16) ] x ])
)
```

You can then instrument Aiken to provide arguments upon calling the program by
simply appending them to the `eval` command:

```console
$ aiken uplc eval program_2.uplc "(con integer 26)"
```

```
Result
------
(con integer 42)
```

## Formatting

Because writing UPLC by hand can be a tedious task, Aiken provides a quick way to automatically format
a UPLC program via the `fmt` command. By default, the command override the file given as input, but you
can also simply prints the result to stdout using `--print`. For example:

```console
$ aiken uplc fmt program_2.uplc --print
```

```
(program
  2.0.0
  (lam x [ [ (builtin addInteger) (con integer 16) ] x ])
)
```

## Converting to/from bytecode

So far, we've been representing UPLC programs using a high-level syntax. In
practice, however, UPLC programs are compiled into bytecode when submitted
on-chain.

Aiken provides means to convert a high-level UPLC program into a low-level flat
bytecode − and vice-versa, via the `flat` and `unflat` commands. For example:

```console
$ aiken uplc flat program_1.uplc --print
```

```
00000010 00000000 00000000 00110011
01110000 00001001 00000001 00000010
01000000 01101001
```

The `--print` flag instruments the command-line to print everything on stdout
in a readable way. Without the flag, the command creates a file `program_1.flat`
next to `program_1.uplc`.

> **Note**
>
> `aiken uplc flat program.uplc`
>
> and
>
> `aiken uplc flat program.uplc --out program.flat`
>
> are therefore equivalent.

From there, one can recover a UPLC high-level syntax from a flat program using
`unflat` as such:

```console
$ aiken uplc unflat program_1.flat --print
```

```
(program
  2.0.0
  [ [ (builtin addInteger) (con integer 16) ] (con integer 26) ]
)
```
