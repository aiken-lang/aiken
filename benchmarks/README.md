# (No Fib) Benchmarks

This folder contains a set of benchmarks inspired and ported from the [plutus-benchmarks](https://github.com/IntersectMBO/plutus/tree/master/plutus-benchmark/nofib#plutus-nofib-benchmarks), written in Haskell. The idea is to provide benchmarks which can capture more realistic patterns and behaviours than usual "Fibonacci" benchmarks often used for benchmarking applications but falling short in capturing real-world scenarios.

Note that the primary use-case of those benchmarks is to compare Aiken with itself across compiler versions. As optimizations get implemented, it comes as a supplimentary means to assess their impact.

## Summary

Results are summarized below, relatively to the previous version. For brevity, we only report versions for which there's a deviation from a previous version.

<!--
Plutus-Tx

Script                     Size     CPU budget      Memory budget
-----------------------------------------------------------------
clausify/F1                1573      3316814452        19803348
clausify/F2                1573      4329805760        25708376
clausify/F3                1573     11847889335        70086982
clausify/F4                1573     26924400260       152296076
clausify/F5                1573     57792275160       340971480
knights/4x4                2049     16660243968        86107706
knights/6x6                2049     49595956778       271079788
knights/8x8                2049     89757683708       495426680
primes/05digits            1501      9617902676        37194205
primes/08digits            1501     15888515320        60723255
primes/10digits            1501     18980946392        71916641
primes/20digits            1501     36493682732       137260387
primes/30digits            1501     57224069574       214186802
primes/40digits            1501     75870264649       282727215
primes/50digits            1501     93666987132       345920797
queens4x4/bt               1867      5135006267        28184130
queens4x4/bm               1867      6345082859        35502236
queens4x4/bjbt1            1867      6252769293        34895616
queens4x4/bjbt2            1867      5820721293        32195316
queens4x4/fc               1867     13740412571        78668768
queens5x5/bt               1867     71081240426       381100320
queens5x5/bm               1867     71574838831       400366576
queens5x5/bjbt1            1867     82536005114       449984088
queens5x5/bjbt2            1867     79887717114       433432288
queens5x5/fc               1867    179227518621      1023295666
-->

<!--
v1.1.13
    ┍━ benchmarks/clausify/benchmark ━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem:  39891097, cpu:  12325496028] bench_clausify_f1
    │ PASS [mem:  50524767, cpu:  15570882882] bench_clausify_f2
    │ PASS [mem: 136054751, cpu:  41872495549] bench_clausify_f3
    │ PASS [mem: 181055087, cpu:  56754761923] bench_clausify_f4
    │ PASS [mem: 660668247, cpu: 203182153626] bench_clausify_f5
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 5 tests | 5 passed | 0 failed


    ┍━ benchmarks/knights/benchmark ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 160204421, cpu:  54958831939] bench_knights_100_4x4
    │ PASS [mem: 292216349, cpu: 131954064320] bench_knights_100_6x6
    │ PASS [mem: 540217437, cpu: 270266226527] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed

v1.1.0

    ┍━ benchmarks/clausify/benchmark ━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem:  53769377, cpu:  16198154564] bench_clausify_f1
    │ PASS [mem:  67108683, cpu:  20169891270] bench_clausify_f2
    │ PASS [mem: 179606857, cpu:  53923018831] bench_clausify_f3
    │ PASS [mem: 231444137, cpu:  70014384566] bench_clausify_f4
    │ PASS [mem: 874286879, cpu: 262421671684] bench_clausify_f5
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 5 tests | 5 passed | 0 failed

    ┍━ benchmarks/knights/benchmark ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 172246681, cpu:  57037226471] bench_knights_100_4x4
    │ PASS [mem: 321690197, cpu: 137399466410] bench_knights_100_6x6
    │ PASS [mem: 601026745, cpu: 281418742606] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed


v1.0.29-alpha & v1.0.28-alpha

    ┍━ benchmarks/clausify/benchmark ━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem:  53769377, cpu:  21594809455] bench_clausify_f1
    │ PASS [mem:  67108683, cpu:  26864755594] bench_clausify_f2
    │ PASS [mem: 179606857, cpu:  71814854199] bench_clausify_f3
    │ PASS [mem: 231444137, cpu:  93024749730] bench_clausify_f4
    │ PASS [mem: 874286879, cpu: 349894049008] bench_clausify_f5
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 5 tests | 5 passed | 0 failed

    ┍━ benchmarks/knights ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 172256715, cpu:  71851995726] bench_knights_100_4x4
    │ PASS [mem: 321712271, cpu: 159767368294] bench_knights_100_6x6
    │ PASS [mem: 601065675, cpu: 319834775948] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed

-->

> [!NOTE]
>
> Comparing version: `v1.1.13`

### CPU

| Benchmark         | vs `v1.1.0` | vs `v1.0.29` |
| ---               | ---:        | ---:         |
| `clausify_f1`     | -23.91%     | -42.92%      |
| `clausify_f2`     | -22.80%     | -42.04%      |
| `clausify_f3`     | -22.35%     | -41.69%      |
| `clausify_f4`     | -18.94%     | -38.99%      |
| `clausify_f5`     | -22.57%     | -41.93%      |
| `knights_100_4x4` | -3.64%      | -7.00%       |
| `knights_100_6x6` | -3.96%      | -17.41%      |
| `knights_100_8x8` | -3.96%      | -15.50%      |

### Mem

| Benchmark         | vs `v1.1.0` | vs `v1.0.29` |
| ---               | ---:        | ---:         |
| `clausify_f1`     | -25.81%     | -25.81%      |
| `clausify_f2`     | -24.71%     | -24.71%      |
| `clausify_f3`     | -24.25%     | -24.25%      |
| `clausify_f4`     | -21.78%     | -21.78%      |
| `clausify_f5`     | -24.43%     | -24.43%      |
| `knights_100_4x4` | -6.99%      | -7.00%       |
| `knights_100_6x6` | -9.16%      | -9.17%       |
| `knights_100_8x8` | -10.12%     | -10.12%      |
