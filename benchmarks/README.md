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

==== There's no 1.0.27-alpha

v1.0.26-alpha & V1.0.25-alpha

    ┍━ benchmarks/clausify/benchmark ━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem:  52538257, cpu:  20243486892] bench_clausify_f1
    │ PASS [mem:  65404263, cpu:  25235091975] bench_clausify_f2
    │ PASS [mem: 174866054, cpu:  67523028814] bench_clausify_f3
    │ PASS [mem: 225087758, cpu:  88367835856] bench_clausify_f4
    │ PASS [mem: 851294369, cpu: 328896952793] bench_clausify_f5
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 5 tests | 5 passed | 0 failed

    ┍━ benchmarks/knights ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 171421863, cpu:  72861467671] bench_knights_100_4x4
    │ PASS [mem: 354137347, cpu: 174027736310] bench_knights_100_6x6
    │ PASS [mem: 688090183, cpu: 356296429240] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed

v1.0.24-alpha & V1.0.23-alpha

    ┍━ benchmarks/clausify/benchmark ━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem:   64738801, cpu:  24123180244] bench_clausify_f1
    │ PASS [mem:   80280627, cpu:  29901360387] bench_clausify_f2
    │ PASS [mem:  214423277, cpu:  79840016473] bench_clausify_f3
    │ PASS [mem:  269232481, cpu: 101739948515] bench_clausify_f4
    │ PASS [mem: 1045036759, cpu: 389233562263] bench_clausify_f5
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 5 tests | 5 passed | 0 failed

    ┍━ benchmarks/knights ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 180946463, cpu:  75052125671] bench_knights_100_4x4
    │ PASS [mem: 374910247, cpu: 178805503310] bench_knights_100_6x6
    │ PASS [mem: 729107683, cpu: 365730454240] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed

==== There's no 1.0.22-alpha

V1.0.21-alpha

    ┍━ benchmarks/clausify/benchmark ━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem:   64738801, cpu:  24123180244] bench_clausify_f1
    │ PASS [mem:   80280627, cpu:  29901360387] bench_clausify_f2
    │ PASS [mem:  214423277, cpu:  79840016473] bench_clausify_f3
    │ PASS [mem:  269232481, cpu: 101739948515] bench_clausify_f4
    │ PASS [mem: 1045036759, cpu: 389233562263] bench_clausify_f5
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 5 tests | 5 passed | 0 failed

    ┍━ benchmarks/knights ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 180697463, cpu:  74944457471] bench_knights_100_4x4
    │ PASS [mem: 374661247, cpu: 178697835110] bench_knights_100_6x6
    │ PASS [mem: 728858683, cpu: 365622786040] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed

V1.0.20-alpha, v1.0.19-alpha & v1.0.18-alpha

    ┍━ benchmarks/clausify/benchmark ━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem:   64861501, cpu:  24151401244] bench_clausify_f1
    │ PASS [mem:   80442927, cpu:  29938689387] bench_clausify_f2
    │ PASS [mem:  214833977, cpu:  79934477473] bench_clausify_f3
    │ PASS [mem:  269959981, cpu: 101907273515] bench_clausify_f4
    │ PASS [mem: 1046684059, cpu: 389612441263] bench_clausify_f5
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 5 tests | 5 passed | 0 failed

    ┍━ benchmarks/knights ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 182244075, cpu:  75300076471] bench_knights_100_4x4
    │ PASS [mem: 380548699, cpu: 180051720110] bench_knights_100_6x6
    │ PASS [mem: 740194031, cpu: 368229509040] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed
-->

### CPU

| Benchmark         | `v1.1.0`     | vs `v1.0.29` | vs `v1.0.25` | vs `v1.0.23` | vs `v1.0.21` |
| ---               | ---:         | ---:         | ---:         | ---:         | ---:         |
| `clausify_f1`     | 16198154564  | -24.99%      | -6.26%       | +11.71%      | +11.71%      |
| `clausify_f2`     | 20169891270  | -24.92%      | -6.07%       | +11.30%      | +11.30%      |
| `clausify_f3`     | 53923018831  | -24.91%      | -5.98%       | +11.17%      | +11.17%      |
| `clausify_f4`     | 70014384566  | -24.74%      | -5.01%       | +9.37%       | +9.37%       |
| `clausify_f5`     | 262421671684 | -25.00%      | -6.00%       | +11.24%      | +11.24%      |
| `knights_100_4x4` | 57037226471  | -20.62%      | +1.40%       | +4.45%       | +4.30%       |
| `knights_100_6x6` | 137399466410 | -14.00%      | +8.93%       | +11.92%      | +11.85%      |
| `knights_100_8x8` | 281418742606 | -12.00%      | +11.40%      | +14.35%      | +14.32%      |

### Mem

| Benchmark         | `v1.1.0`  | vs `v1.0.29` | vs `v1.0.25` | vs `v1.0.23` | vs `v1.0.21` |
| ---               | ---:      | ---:         | ---:         | ---:         | ---:         |
| `clausify_f1`     | 53769377  | ± 0.00%      | -2.29%       | +20.40%      | +20.40%      |
| `clausify_f2`     | 67108683  | ± 0.00%      | -2.54%       | +19.63%      | +19.63%      |
| `clausify_f3`     | 179606857 | ± 0.00%      | -2.64%       | +19.38%      | +19.38%      |
| `clausify_f4`     | 231444137 | ± 0.00%      | -2.75%       | +16.33%      | +16.33%      |
| `clausify_f5`     | 874286879 | ± 0.00%      | -2.63%       | +19.53%      | +19.53%      |
| `knights_100_4x4` | 172246681 | -0.01%       | -0.48%       | +5.04%       | +4.90%       |
| `knights_100_6x6` | 321690197 | -0.01%       | +10.08%      | +16.54%      | +16.46%      |
| `knights_100_8x8` | 601026745 | -0.01%       | +14.48%      | +21.30%      | +21.26%      |
