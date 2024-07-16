# (No Fib) Benchmarks

This folder contains a set of benchmarks inspired and ported from the [plutus-benchmarks](https://github.com/IntersectMBO/plutus/tree/master/plutus-benchmark/nofib#plutus-nofib-benchmarks), written in Haskell. The idea is to provide benchmarks which can capture more realistic patterns and behaviours than usual "Fibonacci" benchmarks often used for benchmarking applications but falling short in capturing real-world scenarios.

Note that the primary use-case of those benchmarks is to compare Aiken with itself across compiler versions. As optimizations get implemented, it comes as a supplimentary means to assess their impact.

## Summary

Results are summarized below, relatively to the previous version. For brevity, we only report versions for which there's a deviation from a previous version.

<!--
v1.0.29-alpha & v1.0.28-alpha
    ┍━ benchmarks/knights ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 172256715, cpu:  71851995726] bench_knights_100_4x4
    │ PASS [mem: 321712271, cpu: 159767368294] bench_knights_100_6x6
    │ PASS [mem: 601065675, cpu: 319834775948] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed

==== There's no 1.0.27-alpha

v1.0.26-alpha & V1.0.25-alpha
    ┍━ benchmarks/knights ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 171421863, cpu:  72861467671] bench_knights_100_4x4
    │ PASS [mem: 354137347, cpu: 174027736310] bench_knights_100_6x6
    │ PASS [mem: 688090183, cpu: 356296429240] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed

v1.0.24-alpha & V1.0.23-alpha
    ┍━ benchmarks/knights ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 180946463, cpu:  75052125671] bench_knights_100_4x4
    │ PASS [mem: 374910247, cpu: 178805503310] bench_knights_100_6x6
    │ PASS [mem: 729107683, cpu: 365730454240] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed

==== There's no 1.0.22-alpha

V1.0.21-alpha
    ┍━ benchmarks/knights ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 180697463, cpu:  74944457471] bench_knights_100_4x4
    │ PASS [mem: 374661247, cpu: 178697835110] bench_knights_100_6x6
    │ PASS [mem: 728858683, cpu: 365622786040] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed

V1.0.20-alpha, v1.0.19-alpha & v1.0.18-alpha
    ┍━ benchmarks/knights ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    │ PASS [mem: 182244075, cpu:  75300076471] bench_knights_100_4x4
    │ PASS [mem: 380548699, cpu: 180051720110] bench_knights_100_6x6
    │ PASS [mem: 740194031, cpu: 368229509040] bench_knights_100_8x8
    ┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 3 tests | 3 passed | 0 failed
-->

### CPU

| Benchmark         | `v1.0.29`    | vs `v1.0.25` | vs `v1.0.23` | vs `v1.0.21` | vs `v1.0.18` |
| ---               | ---:         | ---:         | ---:         | ---:         | ---:         |
| `knights_100_4x4` | 71851995726  | +1.40%       | +4.45%       | +4.30%       | +4.80%       |
| `knights_100_6x6` | 159767368294 | +8.93%       | +11.92%      | +11.85%      | +12.70%      |
| `knights_100_8x8` | 319834775948 | +11.40%      | +14.35%      | +14.32%      | +15.13%      |

### Mem

| Benchmark         | `v1.0.29` | vs `v1.0.25` | vs `v1.0.23` | vs `v1.0.21` | vs `v1.0.18` |
| ---               | ---:      | ---:         | ---:         | ---:         | ---:         |
| `knights_100_4x4` | 172256715 | -0.48%       | +5.04%       | +4.90%       | +5.80%       |
| `knights_100_6x6` | 321712271 | +10.08%      | +16.54%      | +16.46%      | +18.29%      |
| `knights_100_8x8` | 601065675 | +14.48%      | +21.30%      | +21.26%      | +23.15%      |
