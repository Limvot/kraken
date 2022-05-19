# Benchmarks

## cfold_table.md

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-cfold 5` | 24.5 ± 0.7 | 22.9 | 26.7 | 49.60 ± 32.45 |
| `build/java/out/bench/cfold 5` | 73.3 ± 7.0 | 59.0 | 84.9 | 148.58 ± 98.14 |
| `build/ocaml/ml-cfold 5` | 0.5 ± 0.3 | 0.2 | 2.2 | 1.00 |
| `build/swift/sw-cfold 5` | 2.2 ± 0.5 | 1.7 | 3.7 | 4.40 ± 3.04 |
| `build/cpp/cpp-cfold 5` | 0.9 ± 0.4 | 0.5 | 2.9 | 1.79 ± 1.46 |
| `build/haskell/hs-cfold 5` | 0.8 ± 0.4 | 0.5 | 2.6 | 1.53 ± 1.23 |
| `build/koka/out/bench/kk-cfold 5` | 0.5 ± 0.4 | 0.2 | 2.2 | 1.07 ± 1.06 |



## deriv_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-deriv 8` | 3.544 ± 0.007 | 3.537 | 3.559 | 243.49 ± 8.16 |
| `build/java/out/bench/deriv 8` | 0.117 ± 0.021 | 0.084 | 0.132 | 8.07 ± 1.44 |
| `build/ocaml/ml-deriv 8` | 0.015 ± 0.000 | 0.014 | 0.017 | 1.00 |
| `build/swift/sw-deriv 8` | 0.037 ± 0.001 | 0.036 | 0.039 | 2.54 ± 0.09 |
| `build/cpp/cpp-deriv 8` | 0.020 ± 0.001 | 0.020 | 0.024 | 1.41 ± 0.06 |
| `build/haskell/hs-deriv 8` | 0.036 ± 0.001 | 0.035 | 0.037 | 2.45 ± 0.09 |
| `build/koka/out/bench/kk-deriv 8` | 0.017 ± 0.001 | 0.016 | 0.019 | 1.16 ± 0.06 |



## rbnqueens_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-nqueens 10` | 2.231 ± 0.020 | 2.196 | 2.262 | 519.42 ± 46.07 |
| `build/java/out/bench/nqueens 10` | 0.058 ± 0.006 | 0.053 | 0.068 | 13.51 ± 1.78 |
| `build/ocaml/ml-nqueens 10` | 0.005 ± 0.000 | 0.004 | 0.007 | 1.07 ± 0.15 |
| `build/swift/sw-nqueens 10` | 0.016 ± 0.001 | 0.015 | 0.018 | 3.76 ± 0.36 |
| `build/cpp/cpp-nqueens 10` | 0.006 ± 0.001 | 0.006 | 0.008 | 1.43 ± 0.18 |
| `build/haskell/hs-nqueens 10` | 0.035 ± 0.001 | 0.035 | 0.037 | 8.26 ± 0.74 |
| `build/koka/out/bench/kk-nqueens 10` | 0.004 ± 0.000 | 0.004 | 0.006 | 1.00 |
| `build/koka/out/bench/kk-nqueens-int 10` | 0.007 ± 0.001 | 0.006 | 0.008 | 1.55 ± 0.19 |



## rbtree_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-rbtree-opt 42000` | 3.806 ± 0.016 | 3.784 | 3.826 | 847.67 ± 62.79 |
| `build/kraken/out/bench/kraken-rbtree 42000` | 4.092 ± 0.010 | 4.071 | 4.107 | 911.31 ± 67.43 |
| `build/java/out/bench/rbtree 42000` | 0.089 ± 0.017 | 0.076 | 0.131 | 19.78 ± 4.04 |
| `build/ocaml/ml-rbtree 42000` | 0.008 ± 0.000 | 0.008 | 0.010 | 1.81 ± 0.15 |
| `build/swift/sw-rbtree 42000` | 0.040 ± 0.000 | 0.039 | 0.041 | 8.80 ± 0.66 |
| `build/cpp/cpp-rbtree 42000` | 0.005 ± 0.000 | 0.005 | 0.007 | 1.20 ± 0.12 |
| `build/haskell/hs-rbtree 42000` | 0.016 ± 0.000 | 0.016 | 0.018 | 3.55 ± 0.27 |
| `build/koka/out/bench/kk-rbtree 42000` | 0.004 ± 0.000 | 0.004 | 0.007 | 1.00 |



