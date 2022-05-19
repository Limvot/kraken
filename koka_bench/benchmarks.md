#Benchmarks

## cfold_table.md

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-cfold 5` | 24.4 ± 0.8 | 22.9 | 26.8 | 56.36 ± 42.56 |
| `build/java/out/bench/cfold 5` | 73.5 ± 8.4 | 57.5 | 85.0 | 170.13 ± 129.81 |
| `build/ocaml/ml-cfold 5` | 0.4 ± 0.2 | 0.2 | 2.2 | 1.03 ± 0.96 |
| `build/swift/sw-cfold 5` | 2.1 ± 0.5 | 1.6 | 4.5 | 4.86 ± 3.82 |
| `build/cpp/cpp-cfold 5` | 0.8 ± 0.4 | 0.5 | 2.8 | 1.95 ± 1.72 |
| `build/haskell/hs-cfold 5` | 0.7 ± 0.4 | 0.5 | 2.7 | 1.67 ± 1.54 |
| `build/koka/out/bench/kk-cfold 5` | 0.4 ± 0.3 | 0.2 | 2.2 | 1.00 |



## deriv_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-deriv 8` | 3.559 ± 0.014 | 3.536 | 3.582 | 236.58 ± 13.05 |
| `build/java/out/bench/deriv 8` | 0.112 ± 0.020 | 0.085 | 0.131 | 7.45 ± 1.41 |
| `build/ocaml/ml-deriv 8` | 0.015 ± 0.001 | 0.014 | 0.018 | 1.00 |
| `build/swift/sw-deriv 8` | 0.037 ± 0.000 | 0.036 | 0.038 | 2.44 ± 0.14 |
| `build/cpp/cpp-deriv 8` | 0.020 ± 0.000 | 0.020 | 0.022 | 1.35 ± 0.08 |
| `build/haskell/hs-deriv 8` | 0.035 ± 0.001 | 0.035 | 0.038 | 2.36 ± 0.14 |
| `build/koka/out/bench/kk-deriv 8` | 0.017 ± 0.000 | 0.016 | 0.018 | 1.10 ± 0.07 |



## rbnqueens_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-nqueens 10` | 2.225 ± 0.016 | 2.187 | 2.246 | 480.72 ± 59.09 |
| `build/java/out/bench/nqueens 10` | 0.057 ± 0.005 | 0.053 | 0.070 | 12.37 ± 1.92 |
| `build/ocaml/ml-nqueens 10` | 0.005 ± 0.001 | 0.004 | 0.007 | 1.03 ± 0.17 |
| `build/swift/sw-nqueens 10` | 0.016 ± 0.001 | 0.015 | 0.018 | 3.48 ± 0.45 |
| `build/cpp/cpp-nqueens 10` | 0.006 ± 0.001 | 0.006 | 0.009 | 1.34 ± 0.20 |
| `build/haskell/hs-nqueens 10` | 0.036 ± 0.001 | 0.035 | 0.038 | 7.77 ± 0.97 |
| `build/koka/out/bench/kk-nqueens 10` | 0.005 ± 0.001 | 0.004 | 0.007 | 1.00 |
| `build/koka/out/bench/kk-nqueens-int 10` | 0.006 ± 0.000 | 0.006 | 0.008 | 1.39 ± 0.19 |



## rbtree_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-rbtree-opt 42000` | 3.924 ± 0.196 | 3.786 | 4.307 | 863.93 ± 83.96 |
| `build/kraken/out/bench/kraken-rbtree 42000` | 4.116 ± 0.105 | 4.070 | 4.415 | 906.34 ± 79.00 |
| `build/java/out/bench/rbtree 42000` | 0.086 ± 0.010 | 0.075 | 0.130 | 18.96 ± 2.79 |
| `build/ocaml/ml-rbtree 42000` | 0.008 ± 0.000 | 0.008 | 0.010 | 1.80 ± 0.18 |
| `build/swift/sw-rbtree 42000` | 0.039 ± 0.000 | 0.039 | 0.041 | 8.70 ± 0.73 |
| `build/cpp/cpp-rbtree 42000` | 0.005 ± 0.000 | 0.005 | 0.008 | 1.21 ± 0.14 |
| `build/haskell/hs-rbtree 42000` | 0.016 ± 0.000 | 0.016 | 0.018 | 3.53 ± 0.31 |
| `build/koka/out/bench/kk-rbtree 42000` | 0.005 ± 0.000 | 0.004 | 0.006 | 1.00 |



