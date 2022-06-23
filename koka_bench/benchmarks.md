# Benchmarks

## cfold_table.md

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-cfold 5` | 25.6 ± 1.0 | 24.1 | 28.2 | 55.79 ± 44.22 |
| `build/java/out/bench/cfold 5` | 73.8 ± 8.0 | 60.1 | 87.3 | 160.57 ± 128.30 |
| `build/ocaml/ml-cfold 5` | 0.6 ± 0.5 | 0.2 | 3.2 | 1.27 ± 1.43 |
| `build/swift/sw-cfold 5` | 2.0 ± 0.5 | 1.7 | 4.4 | 4.44 ± 3.67 |
| `build/cpp/cpp-cfold 5` | 0.9 ± 0.4 | 0.5 | 3.3 | 1.90 ± 1.76 |
| `build/haskell/hs-cfold 5` | 0.8 ± 0.4 | 0.5 | 3.8 | 1.77 ± 1.65 |
| `build/koka/out/bench/kk-cfold 5` | 0.5 ± 0.4 | 0.2 | 2.8 | 1.00 |



## deriv_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-deriv 8` | 3.228 ± 0.017 | 3.203 | 3.253 | 214.05 ± 14.08 |
| `build/java/out/bench/deriv 8` | 0.116 ± 0.019 | 0.087 | 0.130 | 7.68 ± 1.37 |
| `build/ocaml/ml-deriv 8` | 0.015 ± 0.001 | 0.014 | 0.018 | 1.00 |
| `build/swift/sw-deriv 8` | 0.038 ± 0.001 | 0.036 | 0.040 | 2.51 ± 0.18 |
| `build/cpp/cpp-deriv 8` | 0.021 ± 0.001 | 0.020 | 0.024 | 1.40 ± 0.11 |
| `build/haskell/hs-deriv 8` | 0.036 ± 0.001 | 0.035 | 0.039 | 2.41 ± 0.17 |
| `build/koka/out/bench/kk-deriv 8` | 0.017 ± 0.001 | 0.016 | 0.019 | 1.11 ± 0.08 |



## nqueens_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-nqueens 10` | 2.062 ± 0.010 | 2.052 | 2.083 | 469.37 ± 56.39 |
| `build/java/out/bench/nqueens 10` | 0.058 ± 0.005 | 0.054 | 0.071 | 13.29 ± 1.92 |
| `build/ocaml/ml-nqueens 10` | 0.005 ± 0.001 | 0.004 | 0.007 | 1.06 ± 0.18 |
| `build/swift/sw-nqueens 10` | 0.016 ± 0.001 | 0.015 | 0.018 | 3.63 ± 0.47 |
| `build/cpp/cpp-nqueens 10` | 0.006 ± 0.000 | 0.006 | 0.008 | 1.34 ± 0.19 |
| `build/haskell/hs-nqueens 10` | 0.036 ± 0.001 | 0.035 | 0.037 | 8.09 ± 0.98 |
| `build/koka/out/bench/kk-nqueens 10` | 0.004 ± 0.001 | 0.004 | 0.006 | 1.00 |
| `build/koka/out/bench/kk-nqueens-int 10` | 0.006 ± 0.000 | 0.006 | 0.008 | 1.47 ± 0.21 |



## rbtree_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-rbtree-opt 42000` | 3.469 ± 0.019 | 3.448 | 3.506 | 734.95 ± 106.54 |
| `build/kraken/out/bench/kraken-rbtree 42000` | 3.693 ± 0.008 | 3.680 | 3.706 | 782.55 ± 113.38 |
| `build/java/out/bench/rbtree 42000` | 0.084 ± 0.006 | 0.078 | 0.096 | 17.89 ± 2.90 |
| `build/ocaml/ml-rbtree 42000` | 0.008 ± 0.001 | 0.008 | 0.010 | 1.72 ± 0.27 |
| `build/swift/sw-rbtree 42000` | 0.040 ± 0.001 | 0.039 | 0.041 | 8.42 ± 1.23 |
| `build/cpp/cpp-rbtree 42000` | 0.006 ± 0.000 | 0.005 | 0.007 | 1.22 ± 0.20 |
| `build/haskell/hs-rbtree 42000` | 0.016 ± 0.001 | 0.016 | 0.018 | 3.42 ± 0.51 |
| `build/koka/out/bench/kk-rbtree 42000` | 0.005 ± 0.001 | 0.004 | 0.008 | 1.00 |



