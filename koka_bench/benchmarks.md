# Benchmarks

## cfold_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-cfold-wavm 20` | 1.262 ± 0.009 | 1.255 | 1.285 | 9.52 ± 0.21 |
| `build/java/out/bench/cfold 20` | 0.357 ± 0.006 | 0.346 | 0.364 | 2.69 ± 0.07 |
| `build/ocaml/ml-cfold 20` | 0.539 ± 0.002 | 0.536 | 0.540 | 4.06 ± 0.09 |
| `build/swift/sw-cfold 20` | 0.864 ± 0.004 | 0.855 | 0.871 | 6.52 ± 0.14 |
| `build/cpp/cpp-cfold 20` | 0.306 ± 0.002 | 0.303 | 0.311 | 2.31 ± 0.05 |
| `build/haskell/hs-cfold 20` | 0.504 ± 0.001 | 0.502 | 0.505 | 3.80 ± 0.08 |
| `build/koka/out/bench/kk-cfold 20` | 0.133 ± 0.003 | 0.129 | 0.139 | 1.00 |



## deriv_table.md

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-deriv 8` | 592.7 ± 15.7 | 579.7 | 622.1 | 41.04 ± 2.42 |
| `build/kraken/out/bench/kraken-deriv-wavm 8` | 407.3 ± 1.0 | 405.6 | 408.8 | 28.21 ± 1.49 |
| `build/java/out/bench/deriv 8` | 120.7 ± 17.4 | 86.7 | 130.2 | 8.36 ± 1.28 |
| `build/ocaml/ml-deriv 8` | 14.4 ± 0.8 | 13.1 | 16.5 | 1.00 |
| `build/swift/sw-deriv 8` | 37.3 ± 0.7 | 35.6 | 39.6 | 2.58 ± 0.15 |
| `build/cpp/cpp-deriv 8` | 20.3 ± 0.7 | 19.4 | 22.2 | 1.41 ± 0.09 |
| `build/haskell/hs-deriv 8` | 35.4 ± 0.8 | 34.1 | 37.1 | 2.45 ± 0.14 |
| `build/koka/out/bench/kk-deriv 8` | 16.6 ± 0.8 | 15.7 | 18.7 | 1.15 ± 0.08 |



## fib_table.md

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-fib-let-wavm 30` | 31.8 ± 1.1 | 30.4 | 38.0 | 12.68 ± 3.23 |
| `build/kraken/out/bench/kraken-fib-wavm 30` | 30.9 ± 0.9 | 29.3 | 34.4 | 12.32 ± 3.13 |
| `build/kraken/out/bench/kraken-fib 30` | 38.9 ± 1.1 | 37.0 | 40.9 | 15.51 ± 3.93 |
| `build/kraken/out/bench/kraken-fib-let 30` | 42.8 ± 0.8 | 40.6 | 44.9 | 17.07 ± 4.32 |
| `build/cpp/cpp-fib 30` | 2.5 ± 0.6 | 2.1 | 5.2 | 1.00 |
| `build/picolisp/out/bench/picolisp-fib-let 30` | 107.8 ± 0.9 | 105.7 | 109.3 | 42.94 ± 10.83 |
| `build/picolisp/out/bench/picolisp-fib 30` | 88.7 ± 1.2 | 86.7 | 91.3 | 35.33 ± 8.92 |
| `build/koka/out/bench/kk-fib 30` | 5.9 ± 0.5 | 5.4 | 8.1 | 2.34 ± 0.62 |
| `build/python/out/bench/python-fib-let 30` | 295.5 ± 11.2 | 278.8 | 306.8 | 117.74 ± 30.03 |
| `build/python/out/bench/python-fib 30` | 284.4 ± 9.8 | 266.3 | 295.7 | 113.32 ± 28.84 |
| `build/scheme/out/bench/scheme-fib 30` | 52.1 ± 1.1 | 50.6 | 55.3 | 20.76 ± 5.25 |
| `build/scheme/out/bench/scheme-fib-let 30` | 53.0 ± 0.9 | 51.4 | 55.2 | 21.11 ± 5.34 |



## nqueens_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-nqueens 10` | 1.196 ± 0.013 | 1.185 | 1.230 | 272.10 ± 37.86 |
| `build/kraken/out/bench/kraken-nqueens-wavm 10` | 0.863 ± 0.010 | 0.850 | 0.880 | 196.39 ± 27.33 |
| `build/java/out/bench/nqueens 10` | 0.058 ± 0.005 | 0.053 | 0.071 | 13.28 ± 2.20 |
| `build/ocaml/ml-nqueens 10` | 0.005 ± 0.001 | 0.004 | 0.007 | 1.05 ± 0.20 |
| `build/swift/sw-nqueens 10` | 0.016 ± 0.001 | 0.015 | 0.018 | 3.61 ± 0.53 |
| `build/cpp/cpp-nqueens 10` | 0.006 ± 0.000 | 0.005 | 0.008 | 1.34 ± 0.22 |
| `build/haskell/hs-nqueens 10` | 0.036 ± 0.001 | 0.035 | 0.037 | 8.18 ± 1.15 |
| `build/koka/out/bench/kk-nqueens 10` | 0.004 ± 0.001 | 0.004 | 0.007 | 1.00 |
| `build/koka/out/bench/kk-nqueens-int 10` | 0.006 ± 0.001 | 0.006 | 0.008 | 1.47 ± 0.24 |



## rbtree_table.md

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `build/kraken/out/bench/kraken-rbtree-opt 420000` | 5.319 ± 0.335 | 5.120 | 6.119 | 110.82 ± 7.71 |
| `build/kraken/out/bench/kraken-rbtree-wavm 420000` | 7.603 ± 0.085 | 7.545 | 7.826 | 158.41 ± 5.00 |
| `build/kraken/out/bench/kraken-rbtree-opt-wavm 420000` | 2.388 ± 0.024 | 2.370 | 2.427 | 49.75 ± 1.55 |
| `build/kraken/out/bench/kraken-rbtree 420000` | 14.104 ± 0.672 | 13.574 | 15.735 | 293.85 ± 16.47 |
| `build/java/out/bench/rbtree 420000` | 0.205 ± 0.005 | 0.201 | 0.215 | 4.26 ± 0.17 |
| `build/ocaml/ml-rbtree 420000` | 0.084 ± 0.000 | 0.083 | 0.086 | 1.75 ± 0.05 |
| `build/swift/sw-rbtree 420000` | 0.481 ± 0.002 | 0.478 | 0.487 | 10.03 ± 0.30 |
| `build/cpp/cpp-rbtree 420000` | 0.062 ± 0.003 | 0.058 | 0.070 | 1.28 ± 0.07 |
| `build/haskell/hs-rbtree 420000` | 0.165 ± 0.001 | 0.164 | 0.166 | 3.44 ± 0.10 |
| `build/koka/out/bench/kk-rbtree 420000` | 0.048 ± 0.001 | 0.045 | 0.052 | 1.00 |



## slow_fib_table.md

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `slow/newlisp-fib-let 30` | 352.8 ± 3.6 | 345.0 | 356.9 | 11.44 ± 0.38 |
| `slow/kraken-fib-let-slow-wavm 30` | 8754.9 ± 79.1 | 8635.3 | 8940.4 | 283.97 ± 9.38 |
| `slow/kraken-fib-slow-wavm 30` | 3154.1 ± 31.8 | 3112.0 | 3204.9 | 102.31 ± 3.41 |
| `slow/kraken-fib-let-wavm 30` | 31.8 ± 0.8 | 30.1 | 34.5 | 1.03 ± 0.04 |
| `slow/newlisp-fib 30` | 308.3 ± 3.1 | 304.0 | 313.0 | 10.00 ± 0.33 |
| `slow/kraken-fib-wavm 30` | 30.8 ± 1.0 | 29.6 | 33.8 | 1.00 |
| `slow/kraken-fib 30` | 38.7 ± 0.9 | 37.2 | 41.1 | 1.25 ± 0.05 |
| `slow/kraken-fib-let 30` | 42.7 ± 1.0 | 40.8 | 45.5 | 1.38 ± 0.05 |



## slow_ish_rbtree_table.md

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `./slow/kraken-rbtree-opt 890` | 28.9 ± 1.1 | 26.9 | 32.8 | 1.00 |
| `./slow/kraken-rbtree-opt-wavm 890` | 35.9 ± 0.7 | 34.7 | 37.8 | 1.24 ± 0.05 |
| `./slow/newlisp-slow-fexpr-rbtree 890` | 6516.8 ± 15.4 | 6491.3 | 6537.4 | 225.82 ± 8.46 |
| `./slow/newlisp-macro-rbtree 890` | 1003.7 ± 2.4 | 1000.1 | 1008.9 | 34.78 ± 1.30 |



## slow_rbtree_table.md

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `./slow/kraken-rbtree-opt 100` | 24.1 ± 1.0 | 22.2 | 26.7 | 1.88 ± 0.12 |
| `./slow/kraken-rbtree-opt-wavm 100` | 34.7 ± 7.8 | 32.5 | 104.4 | 2.71 ± 0.63 |
| `./slow/newlisp-slow-fexpr-rbtree 100` | 334.7 ± 1.7 | 331.3 | 337.2 | 26.13 ± 1.27 |
| `./slow/newlisp-macro-rbtree 100` | 12.8 ± 0.6 | 12.1 | 15.0 | 1.00 |
| `./slow/kraken-rbtree-slow-wavm 100` | 2501167.5 ± 26231.6 | 2428798.0 | 2514951.4 | 195272.93 ± 9651.02 |



