| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `./slow/kraken-rbtree-opt 100` | 24.1 ± 1.0 | 22.2 | 26.7 | 1.88 ± 0.12 |
| `./slow/kraken-rbtree-opt-wavm 100` | 34.7 ± 7.8 | 32.5 | 104.4 | 2.71 ± 0.63 |
| `./slow/newlisp-slow-fexpr-rbtree 100` | 334.7 ± 1.7 | 331.3 | 337.2 | 26.13 ± 1.27 |
| `./slow/newlisp-macro-rbtree 100` | 12.8 ± 0.6 | 12.1 | 15.0 | 1.00 |
| `./slow/kraken-rbtree-slow-wavm 100` | 2501167.5 ± 26231.6 | 2428798.0 | 2514951.4 | 195272.93 ± 9651.02 |
