| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `echo 30 \| wasmtime ./fib_compiled.wasm` | 93.1 ± 0.7 | 91.6 | 94.7 | 7.28 ± 0.34 |
| `echo 30 \| wasmtime ./fib_compiled_let.wasm` | 119.1 ± 0.7 | 118.2 | 120.4 | 9.31 ± 0.43 |
| `echo 30 \| wasmtime ./builtin_fib.wasm` | 12.8 ± 0.6 | 11.4 | 14.7 | 1.00 |
| `echo 30 \| wasmtime ./fib_compiled_manual.wasm` | 257.4 ± 5.8 | 245.4 | 262.8 | 20.12 ± 1.03 |
| `scheme --script ./fib.scm 30` | 54.7 ± 1.2 | 52.7 | 57.9 | 4.27 ± 0.22 |
| `scheme --script ./fib_let.scm 30` | 54.5 ± 0.9 | 52.7 | 56.1 | 4.26 ± 0.21 |
| `python3 ./fib.py 30` | 283.5 ± 4.1 | 280.7 | 294.8 | 22.16 ± 1.07 |
| `python3 ./fib_let.py 30` | 299.4 ± 2.1 | 296.9 | 304.7 | 23.41 ± 1.09 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/debug/rust_let.wasm` | 29.6 ± 0.6 | 28.5 | 31.4 | 2.31 ± 0.12 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/release/rust_let.wasm` | 18.7 ± 0.6 | 17.2 | 20.2 | 1.47 ± 0.08 |
| `echo 30 \| java -jar ./clojure_fib/target/uberjar/clojure_fib-0.1.0-SNAPSHOT-standalone.jar` | 572.6 ± 17.4 | 546.0 | 599.5 | 44.76 ± 2.47 |
| `echo 30 \| java -jar ./clojure_hi/target/uberjar/clojure_hi-0.1.0-SNAPSHOT-standalone.jar` | 555.2 ± 12.0 | 536.6 | 571.4 | 43.40 ± 2.21 |
| `echo 30 \| wasmtime ./fib_interpreted.wasm` | 7716.7 ± 27.4 | 7679.8 | 7761.5 | 603.19 ± 27.93 |
| `echo 30 \| wasmtime ./fib_interpreted_let.wasm` | 21414.3 ± 307.9 | 21122.2 | 22246.7 | 1673.87 ± 80.94 |
