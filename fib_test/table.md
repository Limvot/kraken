| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `echo 30 \| wasmtime ./fib_compiled.wasm` | 104.2 ± 1.7 | 102.3 | 109.5 | 8.43 ± 0.46 |
| `echo 30 \| wasmtime ./fib_compiled_let.wasm` | 138.2 ± 1.6 | 134.9 | 141.0 | 11.19 ± 0.59 |
| `echo 30 \| wasmtime ./builtin_fib.wasm` | 12.4 ± 0.6 | 10.9 | 15.8 | 1.00 |
| `echo 30 \| wasmtime ./fib_compiled_manual.wasm` | 299.3 ± 2.2 | 296.5 | 302.5 | 24.23 ± 1.27 |
| `scheme --script ./fib.scm 30` | 53.6 ± 1.1 | 52.4 | 59.2 | 4.34 ± 0.24 |
| `scheme --script ./fib_let.scm 30` | 53.7 ± 0.7 | 52.7 | 55.2 | 4.35 ± 0.23 |
| `python3 ./fib.py 30` | 288.4 ± 3.1 | 283.6 | 292.7 | 23.34 ± 1.23 |
| `python3 ./fib_let.py 30` | 303.4 ± 1.3 | 300.6 | 304.9 | 24.56 ± 1.27 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/debug/rust_let.wasm` | 30.0 ± 0.6 | 28.6 | 31.7 | 2.43 ± 0.14 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/release/rust_let.wasm` | 18.9 ± 0.5 | 17.7 | 20.1 | 1.53 ± 0.09 |
| `echo 30 \| java -jar ./clojure_fib/target/uberjar/clojure_fib-0.1.0-SNAPSHOT-standalone.jar` | 691.4 ± 13.8 | 674.5 | 721.1 | 55.97 ± 3.10 |
| `echo 30 \| java -jar ./clojure_hi/target/uberjar/clojure_hi-0.1.0-SNAPSHOT-standalone.jar` | 658.8 ± 19.1 | 639.7 | 699.9 | 53.33 ± 3.16 |
| `echo 30 \| wasmtime ./fib_interpreted.wasm` | 11335.6 ± 65.1 | 11258.2 | 11479.3 | 917.63 ± 47.75 |
| `echo 30 \| wasmtime ./fib_interpreted_let.wasm` | 31732.1 ± 161.9 | 31510.3 | 32067.4 | 2568.76 ± 133.49 |
