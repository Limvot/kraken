| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `echo 30 \| wasmtime ./fib_compiled.wasm` | 500.3 ± 3.0 | 495.3 | 503.8 | 111.77 ± 14.02 |
| `echo 30 \| wasmtime ./fib_compiled_let.wasm` | 816.4 ± 3.4 | 812.8 | 821.6 | 182.39 ± 22.86 |
| `echo 30 \| wasmtime ./fib_interpreted.wasm` | 11818.0 ± 64.9 | 11751.4 | 11983.8 | 2640.24 ± 331.01 |
| `echo 30 \| wasmtime ./fib_interpreted_let.wasm` | 32808.0 ± 126.9 | 32655.0 | 33015.6 | 7329.58 ± 918.47 |
| `echo 30 \| wasmtime ./builtin_fib.wasm` | 11.4 ± 1.8 | 10.1 | 28.6 | 2.55 ± 0.52 |
| `echo 30 \| wasmtime ./fib_compiled_manual.wasm` | 562.4 ± 3.9 | 556.2 | 569.0 | 125.64 ± 15.76 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/debug/rust_let.wasm` | 29.8 ± 2.3 | 28.5 | 47.0 | 6.66 ± 0.98 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/release/rust_let.wasm` | 19.0 ± 2.1 | 17.6 | 36.9 | 4.25 ± 0.71 |
| `echo 30 \| ./rust_fib/target/debug/rust_let` | 8.3 ± 0.5 | 7.2 | 10.3 | 1.86 ± 0.26 |
| `echo 30 \| ./rust_fib/target/release/rust_let` | 4.5 ± 0.6 | 3.2 | 6.2 | 1.00 |
| `scheme --script ./fib.scm 30` | 53.7 ± 0.7 | 52.8 | 55.5 | 11.99 ± 1.51 |
| `scheme --script ./fib_let.scm 30` | 53.3 ± 0.5 | 52.5 | 54.5 | 11.91 ± 1.50 |
| `python3 ./fib.py 30` | 287.6 ± 3.3 | 282.1 | 292.7 | 64.26 ± 8.08 |
| `python3 ./fib_let.py 30` | 304.3 ± 3.6 | 299.4 | 310.0 | 67.99 ± 8.55 |
| `echo 30 \| java -jar ./clojure_fib/target/uberjar/clojure_fib-0.1.0-SNAPSHOT-standalone.jar` | 686.2 ± 14.6 | 676.0 | 725.2 | 153.29 ± 19.47 |
| `echo 30 \| java -jar ./clojure_hi/target/uberjar/clojure_hi-0.1.0-SNAPSHOT-standalone.jar` | 658.2 ± 13.2 | 639.2 | 679.3 | 147.05 ± 18.65 |
