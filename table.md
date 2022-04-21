| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `echo 30 \| wasmtime ./fib_compiled.wasm` | 281.3 ± 4.3 | 274.3 | 289.5 | 24.35 ± 1.25 |
| `echo 30 \| wasmtime ./fib_compiled_let.wasm` | 716.4 ± 52.4 | 692.3 | 862.6 | 62.00 ± 5.46 |
| `echo 30 \| wasmtime ./builtin_fib.wasm` | 11.6 ± 0.6 | 10.3 | 13.2 | 1.00 |
| `echo 30 \| wasmtime ./fib_compiled_manual.wasm` | 468.8 ± 4.1 | 462.5 | 477.2 | 40.57 ± 2.03 |
| `scheme --script ./fib.scm 30` | 53.4 ± 0.8 | 52.2 | 57.4 | 4.62 ± 0.24 |
| `scheme --script ./fib_let.scm 30` | 53.6 ± 0.9 | 52.4 | 56.7 | 4.64 ± 0.24 |
| `python3 ./fib.py 30` | 284.7 ± 5.6 | 276.9 | 292.7 | 24.64 ± 1.30 |
| `python3 ./fib_let.py 30` | 299.8 ± 5.3 | 291.5 | 304.7 | 25.94 ± 1.36 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/debug/rust_let.wasm` | 29.8 ± 0.7 | 28.7 | 32.5 | 2.58 ± 0.14 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/release/rust_let.wasm` | 18.5 ± 0.6 | 17.4 | 20.4 | 1.60 ± 0.09 |
