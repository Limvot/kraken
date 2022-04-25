| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `echo 30 \| wasmtime ./fib_compiled.wasm` | 106.4 ± 4.2 | 102.8 | 122.5 | 5.68 ± 0.28 |
| `echo 30 \| wasmtime ./fib_compiled_let.wasm` | 138.8 ± 4.2 | 136.1 | 155.8 | 7.42 ± 0.31 |
| `scheme --script ./fib.scm 30` | 53.9 ± 0.8 | 52.8 | 56.1 | 2.88 ± 0.10 |
| `scheme --script ./fib_let.scm 30` | 54.1 ± 0.8 | 53.1 | 57.3 | 2.89 ± 0.10 |
| `python3 ./fib.py 30` | 286.5 ± 2.9 | 281.0 | 289.8 | 15.31 ± 0.48 |
| `python3 ./fib_let.py 30` | 300.9 ± 2.0 | 297.8 | 303.9 | 16.08 ± 0.49 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/debug/rust_let.wasm` | 29.6 ± 0.7 | 28.7 | 33.2 | 1.58 ± 0.06 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/release/rust_let.wasm` | 18.7 ± 0.6 | 17.7 | 21.1 | 1.00 |
