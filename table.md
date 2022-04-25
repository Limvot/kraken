| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `echo 30 \| wasmtime ./fib_compiled.wasm` | 106.3 ± 1.7 | 103.8 | 111.8 | 8.18 ± 0.35 |
| `echo 30 \| wasmtime ./fib_compiled_let.wasm` | 142.1 ± 1.5 | 140.0 | 144.9 | 10.93 ± 0.45 |
| `echo 30 \| wasmtime ./builtin_fib.wasm` | 13.0 ± 0.5 | 11.7 | 14.7 | 1.00 |
| `echo 30 \| wasmtime ./fib_compiled_manual.wasm` | 305.9 ± 3.2 | 301.8 | 311.7 | 23.53 ± 0.98 |
| `scheme --script ./fib.scm 30` | 53.9 ± 0.6 | 53.0 | 56.0 | 4.15 ± 0.17 |
| `scheme --script ./fib_let.scm 30` | 54.0 ± 0.6 | 53.1 | 55.6 | 4.15 ± 0.17 |
| `python3 ./fib.py 30` | 287.6 ± 2.7 | 283.8 | 293.1 | 22.12 ± 0.91 |
| `python3 ./fib_let.py 30` | 303.8 ± 2.9 | 300.5 | 308.0 | 23.37 ± 0.97 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/debug/rust_let.wasm` | 29.7 ± 0.6 | 28.5 | 32.3 | 2.29 ± 0.10 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/release/rust_let.wasm` | 18.6 ± 0.4 | 17.8 | 19.8 | 1.43 ± 0.06 |
