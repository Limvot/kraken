| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `echo 30 \| wasmtime ./fib_compiled.wasm` | 107.5 ± 2.2 | 105.6 | 116.4 | 9.33 ± 0.51 |
| `echo 30 \| wasmtime ./fib_compiled_let.wasm` | 469.3 ± 3.1 | 464.3 | 474.2 | 40.74 ± 2.08 |
| `echo 30 \| wasmtime ./builtin_fib.wasm` | 11.5 ± 0.6 | 10.4 | 14.3 | 1.00 |
| `echo 30 \| wasmtime ./fib_compiled_manual.wasm` | 292.5 ± 5.7 | 287.5 | 308.1 | 25.39 ± 1.38 |
| `scheme --script ./fib.scm 30` | 54.6 ± 1.4 | 52.8 | 60.1 | 4.74 ± 0.27 |
| `scheme --script ./fib_let.scm 30` | 53.7 ± 0.9 | 52.2 | 55.9 | 4.67 ± 0.25 |
| `python3 ./fib.py 30` | 291.7 ± 3.7 | 286.0 | 296.9 | 25.32 ± 1.32 |
| `python3 ./fib_let.py 30` | 303.7 ± 4.2 | 293.7 | 308.0 | 26.36 ± 1.38 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/debug/rust_let.wasm` | 29.6 ± 0.6 | 28.4 | 31.6 | 2.57 ± 0.14 |
| `echo 30 \| wasmtime ./rust_fib/target/wasm32-wasi/release/rust_let.wasm` | 18.4 ± 0.5 | 17.3 | 20.0 | 1.59 ± 0.09 |
