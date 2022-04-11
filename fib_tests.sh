#!/usr/bin/env bash

NUMBER=30
#NUMBER=25

#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time perf record -k mono wasmtime --jitdump ./csc_out.wasm
#exit

echo "Compile Straight"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time echo $NUMBER | wasmtime ./csc_out.wasm
#cp csc_out.wasm comp_fib_dyn.wasm

echo "Compile Straight 2"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib2.kp                && time echo $NUMBER | wasmtime ./csc_out.wasm

echo "Python"
time python3 ./fib.py $NUMBER

echo "Rust Wasm Debug"
pushd rust_fib
cargo build --target=wasm32-wasi && time echo $NUMBER | wasmtime target/wasm32-wasi/debug/rust_let.wasm
popd

echo "Rust Wasm Release"
pushd rust_fib
cargo build --release --target=wasm32-wasi && time echo $NUMBER | wasmtime target/wasm32-wasi/release/rust_let.wasm
popd

exit

echo "Interpret Straight"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp no_compile     && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp no_compile     && time echo $NUMBER | wasmtime ./csc_out.wasm

echo "Compile Let"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp            && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp            && time echo $NUMBER | wasmtime ./csc_out.wasm

echo "Interpret Let"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp no_compile && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp no_compile && time echo $NUMBER | wasmtime ./csc_out.wasm

echo "Chez Scheme"
time scheme --script ./fib.scm $NUMBER

echo "Chez Scheme Let"
time scheme --script ./fib_let.scm $NUMBER

echo "Python"
time python3 ./fib.py $NUMBER

echo "Python Let"
time python3 ./fib_let.py $NUMBER

echo "C"
clang-11 fib.c -o fib && time ./fib $NUMBER

echo "C let"
clang-11 fib_let.c -o fib_let && time ./fib_let $NUMBER

