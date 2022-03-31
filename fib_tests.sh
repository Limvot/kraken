#!/usr/bin/env bash

NUMBER=30
#NUMBER=25

echo "Compile Straight"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time echo $NUMBER | wasmtime ./csc_out.wasm
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time echo $NUMBER | wasmer ./csc_out.wasm

echo "Interpret Straight"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp no_compile     && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp no_compile     && time echo $NUMBER | wasmtime ./csc_out.wasm

#echo "Compile Let"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp            && time echo $NUMBER | wasm3 ./csc_out.wasm

#echo "Interpret Let"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp no_compile && time echo $NUMBER | wasm3 ./csc_out.wasm


