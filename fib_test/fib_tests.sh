#!/usr/bin/env bash

NUMBER=30
#NUMBER=27

touch csc_out.wasm && rm csc_out.wasm && scheme --script ../partial_eval.scm fib.kp
mv csc_out.wasm fib_compiled.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ../partial_eval.scm fib_let.kp
mv csc_out.wasm fib_compiled_let.wasm

touch csc_out.wasm && rm csc_out.wasm && scheme --script ../partial_eval.scm fib.kp no_compile
mv csc_out.wasm fib_interpreted.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ../partial_eval.scm fib_let.kp no_compile
mv csc_out.wasm fib_interpreted_let.wasm

touch csc_out.wasm && rm csc_out.wasm && scheme --script ../partial_eval.scm fib2.kp
mv csc_out.wasm fib_compiled_manual.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ../partial_eval.scm builtin_fib.kp
mv csc_out.wasm builtin_fib.wasm

pushd rust_fib
cargo build --target=wasm32-wasi
cargo build --release --target=wasm32-wasi
cargo build
cargo build --release
popd

pushd clojure_fib
lein uberjar
popd

pushd clojure_hi
lein uberjar
popd

hyperfine --warmup 2 --export-markdown table.md \
	      'echo '$NUMBER' | wasmtime ./fib_compiled.wasm' 'echo '$NUMBER' | wasmtime ./fib_compiled_let.wasm' \
		  'echo '$NUMBER' | wasmtime ./builtin_fib.wasm'  'echo '$NUMBER' | wasmtime ./fib_compiled_manual.wasm' \
          "scheme --script ./fib.scm $NUMBER" "scheme --script ./fib_let.scm $NUMBER" \
          "python3 ./fib.py $NUMBER" "python3 ./fib_let.py $NUMBER" \
          'echo '$NUMBER' | wasmtime ./rust_fib/target/wasm32-wasi/debug/rust_let.wasm' 'echo '$NUMBER' | wasmtime ./rust_fib/target/wasm32-wasi/release/rust_let.wasm' \
          "echo $NUMBER | java -jar ./clojure_fib/target/uberjar/clojure_fib-0.1.0-SNAPSHOT-standalone.jar" "echo $NUMBER | java -jar ./clojure_hi/target/uberjar/clojure_hi-0.1.0-SNAPSHOT-standalone.jar" \
	      'echo '$NUMBER' | wasmtime ./fib_interpreted.wasm' 'echo '$NUMBER' | wasmtime ./fib_interpreted_let.wasm' \
		  #end
