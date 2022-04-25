#!/usr/bin/env bash

NUMBER=30
#NUMBER=25

touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp
mv csc_out.wasm fib_compiled.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp
mv csc_out.wasm fib_compiled_let.wasm

touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp no_compile
mv csc_out.wasm fib_interpreted.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp no_compile
mv csc_out.wasm fib_interpreted_let.wasm

touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib2.kp
mv csc_out.wasm fib_compiled_manual.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm builtin_fib.kp
mv csc_out.wasm builtin_fib.wasm

pushd rust_fib
cargo build --target=wasm32-wasi
cargo build --release --target=wasm32-wasi
cargo build
cargo build --release
popd

#pushd rust_let
#cargo build --target=wasm32-wasi
#cargo build --release --target=wasm32-wasi
#cargo build
#cargo build --release
#popd
#
#pushd clojure_fib
#lein uberjar
#popd
#
#pushd clojure_hi
#lein uberjar
#popd

#clang-11 fib.c -o c_fib
#clang-11 fib_let.c -o c_fib_let


hyperfine --warmup 2 --export-markdown table.md \
	      'echo '$NUMBER' | wasmtime ./fib_compiled.wasm' 'echo '$NUMBER' | wasmtime ./fib_compiled_let.wasm' \
          "scheme --script ./fib.scm $NUMBER" "scheme --script ./fib_let.scm $NUMBER" \
          "python3 ./fib.py $NUMBER" "python3 ./fib_let.py $NUMBER" \
          'echo '$NUMBER' | wasmtime ./rust_fib/target/wasm32-wasi/debug/rust_let.wasm' 'echo '$NUMBER' | wasmtime ./rust_fib/target/wasm32-wasi/release/rust_let.wasm' \

		  #'echo '$NUMBER' | wasmtime ./builtin_fib.wasm'  'echo '$NUMBER' | wasmtime ./fib_compiled_manual.wasm' \
          #'echo '$NUMBER' | wasmtime ./rust_fib/target/wasm32-wasi/debug/rust_let.wasm' 'echo '$NUMBER' | wasmtime ./rust_fib/target/wasm32-wasi/release/rust_let.wasm' \
          #'echo '$NUMBER' | ./rust_fib/target/debug/rust_let' 'echo '$NUMBER' | ./rust_fib/target/release/rust_let' \
	      #'echo '$NUMBER' | wasmtime ./fib_interpreted.wasm' 'echo '$NUMBER' | wasmtime ./fib_interpreted_let.wasm' \
          #"echo $NUMBER | java -jar ./clojure_fib/target/uberjar/clojure_fib-0.1.0-SNAPSHOT-standalone.jar" "echo $NUMBER | java -jar ./clojure_hi/target/uberjar/clojure_hi-0.1.0-SNAPSHOT-standalone.jar"

          #"./c_fib $NUMBER" "./c_fib_let $NUMBER" \

exit

#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time perf record -k mono wasmtime --jitdump ./csc_out.wasm
#exit

echo "Compile Straight"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time echo $NUMBER | wasmtime ./csc_out.wasm
#cp csc_out.wasm comp_fib_dyn.wasm

echo "Compile Straight 2"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp                && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib2.kp                && time echo $NUMBER | wasmtime ./csc_out.wasm

echo "Compile Builtin"
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm builtin_fib.kp         && time echo $NUMBER | wasmtime ./csc_out.wasm

echo "Interpret Straight"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp no_compile     && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib.kp no_compile     && time echo $NUMBER | wasmtime ./csc_out.wasm

echo "Compile Let"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp            && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp            && time echo $NUMBER | wasmtime ./csc_out.wasm

echo "Interpret Let"
#touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp no_compile && time echo $NUMBER | wasm3 ./csc_out.wasm
touch csc_out.wasm && rm csc_out.wasm && scheme --script ./partial_eval.scm fib_let.kp no_compile && time echo $NUMBER | wasmtime ./csc_out.wasm

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

echo "Rust x86 Debug"
pushd rust_fib
cargo build && time echo $NUMBER | ./target/debug/rust_let
popd

echo "Rust x86 Release"
pushd rust_fib
cargo build --release && time echo $NUMBER | ./target/release/rust_let
popd

echo "Clojure"
pushd clojure_fib
lein uberjar && time echo $NUMBER | time java -jar target/uberjar/clojure_fib-0.1.0-SNAPSHOT-standalone.jar
popd

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

