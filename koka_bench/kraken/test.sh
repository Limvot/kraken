#!/usr/bin/env bash
set -e
NUMBER=42000

#rm rbtree.wasm || true
#scheme --script ../../partial_eval.scm rbtree.kp     && mv csc_out.wasm rbtree.wasm
#hyperfine --warmup 2 "wasmtime ./rbtree.wasm $NUMBER" "wasmtime ./old_rbtree.wasm $NUMBER"

rm rbtree-opt.wasm || true
scheme --script ../../partial_eval.scm rbtree-opt.kp && mv csc_out.wasm rbtree-opt.wasm
hyperfine --warmup 2  "wasmtime ./rbtree-opt.wasm $NUMBER" "wasmtime ./old_rbtree-opt.wasm $NUMBER"

#rm rbtree.wasm || true
#rm rbtree-opt.wasm || true
#scheme --script ../../partial_eval.scm rbtree.kp     && mv csc_out.wasm rbtree.wasm
#scheme --script ../../partial_eval.scm rbtree-opt.kp && mv csc_out.wasm rbtree-opt.wasm
#hyperfine --warmup 2 "wasmtime ./rbtree.wasm $NUMBER" "wasmtime ./rbtree-opt.wasm $NUMBER" "wasmtime ./old_rbtree.wasm $NUMBER" "wasmtime ./old_rbtree-opt.wasm $NUMBER"

