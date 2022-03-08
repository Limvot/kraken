#!/usr/bin/env bash

echo
echo "Partially Evaluating & compiling " $@
echo "Source is"
cat $@
echo
touch csc_out.wasm && rm csc_out.wasm && time scheme --script ./partial_eval.scm $@
echo
echo "Running"
echo
wasmtime ./csc_out.wasm
echo
echo
