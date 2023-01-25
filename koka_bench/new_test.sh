#!/usr/bin/env bash
set -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd "$SCRIPT_DIR"

	# Yeet ourselves inside a pure flake shell (-i for ignore-environment)
	if [[ -z "${INSIDE_FLAKE}" ]]; then
		echo "Not inside flake, entering"
		# thanks to https://stackoverflow.com/questions/59895/how-do-i-get-the-directory-where-a-bash-script-is-located-from-within-the-script
		echo "about to run nix develop"
		nix develop -i -c env INSIDE_FLAKE=true bash -c "$SCRIPT_DIR/new_test.sh"
		exit
	else
		echo "Inside flake, running!"
	fi

	rm -rf build || true
	mkdir build

	pushd build
		# workaround thanks to https://github.com/NixOS/nixpkgs/issues/139943
		cp -r  "$(dirname $(dirname $(which emcc)))/share/emscripten/cache" ./emcache
		chmod u+rwX -R emcache
		export EM_CACHE="$(pwd)/emcache"

        #no_compile
        #no_lazy_env
        #no_y_comb
        #no_prim_inline
        #no_closure_inline

		echo "RB-Tree"
	    ITERS=420000
		scheme --script ../../partial_eval.scm ../kraken/rbtree-opt.kp           && mv csc_out.wasm kraken-rbtree-opt.wasm
		koka --target=wasm -v -O2 ../koka/rbtree.kk && mv ./.koka/v*/emcc-wasm32-drelease/koka_rbtree.wasm ./
		#koka --target=c    -v -O2 ../koka/rbtree.kk && mv ./.koka/v*/cc-drelease/koka_rbtree ./

		hyperfine --warmup 2 "wasmtime ./koka_rbtree.wasm $ITERS" "wasmtime ./kraken-rbtree-opt.wasm $ITERS" --export-markdown rbtree_table.md --export-csv rbtree_table.csv

		echo "Fib"
	    ITERS=40
		scheme --script ../../partial_eval.scm ../kraken/fib.kp           && mv csc_out.wasm kraken-fib.wasm
		koka --target=wasm -v -O2 ../koka/fib.kk && mv ./.koka/v*/emcc-wasm32-drelease/koka_fib.wasm ./
		hyperfine --warmup 2 "wasmtime ./koka_fib.wasm $ITERS" "wasmtime ./kraken-fib.wasm $ITERS" --export-markdown fib_table.md --export-csv fib_table.csv


		echo "CFold"
	    ITERS=9
		scheme --script ../../partial_eval.scm ../kraken/cfold.kp           && mv csc_out.wasm kraken-cfold.wasm
		koka --target=wasm -v -O2 ../koka/cfold.kk && mv ./.koka/v*/emcc-wasm32-drelease/koka_cfold.wasm ./
		hyperfine --warmup 2 "wasmtime ./koka_cfold.wasm $ITERS" "wasmtime ./kraken-cfold.wasm $ITERS" --export-markdown cfold_table.md --export-csv cfold_table.csv

		echo "N-Queens"
	    ITERS=10
		scheme --script ../../partial_eval.scm ../kraken/nqueens.kp           && mv csc_out.wasm kraken-nqueens.wasm
		koka --target=wasm -v -O2 ../koka/nqueens.kk && mv ./.koka/v*/emcc-wasm32-drelease/koka_nqueens.wasm ./
		hyperfine --warmup 2 "wasmtime ./koka_nqueens.wasm $ITERS" "wasmtime ./kraken-nqueens.wasm $ITERS" --export-markdown nqueens_table.md --export-csv nqueens_table.csv

		echo "Deriv"
	    ITERS=9
		scheme --script ../../partial_eval.scm ../kraken/deriv.kp           && mv csc_out.wasm kraken-deriv.wasm
		koka --target=wasm -v -O2 ../koka/deriv.kk && mv ./.koka/v*/emcc-wasm32-drelease/koka_deriv.wasm ./
		hyperfine --warmup 2 "wasmtime ./koka_deriv.wasm $ITERS" "wasmtime ./kraken-deriv.wasm $ITERS" --export-markdown deriv_table.md --export-csv deriv_table.csv
	popd
popd
