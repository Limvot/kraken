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

	ITERS=420000

	rm -rf build || true
	mkdir build


	pushd build
		# workaround thanks to https://github.com/NixOS/nixpkgs/issues/139943
		cp -r  "$(dirname $(dirname $(which emcc)))/share/emscripten/cache" ./emcache
		chmod u+rwX -R emcache
		export EM_CACHE="$(pwd)/emcache"

		scheme --script ../../partial_eval.scm  ../kraken/rbtree-opt.kp && mv csc_out.wasm kraken-rbtree-opt.wasm
		koka --target=wasm -v -O2 ../koka/rbtree.kk && mv ./.koka/v*/emcc-wasm32-drelease/koka_rbtree.wasm ./
		koka --target=c    -v -O2 ../koka/rbtree.kk && mv ./.koka/v*/cc-drelease/koka_rbtree ./

		hyperfine --warmup 2 "./koka_rbtree $ITERS" "wasmtime ./koka_rbtree.wasm $ITERS" "wasmtime ./kraken-rbtree-opt.wasm $ITERS"
	popd
popd
