#!/usr/bin/env bash
OUR_DIR="$(dirname $(readlink -f $0))"
SOURCE="$1"
OUT_DIR="$2"
OUT_NAME="$3"

doit() {
	TAG=$1
	OPTION=$2
	scheme --script "$OUR_DIR/../partial_eval.scm" $SOURCE $OPTION
	mkdir -p "$OUT_DIR"
	mv ./csc_out.wasm "$OUT_DIR/$OUT_NAME$TAG.wasm"
	printf '#!/usr/bin/env bash\nwasmtime "$(dirname $(readlink -f $0))/'"$OUT_NAME$TAG"'.wasm" $@' > "$OUT_DIR/$OUT_NAME$TAG"
	chmod 755 "$OUT_DIR/$OUT_NAME$TAG"

	printf '#!/usr/bin/env bash\nWAVM_OBJECT_CACHE_DIR=$(pwd) wavm run "$(dirname $(readlink -f $0))/'"$OUT_NAME$TAG"'.wasm" $@' > "$OUT_DIR/$OUT_NAME$TAG-wavm"
	chmod 755 "$OUT_DIR/$OUT_NAME$TAG-wavm"
}

doit "-n" 					""
doit -slow 					no_compile
doit -no_lazy_env			no_lazy_env
doit -no_y_comb 			no_y_comb
doit -no_prim_inline		no_prim_inline
doit -no_closure_inline		no_closure_inline
