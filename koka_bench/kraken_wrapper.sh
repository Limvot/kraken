#!/usr/bin/env bash
OUR_DIR="$(dirname $(readlink -f $0))"
SOURCE="$1"
OUT_DIR="$2"
OUT_NAME="$3"

scheme --script "$OUR_DIR/../partial_eval.scm" $SOURCE
mkdir -p "$OUT_DIR"
mv ./csc_out.wasm "$OUT_DIR/$OUT_NAME.wasm"
printf '#!/usr/bin/env bash\nwasmtime "$(dirname $(readlink -f $0))/'"$OUT_NAME"'.wasm" $@' > "$OUT_DIR/$OUT_NAME"
chmod 755 "$OUT_DIR/$OUT_NAME"

printf '#!/usr/bin/env bash\nWAVM_OBJECT_CACHE_DIR=$(pwd) wavm run "$(dirname $(readlink -f $0))/'"$OUT_NAME"'.wasm" $@' > "$OUT_DIR/$OUT_NAME-wavm"
chmod 755 "$OUT_DIR/$OUT_NAME-wavm"


scheme --script "$OUR_DIR/../partial_eval.scm" $SOURCE no_compile
mv ./csc_out.wasm "$OUT_DIR/$OUT_NAME-slow.wasm"
#printf '#!/usr/bin/env bash\nwasmtime "$(dirname $(readlink -f $0))/'"$OUT_NAME-slow"'.wasm" $@' > "$OUT_DIR/$OUT_NAME-slow"
#chmod 755 "$OUT_DIR/$OUT_NAME-slow"

printf '#!/usr/bin/env bash\nWAVM_OBJECT_CACHE_DIR=$(pwd) wavm run "$(dirname $(readlink -f $0))/'"$OUT_NAME-slow"'.wasm" $@' > "$OUT_DIR/$OUT_NAME-slow-wavm"
chmod 755 "$OUT_DIR/$OUT_NAME-slow-wavm"
