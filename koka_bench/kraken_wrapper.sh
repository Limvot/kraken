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
