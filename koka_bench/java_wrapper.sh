#!/usr/bin/env bash
OUR_DIR="$(dirname $(readlink -f $0))"
SOURCE="$1"
OUT_DIR="$2"
OUT_NAME="$3"

mkdir -p "$OUT_DIR"
javac --enable-preview -source 17 -d "$OUT_DIR" $SOURCE

printf '#!/usr/bin/env bash\njava -Xss1024m --enable-preview -classpath "$(dirname $(readlink -f $0))" '"$OUT_NAME"' $@' > "$OUT_DIR/$OUT_NAME"
chmod 755 "$OUT_DIR/$OUT_NAME"
