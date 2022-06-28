#!/usr/bin/env bash
OUR_DIR="$(dirname $(readlink -f $0))"
SOURCE="$1"
OUT_DIR="$2"
OUT_NAME="$3"

mkdir -p "$OUT_DIR"
cp $SOURCE "$OUT_DIR/$OUT_NAME"
