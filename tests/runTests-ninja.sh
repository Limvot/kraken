#!/bin/bash

krakenPath="../build-ninja/kraken"
#testDir=${1:-"../tests"}
testDir="."
ext=${2:-"krak"}

fileList=""
for dir in `find ${testDir} -type f -name "test_*.${ext}"`; do
	filename=$(basename ${dir})
	filename="${filename%.*}"
	fileList+=\ $testDir\/$filename
done

${krakenPath} "--test" ${fileList}
