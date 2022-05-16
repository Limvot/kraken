#!/usr/bin/env bash
set -e

if [[ ! -d "build" ]]
then
	mkdir build
	pushd build
	nix develop -i -c bash -c 'cmake .. -DCMAKE_BUILD_TYPE=Release && cmake --build .'
	popd
    rm -rf  ./build/CMakeFiles || true
fi

nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*rbtree -printf "\"%p 42000\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown rbtree_table.md'
