#!/usr/bin/env bash
set -e

if [[ ! -d "build" ]]
then
	mkdir build
	pushd build
	nix develop -i -c bash -c 'cmake .. -DCMAKE_BUILD_TYPE=Release && cmake --build .'
	popd
fi
pushd build
nix develop -i -c bash -c 'make'
popd

nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*nqueens\* -printf "\"%p 10\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown rbnqueens_table.md'
nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*rbtree\* -printf "\"%p 42000\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown rbtree_table.md'
nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*cfold\* -printf "\"%p 5\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown cfold_table.md'
nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*deriv\* -printf "\"%p 8\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown deriv_table.md'

printf "# Benchmarks\n\n" > benchmarks.md
for x in *_table.md
do
	printf "## $x\n\n" >> benchmarks.md
	cat "$x" >> benchmarks.md
	printf "\n\n\n" >> benchmarks.md
done
