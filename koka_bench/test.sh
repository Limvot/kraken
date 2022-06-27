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

mkdir -p slow
find build -type f -name \*slow\* -exec mv {} slow \;
cp ./build/kraken/out/bench/kraken-* ./slow


nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*nqueens\* -printf "\"%p 10\"\n"   | xargs hyperfine --ignore-failure --warmup 2 --export-markdown nqueens_table.md --export-csv nqueens_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*rbtree\* -printf "\"%p 42000\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown rbtree_table.md --export-csv rbtree_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*cfold\* -printf "\"%p 5\"\n"      | xargs hyperfine --ignore-failure --warmup 2 --export-markdown cfold_table.md --export-csv cfold_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*deriv\* -printf "\"%p 8\"\n"      | xargs hyperfine --ignore-failure --warmup 2 --export-markdown deriv_table.md --export-csv deriv_table.csv'

#nix develop -i -c bash -c 'ulimit -s unlimited && find slow -type f -executable -name \*nqueens\* -printf "\"%p 7\"\n"  | xargs hyperfine --ignore-failure --warmup 2 --export-markdown slow_nqueens_table.md --export-csv slow_nqueens_table.csv'
#nix develop -i -c bash -c 'ulimit -s unlimited && find slow -type f -executable -name \*cfold\* -printf "\"%p 5\"\n"    | xargs hyperfine --ignore-failure --warmup 2 --export-markdown slow_cfold_table.md   --export-csv slow_cfold_table.csv'
#nix develop -i -c bash -c 'ulimit -s unlimited && find slow -type f -executable -name \*deriv\* -printf "\"%p 3\"\n"    | xargs hyperfine --ignore-failure --warmup 2 --export-markdown slow_deriv_table.md   --export-csv slow_deriv_table.csv'
#nix develop -i -c bash -c 'ulimit -s unlimited && find slow -type f -executable -name \*rbtree\* -printf "\"%p 100\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown slow_rbtree_table.md  --export-csv slow_rbtree_table.csv'

for x in *_table.csv
do
	./relative.py $x
done

printf "# Benchmarks\n\n" > benchmarks_table.md
for x in *_table.md
do
	printf "## $x\n\n" >> benchmarks_table.md
	cat "$x" >> benchmarks_table.md
	printf "\n\n\n" >> benchmarks_table.md
done
