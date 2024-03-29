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
cp ./slow/newlisp-slow-fexpr-rbtree ./build/newlisp/out/bench/
#cp ./build/kraken/out/bench/kraken-* ./slow
cp ./build/kraken/out/bench/kraken-*-n* ./slow
mv ./build/kraken/out/bench/kraken-cfold-n ./slow
#mv ./build/newlisp/out/bench/* ./slow
 

nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*rbtree\* -printf "\"%p 880\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown rbtree_table.md --export-csv rbtree_table.csv'
#nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*rbtree\* -printf "\"%p 420000\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown rbtree_table.md --export-csv rbtree_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*fib\* -printf "\"%p 30\"\n"      | xargs hyperfine --ignore-failure --warmup 2 --export-markdown fib_table.md --export-csv fib_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*nqueens\* -printf "\"%p 8\"\n"   | xargs hyperfine --ignore-failure --warmup 2 --export-markdown nqueens_table.md --export-csv nqueens_table.csv'
#nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*nqueens\* -printf "\"%p 10\"\n"   | xargs hyperfine --ignore-failure --warmup 2 --export-markdown nqueens_table.md --export-csv nqueens_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*deriv\* -printf "\"%p 8\"\n"      | xargs hyperfine --ignore-failure --warmup 2 --export-markdown deriv_table.md --export-csv deriv_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*cfold\* -printf "\"%p 7\"\n"      | xargs hyperfine --ignore-failure --warmup 2 --export-markdown cfold_table.md --export-csv cfold_table.csv'
#nix develop -i -c bash -c 'ulimit -s unlimited && find build -type f -executable -name \*cfold\* -printf "\"%p 20\"\n"      | xargs hyperfine --ignore-failure --warmup 2 --export-markdown cfold_table.md --export-csv cfold_table.csv'


#nix develop -i -c bash -c 'ulimit -s unlimited && find slow -type f -executable -name \*rbtree\* -printf "\"%p 10\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown slow_rbtree_table.md  --export-csv slow_rbtree_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find slow -type f -executable -name \*rbtree\* -printf "\"%p 100\"\n" | xargs hyperfine --ignore-failure --warmup 2 --export-markdown slow_rbtree_table.md  --export-csv slow_rbtree_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find slow -type f -executable -name \*fib\* -printf "\"%p 30\"\n"  | xargs hyperfine --ignore-failure --warmup 2 --export-markdown slow_fib_table.md --export-csv slow_fib_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find slow -type f -executable -name \*nqueens\* -printf "\"%p 7\"\n"  | xargs hyperfine --ignore-failure --warmup 2 --export-markdown slow_nqueens_table.md --export-csv slow_nqueens_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find slow -type f -executable -name \*deriv\* -printf "\"%p 3\"\n"    | xargs hyperfine --ignore-failure --warmup 2 --export-markdown slow_deriv_table.md   --export-csv slow_deriv_table.csv'
nix develop -i -c bash -c 'ulimit -s unlimited && find slow -type f -executable -name \*cfold\* -printf "\"%p 5\"\n"    | xargs hyperfine --ignore-failure --warmup 2 --export-markdown slow_cfold_table.md   --export-csv slow_cfold_table.csv'

#nix develop -i -c bash -c 'ulimit -s unlimited && hyperfine --ignore-failure --warmup 2 --export-markdown slow_ish_rbtree_table.md  --export-csv slow_ish_rbtree_table.csv "./slow/kraken-rbtree-opt 890" "./slow/kraken-rbtree-opt-wavm 890" "./slow/newlisp-slow-fexpr-rbtree 890" "./slow/newlisp-macro-rbtree 890"'
#nix develop -i -c bash -c 'ulimit -s unlimited && hyperfine --ignore-failure --warmup 2 --export-markdown slow_rbtree_table.md  --export-csv slow_rbtree_table.csv "./slow/kraken-rbtree-opt 100" "./slow/kraken-rbtree-opt-wavm 100" "./slow/newlisp-slow-fexpr-rbtree 100" "./slow/newlisp-macro-rbtree 100" "./slow/kraken-rbtree-slow-wavm 100"'

for x in *_table.csv
do
	nix develop -i -c bash -c "./relative.py $x"
done

printf "# Benchmarks\n\n" > benchmarks.md
for x in *_table.md
do
	printf "## $x\n\n" >> benchmarks.md
	cat "$x" >> benchmarks.md
	printf "\n\n\n" >> benchmarks.md
done
cp *.png ~/school/vau_partial_eval_paper/images/
cp *.cssv ~/school/vau_partial_eval_paper/
