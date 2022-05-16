# NOTE

These are the benchmarks extracted from the Koka repository https://github.com/koka-lang/koka
at b1670308f88dd1fc6c22cad28385fcb185d5b27d from the test/bench directory
and modified by me to run inside a reproducable environment defined by a Nix Flake,
and to use Bash to coordinate and Hyperfine to time instead of the Koka based runner.

./test.sh will build and run the benchmarks.

I'm slowly porting them over to the Kraken project, and adding Kraken versions of each one.


## Some explanation from a PLDI Readme originally included:

Available languages are:

- `kk`  : Koka v2.1.1 compiling using gcc 9.3.0.
- `kkx` : Koka v2.1.1 compiling using gcc 9.3.0 but without reuse optimization (Section 2.4).
- `ml`  : OCaml v4.08.1 using the optimizing compiler (`ocamlopt`)
- `hs`  : Haskell GHC 8.6.5.
- `sw`  : Swift 5.3.3.
- `jv`  : Java 15.0.2, Java(TM) SE Runtime Environment (build 15.0.2+7-27), 
          Java HotSpot(TM) 64-Bit Server VM (build 15.0.2+7-27, mixed mode, sharing).
- `cpp` : GCC 9.3.0, 

Available tests are described in detail in Section 4 and are:

- `rbtree`    : inserts 42 million items into a red-black tree.
- `rbtree-ck` : a variant of rbtree that keeps a list of every 5th subtree and thus shares many subtrees.
- `deriv`     : the symbolic derivative of a large expression.
- `nqueens`   : calculates all solutions for the n-queens problem of size 13 into a list, and returns the length of that list.  
- `cfold`     : constant-folding over a large symbolic expression.


# Original README follows:


# Build and run benchmarks

This contains the standard benchmark suite (discussed in detail in [Perceus] paper).
It is still basic but more benchmarks
with effect handlers are coming. The suite can run on (Ubuntu Linux), WSL2, and macOS,
and the benchmarks need:

- `gcc`. Should be there, otherwise use `sudo apt install gcc`,
- `ghc`. Use `sudo apt install ghc`,
- `ocamlopt`. Use `sudo apt install ocaml`.
  We used the new multi-core OCaml, see <https://github.com/ocaml-multicore/multicore-opam> 
  for installation instructions (including `domainslib` for the binarytrees benchmark)
  ```
  > opam update
  > opam switch create 4.12.0+domains+effects --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default
  > opam install dune domainslib
  ```

- `swiftc`. The Swift compiler can be downloaded [here](https://swift.org/download/).
   The benchmarks expect `swiftc` to be installed at `/opt/swift/bin`,
   so unpack and copy everything under `swift-.../usr` to `/opt/swift/bin`:
   ```
   > tar -xzf swift-5.5-RELEASE-ubuntu20.04.tar.gz
   > cd swift-5.5-RELEASE-ubuntu20.04/usr
   > sudo mkdir /opt/swift
   > sudo cp -r * /opt/swift
   ```

- `javac`/`java`. We used these [instructions](https://www.linuxcapable.com/how-to-install-java-17-lts-jdk-17-on-ubuntu-20-04/)
   to install the Java SE 17 Hotspot compiler:
   ```
   > sudo apt update
   > sudo add-apt-repository ppa:linuxuprising/java
   > sudo apt-get -y install oracle-java17-installer oracle-java17-set-default
   > java --version
   java 17 2021-09-14 LTS
   Java(TM) SE Runtime Environment (build 17+35-LTS-2724)
   Java HotSpot(TM) 64-Bit Server VM (build 17+35-LTS-2724, mixed mode, sharing)
   ```

The benchmarks can now be build using:

```
> cd test/bench
> mkdir build
> cd build
> cmake .. -DCMAKE_BUILD_TYPE=Release
> cmake --build .
```

For some benchmarks, like `cfold`, we may need a large stack, so it may be good to raise the limit:
```
> ulimit -s unlimited
```

We can then run all benchmarks as:
```
> ctest .
```
Or only run benchmarks for one language with `-L <lang>`:
```
> ctest -L koka
```
Or run specific benchmarks using `-R <regex>`,
like the symbolic derivative benchmark:
```
> ctest -R deriv      
Test project /home/daan/dev/koka/test/bench/build
    Start  4: hs-deriv
1/4 Test  #4: hs-deriv .........................   Passed    2.29 sec
    Start 10: kk-deriv
2/4 Test #10: kk-deriv .........................   Passed    1.25 sec
    Start 19: ml-deriv
3/4 Test #19: ml-deriv .........................   Passed    1.73 sec
    Start 25: sw-deriv
4/4 Test #25: sw-deriv .........................   Passed    2.88 sec

100% tests passed, 0 tests failed out of 4
...
```

We can also run the tests using the `test/bench/bench.kk` script instead of
using `ctest` which also measures peak working set and calculates
normalized scores. For example, from the `build` directory, we can run all benchmarks as:
```
> koka -e ../bench
```
Use the `--lang` or `--test` options to specify a comma separated list of
languages or benchmarks:
```
> koka -e ../bench -- --lang=koka,ocaml  --test=rbtree,rbtree-ck
```
The `-i<N>` switch runs `N` iterations on each benchmark and calculates
the average and the error interval.
