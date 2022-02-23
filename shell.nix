
with import <nixpkgs> { };

mkShell {
  LANG="en_US.UTF-8";
  nativeBuildInputs = [
    chicken
    #gambit
    gambit-unstable
    chez
    wabt
    wasmtime
    wasm3
    kakoune
  ];
}
