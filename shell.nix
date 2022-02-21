
with import <nixpkgs> { };

mkShell {
  LANG="en_US.UTF-8";
  nativeBuildInputs = [
    chicken
    wabt
    wasmtime
    wasm3
    kakoune
  ];
}
