
let
    moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
      nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
in with nixpkgs;
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
    wasmer
    kakoune
    #(rustChannelOf { rustToolchain = ./rust-toolchain; }).rust
    #(rustChannelOf { date = "2022-04-10"; channel = "nightly"; targets = [ "wasm32-wasi" ]; }).rust
    (latest.rustChannels.nightly.rust.override  {  targets = [ "wasm32-wasi" ]; })
    leiningen
  ];
}
