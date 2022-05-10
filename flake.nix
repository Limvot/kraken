{
  description = "Env for Kraken and the extacted Koka bencmarks";
  inputs = {

    #flake.lock pins a particular version of 21.11 that has non-broken Swift
    nixpkgs.url = "nixpkgs/nixos-21.11";
    #nixpkgs.url = "github:NixOS/nixpkgs";

    # Pure-er, so we don't have to mess with the --impure flag
    moz_overlay.url = "github:oxalica/rust-overlay";
    #moz_overlay.url = "github:mozilla/nixpkgs-mozilla";

    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, moz_overlay, flake-utils }:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ moz_overlay.overlay ];
        };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [

            chicken
            #gambit
            gambit-unstable
            chez
            wabt
            wasmtime
            wasm3
            wasmer
            leiningen
            clang
            kakoune

            hyperfine
            (rust-bin.stable.latest.default.override {
              targets = [ "wasm32-wasi" ];
            })
            cmake
            stack (haskellPackages.ghcWithPackages (p: [p.parallel]))
            koka
            ocaml
            jdk
            swift
          ];
        };
      }
    ));
}
