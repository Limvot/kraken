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
        wavm = pkgs.stdenv.mkDerivation rec {
		  pname = "wavm";
		  version = "0.0.0";

		  src = pkgs.fetchurl {
			url = "https://github.com/WAVM/WAVM/releases/download/nightly%2F2022-05-14/wavm-0.0.0-prerelease-linux.tar.gz";
			sha256 = "sha256-+PpnwPJDty6XCjjuHVFwiHc1q+k0zPF11EbRpqSKfyY=";
		  };

		  nativeBuildInputs = [
			pkgs.autoPatchelfHook
		  ];

		  buildInputs = [
			pkgs.stdenv.cc.cc.lib
		  ];

		  sourceRoot = ".";

		  installPhase = ''
            mkdir -p $out/bin
            cp -r bin/wavm $out/bin/
            cp -r include/ $out
            cp -r lib64/ $out
            cp -r share/ $out
			#install -m755 -D studio-link-standalone-v${version} $out/bin/studio-link
		  '';

		  meta = with pkgs.lib; {
			homepage = "https://wavm.github.io/";
			description = "WAVM WebAssembly Engine";
			platforms = platforms.linux;
		  };
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

            wavm

            hyperfine graph-cli
            (rust-bin.stable.latest.default.override {
              targets = [ "wasm32-wasi" ];
            })
            cmake
            stack (haskellPackages.ghcWithPackages (p: [p.parallel]))
            koka
            ocaml
            jdk
            swift
            picolisp
          ];
        };
      }
    ));
}
