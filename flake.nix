{
  description = "Env for Kraken and the extacted Koka bencmarks";
  inputs = {
    # For some reason the newer one has broken koka/emscripten (probs same change)
    nixpkgs_stable_new.url = "nixpkgs/nixos-22.11";
    nixpkgs_stable_old.url = "nixpkgs/nixos-21.11";
    #nixpkgs.url = "github:NixOS/nixpkgs";
    moz_overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs_stable_new, nixpkgs_stable_old, moz_overlay, flake-utils }:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs_new = import nixpkgs_stable_new {
          inherit system;
          overlays = [ moz_overlay.overlay ];
        };
        pkgs_old = import nixpkgs_stable_old {
          inherit system;
          overlays = [ moz_overlay.overlay ];
        };
        #newlisp = pkgs.stdenv.mkDerivation rec {
		  #pname = "newLisp";
		  #version = "10.7.5";

		  #src = pkgs.fetchurl {
			#url = "http://www.newlisp.org/downloads/newlisp-10.7.5.tgz";
			#sha256 = "sha256-3C0P9lHCsnW8SvOvi6WYUab7bh6t3CCudftgsekBJuw=";
		  #};

		  #nativeBuildInputs = [
			#pkgs.autoPatchelfHook
		  #];

		  #buildInputs = [
			#pkgs.stdenv.cc.cc.lib
            #pkgs.libffi
            #pkgs.readline
		  #];

		  #installPhase = ''
            #mkdir -p $out/bin
            #cp newlisp $out/bin
		  #'';

		  #meta = with pkgs.lib; {
			#homepage = "http://www.newlisp.org/index.cgi";
			#description = "A Lisp-like, general-purpose scripting language";
			#platforms = platforms.linux;
		  #};
        #};
        #wavm = pkgs.stdenv.mkDerivation rec {
		  #pname = "wavm";
		  #version = "0.0.0";

		  #src = pkgs.fetchurl {
			#url = "https://github.com/WAVM/WAVM/releases/download/nightly%2F2022-05-14/wavm-0.0.0-prerelease-linux.tar.gz";
			#sha256 = "sha256-+PpnwPJDty6XCjjuHVFwiHc1q+k0zPF11EbRpqSKfyY=";
		  #};

		  #nativeBuildInputs = [
			#pkgs.autoPatchelfHook
		  #];

		  #buildInputs = [
			#pkgs.stdenv.cc.cc.lib
		  #];

		  #sourceRoot = ".";

		  #installPhase = ''
            #mkdir -p $out/bin
            #cp -r bin/wavm $out/bin/
            #cp -r include/ $out
            #cp -r lib64/ $out
            #cp -r share/ $out
			##install -m755 -D studio-link-standalone-v${version} $out/bin/studio-link
		  #'';

		  #meta = with pkgs.lib; {
			#homepage = "https://wavm.github.io/";
			#description = "WAVM WebAssembly Engine";
			#platforms = platforms.linux;
		  #};
		#};
      in {
        devShell = pkgs_old.mkShell {
          buildInputs = with pkgs_old; [
            which # used for shell stuff when inside pure env

            hyperfine
            graph-cli # is this just for python?!

            chicken gambit-unstable chez
            #gambit

            wabt wasmtime wavm
            #wasm3

            clang cmake
            (rust-bin.stable.latest.default.override { targets = [ "wasm32-wasi" ]; })
            #stack (haskellPackages.ghcWithPackages (p: [p.parallel]))
            koka
            pkgs_old.emscripten

            #picolisp
            #newlisp
          ];
        };
      }
    ));
}
