
with import <nixpkgs> { };

mkShell {
  LANG="en_US.UTF-8";
  nativeBuildInputs = [
    emscripten
    nodejs
    valgrind
    kcachegrind
  ];
}
