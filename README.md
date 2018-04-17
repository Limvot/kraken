Kraken
======

The Kraken Programming Language

(try it out online at http://www.kraken-lang.org/)

(vim integration (filetype, syntax highlighting, Syntastic) at https://github.com/Limvot/kraken.vim)
(emacs integration (filetype, syntax highlighting) at https://github.com/Limvot/kraken-mode)

The Kraken Programming Language is functional but very much still in development.
It has both the normal features you might expect of a modern language, (functions, variables, an object system, dynamic memory), as well as some more advanced ones (mutually recursive definitions, lambdas/closures, algebraic data types, templates, marker traits, defer statements, etc).

Kraken can either compile to C or its own bytecode which is then interpreted.

Dependencies
============

Kraken is self-hosted - in order to build it, a script is included that will compile the original C++ version (which depends on CMake) and then checks out each necessary version to compile up to the current one. This can take quite a while - when it hits 1.0 I am planning on removing the old C++ version and checking in a pre-compiled-to-c version to use for further bootstrapping.

Goals
=====

It has the following design goals:
*	Compiled
*	Clean
*	Fast (both running and writing)
*	Good for Systems (including Operating Systems) programming
* Very powerful libraries (say, a library that allows you import from automatically parsed C header files)
*	Minimal "magic" code. (no runtime, other libraries automatically included)

It is inspired by C, Kotlin, Rust, and Jai.
