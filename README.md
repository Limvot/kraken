Kraken
======

The Kraken Programming Language

(try it out online at http://www.kraken-lang.org/)

(vim integration (filetype, syntax highlighting, Syntastic) at https://github.com/Limvot/kraken.vim)

(more info and examples at http://limvot.github.io/kraken/)

The Kraken Programming Language is functional but very much still in development.
It has both the normal features you might expect of a modern language, (functions, variables, an object system, dynamic memory), as well as some more advanced ones (mutually recursive definitions, lambdas/closures, algebraic data types, templates, marker traits, defer statements, etc).

Currently, it consists of a RNGLALR parser written in C++, an AST intermediate representation, and a C code generator.
When compiled, the kraken compiler will take in a text file to be parsed and optionally the grammer file to use and an output file name.
Kraken will then generate the RNLALR parsing tables from the grammer or load them from a binary file if Kraken has been run with this exact version of the grammer before. Then it will parse the input and export one .c file along with a .sh script containing the compiler command to compile the C files together into a binary.

The current work is going into self-hosting the compiler, which is progressing quickly. Currently, the self-hosted version can compile simple programs consisting of functions, variables, normal operators, if statements, while loops, and very simple C-passthroughs.

It is invoked in this way:

kraken inputKrakenFile

OR

kraken inputKrakenFile inputGrammerFile outputName


Dependencies
============

It is built using CMake, which is also its only dependency.

Goals
=====

It has the following design goals:
*	Compiled
*	Clean
*	Fast (both running and writing)
*	Good for Systems (including Operating Systems) programming
*	Minimal "magic" code. (no runtime, other libraries automatically included)

It is inspired by C/C++, Scala, and Rust.
