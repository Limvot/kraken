Kraken
======

The Kraken Programming Language

(more info and examples at http://limvot.github.io/kraken/)

The Kraken Programming Language is in its infancy.
Currently, it consists of a RNGLALR parser written in C++, an experimental grammer that is evolving, and a C code generator.
When compiled, the kraken compiler will take in a text file to be parsed, the grammer file to use, and an output file name.
Kraken will then generate the RN parsing tables from the grammer OR load them from a binary file if Kraken has been run with this exact version of the grammer before. Then it will parse the input and export DOT files for every .krak file in the project (these can be renderd into a graph using Graphviz), a C file for every file in the project, and a .sh script containing the compiler command to compile the C files together into a binary.

It is invoked in this way:
kraken inputTextFile inputGrammerFile outputName

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

It is inspired by C/C++, Python, and Go.
