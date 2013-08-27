Kraken
======

The Kraken Programming Language

The Kraken Programming Language is in its infancy.
Currently, it consists of a RNGLALR parser written in C++ and a very experimental grammer that is evolving quickly.
When compiled, the kraken program (as it is not yet a compiler) will take in a text file to be parsed, the grammer file to use, and a filename to output a DOT file to.
Kraken will then generate the RN parsing tables from the grammer and then parse the input and export a DOT file that can be renderd into a graph using Graphviz.

It has the following design goals:
	-Compiled
	-Clean
	-Fast (both running and writing)
	-Good for Systems (including Operating Systems) programming
	-Minimal "magic" code. (no runtime, other libraries automatically included)

It is inspired by C/C++, Python, and Go.
