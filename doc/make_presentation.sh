#!/usr/bin/env bash
rm presentation.aux
rm presentation.bbl
rm presentation.blg
rm presentation.log
rm presentation.out
rm presentation.pdf
#pdflatex presentation && bibtex presentation && pdflatex presentation && bibtex presentation && pdflatex presentation && bibtex presentation && evince presentation.pdf
pdflatex presentation && evince presentation.pdf
