#!/usr/bin/env bash
rm writeup.aux
rm writeup.bbl
rm writeup.blg
rm writeup.log
rm writeup.out
rm writeup.pdf
pdflatex writeup && bibtex writeup && pdflatex writeup && bibtex writeup && pdflatex writeup && bibtex writeup && evince writeup.pdf
