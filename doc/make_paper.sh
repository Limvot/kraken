#!/usr/bin/env bash
touch writeup.pdf && rm  writeup.aux writeup.bbl writeup.blg writeup.log writeup.out writeup.pdf && pdflatex writeup && bibtex writeup && pdflatex writeup && bibtex writeup && pdflatex writeup && bibtex writeup && evince writeup.pdf
