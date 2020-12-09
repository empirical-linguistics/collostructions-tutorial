#!/bin/bash

cp -R docs/docs/fig/ docs/fig
mv 00-index-html.Rmd docs
mv 00-index-latex.Rmd index.Rmd
Rscript build_book_latex.R
mv index.Rmd 00-index-latex.Rmd
mv docs/00-index-html.Rmd .
mv 00-index-latex.Rmd docs
mv 00-index-html.Rmd index.Rmd
Rscript build_book_html.R
mv index.Rmd 00-index-html.Rmd
mv docs/00-index-latex.Rmd .
rm -R docs/fig