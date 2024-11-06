#!/bin/bash  

quarto publish quarto-pub demo.qmd --no-browser
rm -r *_files
rm *.html
