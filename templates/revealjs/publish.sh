#!/bin/bash  

quarto publish quarto-pub slides.qmd --no-browser
rm -r *_files
rm *.html
