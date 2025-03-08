#!/bin/bash  

FILE=$1

quarto publish quarto-pub "$FILE" --no-browser

# will need to comment out these rm if using multiplex
# also added to gitignore
rm -r *_files
rm *.html