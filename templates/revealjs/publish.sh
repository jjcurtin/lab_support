#!/bin/bash  

# ./publish.sh FILE
FILE=$1

quarto publish quarto-pub "$FILE" --no-browser
rm -r *_files
rm *.html
