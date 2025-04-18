#!/bin/bash  
# ./publish.sh FILE

# FILE = filename
FILE=$1

echo ""
echo "Publishing $FILE to quarto-pub"
echo ""

cp _quarto_uw.yml _quarto.yml
quarto publish quarto-pub "$FILE" --no-browser

rm _quarto.yml
# will need to comment out these next 2 rm if using multiplex
rm -r *_files
rm *.html
