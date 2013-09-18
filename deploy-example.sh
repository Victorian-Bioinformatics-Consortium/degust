#!/bin/sh

src=$1
dest=dist-example

coffee -o html/js -c coffee

mkdir -p "$dest"
cd "$dest"

for f in ../html/{css,lib,images,js} ../kegg; do ln -sf $f .; done

sed -e 's/##SETTINGS##/settings.js/' ../html/compare.html > index.html
cp -f ../examples/${src}settings.js settings.js
cp -f ../examples/${src}example.csv .

echo "Linked files to '$dest'.  Now run (cd $dest; python -mSimpleHTTPServer 8040)"
echo "For active development want: coffee -o html/js -c -w coffee"

