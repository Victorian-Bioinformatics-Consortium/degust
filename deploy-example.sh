#!/bin/sh

dest=dist-example

coffee -o html/js -c coffee

mkdir -p "$dest"
cd "$dest"

for f in ../html/{css,lib,images,js} ../kegg; do ln -sf $f .; done

sed -e 's/##SETTINGS##/settings.js/' ../html/compare.html > index.html
cp -f ../examples/settings.js settings.js

echo "Linked files to '$dest'.  Now run (cd $dest; python -mSimpleHTTPServer 8040)"

