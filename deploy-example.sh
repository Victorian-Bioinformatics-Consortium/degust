#!/bin/sh

dest=dist-example

(cd coffee ; coffee -c .)

mkdir -p "$dest"
cd "$dest"

for f in ../html/{css,lib,images} ../kegg; do ln -sf $f .; done

mkdir -p js
for f in ../coffee/*.js; do ln -sf "../$f" js; done

sed -e 's/##SETTINGS##/settings.js/' ../html/compare.html > index.html
cp -f ../examples/settings.js settings.js

echo "Linked files to '$dest'.  Now run (cd $dest; python -mSimpleHTTPServer 8040)"

