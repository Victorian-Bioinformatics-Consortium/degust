#!/bin/sh

dest=dist-dev

(cd coffee ; coffee -c .)

mkdir -p "$dest"
cd "$dest"

ln -sf ../src/r-json.hs r-json.cgi

for f in ../html/* ../src/* ../kegg; do ln -sf $f .; done

mkdir js
for f in ../coffee/*.js; do ln -sf $f js; done

mkdir -p tmp
mkdir -p cached
mkdir -p user-files

echo "Linked files to '$dest'.  Now run ./server.py"

