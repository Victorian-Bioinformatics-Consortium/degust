#!/bin/sh

(cd coffee ; coffee -c .)

mkdir -p dist
cd dist

ln -sf ../src/r-json.hs r-json.cgi

for f in ../html/* ../src/* ../coffee/*.js ../kegg; do ln -sf $f .; done

mkdir -p tmp
mkdir -p cached
mkdir -p user-files

