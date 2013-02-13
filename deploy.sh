#!/bin/sh

mkdir -p dist
cd dist

ln -sf ../src/r-json.hs r-json.cgi
for f in ../web/* ../src/*; do ln -sf $f .; done

mkdir -p tmp
mkdir -p cached
mkdir -p user-files

