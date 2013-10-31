#!/bin/sh

set -e

rm -rf build
mkdir build

#cp -r css/images build
#cp index.html build

#echo "Combining css and minifying..."
#cat css/bootstrap-tour.min.css css/dge.css css/venn.css css/slick.grid.css | cleancss > build/main.min.css

echo "Compiling CoffeeScript and bundling all js..."
for f in app/js/*-req.coffee; do
  t="build/"$(basename "${f%-req.coffee}")".js"
  b=${t/.js/-big.js}
  echo "Building $f -> $t"

  browserify -t coffeeify -t hbsfy $f > $b
  uglifyjs $b > $t
  rm $b
done
