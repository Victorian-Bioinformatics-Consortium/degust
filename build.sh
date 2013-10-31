#!/bin/sh

set -e
debug=1

mkdir -p build
rm -rf build/*

cp -r app/html/ build
cp -r app/images build

#echo "Combining css and minifying..."
#cat css/bootstrap-tour.min.css css/dge.css css/venn.css css/slick.grid.css | cleancss > build/main.min.css
cp -r app/css build


echo "Compiling CoffeeScript and bundling all js..."
for f in app/js/*-req.coffee; do
  t="build/"$(basename "${f%-req.coffee}")".js"
  b=${t/.js/-big.js}
  echo "Building $f -> $t"

  if [ $debug ]; then
      browserify --debug -t coffeeify -t hbsfy $f > $t
  else
      browserify -t coffeeify -t hbsfy $f > $b
      uglifyjs $b > $t
      cp $b > $t
      rm $b
  fi
done
