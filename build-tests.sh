#!/bin/sh

dest=tests/js/js-build

for f in app/js/*-req.coffee; do
    t="$dest/"$(basename "${f%-req.coffee}")".js"
    echo "Building $f -> $t"
    browserify --debug -t coffeeify -t hbsfy $f > $t
done

echo "Building tests"
coffee -c -o tests/js/js-build tests/js
