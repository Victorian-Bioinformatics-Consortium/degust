#!/bin/sh

dest=build

for f in app/js/*-req.coffee; do
    t="$dest/public/"$(basename "${f%-req.coffee}")".js"
    echo "Watching $f -> $t"
    watchify --debug -t coffeeify -t hbsfy $f -o $t -v &
done

wait
