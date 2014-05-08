#!/bin/sh

# Exit if any command fails
set -e

degust_home='http://victorian-bioinformatics-consortium.github.io/degust'

case "$1" in
    local)
        url='./'
        echo "Building LOCAL, ensure you run ./build.sh dev"
        echo "You will need build/css/*  and build/js/*.js"
        ;;
    local-srv)
        url='http://localhost:8000/'
        echo "Building LOCAL, ensure you run ./build.sh dev"
        ;;
    remote)
        url='http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/'
        ;;
    *)
      echo "Usage: ./build-embed.sh local|local-srv|remote"
      exit 1
      ;;
esac

ver=$(grep ver app/js/version.coffee | sed -e 's/^.* = //')
ver="${ver//\'}"

echo "Building v$ver.  Using URL $url"

if [ -z "$ver" ]; then
  echo "Version not found!"
  exit 1
fi

sed -e "s|'\./|'$url|" \
    -e s"|index\.html|$degust_home|" app/html/compare.html > xx.html



sed -e "/HTML-HERE/r xx.html" \
    -e '/HTML-HERE/d' \
    -e "s/VERSION-HERE/$ver/g" \
    -e "s^ASSET-HERE^$url^g"  app/scripts/embed.py > build/degust.py

rm -f xx.html

echo "Built: build/degust.py"
