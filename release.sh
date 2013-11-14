#!/bin/sh

# Exit if any command fails
set -e

url='http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/'

ver=$(grep ver app/js/version.coffee | sed -e 's/^.* = //')
ver="${ver//\'}"

echo "Building v$ver.  Using URL $url"

if [ -z "$ver" ]; then
  echo "Version not found!"
  exit 1
fi

echo "Minifying..."
./build.sh prod

echo "Changing repo to gh-pages"
git checkout gh-pages

echo "Copying to dist/$ver"

mkdir -p dist/$ver
cp -r build/{compare.html,*.js,css,images} dist/$ver
sed -e "s|'\./|'$url|" build/compare.html > dist/$ver/index.html

sed -e "/HTML-HERE/r dist/$ver/index.html" -e '/HTML-HERE/d' -e "s/VERSION-HERE/$ver/g" build/embed.py > dist/$ver/degust.py

(  cd dist
   rm -f latest
   ln -sf $ver latest
)


echo "Now commit the changes and switch back:"
echo "    git add dist/$ver dist/latest"
echo "    git commit -a"
echo "    git checkout master"
