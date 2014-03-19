#!/bin/sh

# Exit if any command fails
set -e

degust_home='http://victorian-bioinformatics-consortium.github.io/degust'
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
./build-embed.sh remote

echo "Changing repo to gh-pages"
git checkout gh-pages

echo "Copying to dist/$ver"

mkdir -p dist/$ver
cp -r build/{compare.html,*.js,css,images,degust.py} dist/$ver
sed -e "s|'\./|'$url|" \
    -e s"|index\.html|$degust_home|" build/compare.html > dist/$ver/index.html

(  cd dist
   rm -rf latest
   cp -r $ver latest
)


echo "Now commit the changes and switch back:"
echo "    git add dist/$ver dist/latest"
echo "    git commit -a"
echo "    git checkout master"
