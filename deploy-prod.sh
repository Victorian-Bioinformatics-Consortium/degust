#!/bin/sh

dest=$1
echo "dest=$dest"
if [ -z "$dest" ]; then
  echo "Usage: $0 <destination directory>"
  exit 1;
elif [ ! -d "$dest" ]; then
  echo "ERROR: Destination directory must exist"
  exit 1;
fi

echo "Installing DGE-Vis to : $dest"

(cd coffee ; coffee -c .)

(cd src/ ; ghc -O2 --make r-json)
cp src/r-json "$dest"/r-json.cgi

for f in html/* coffee/*.js kegg; do cp -r "$f" "$dest"; done
mkdir "$dest/js"
for f in coffee/*.js; do cp -r "$f" "$dest/js"; done


cp htaccess "$dest"/.htaccess

mkdir -p "$dest"/tmp
mkdir -p "$dest"/cached
mkdir -p "$dest"/user-files

