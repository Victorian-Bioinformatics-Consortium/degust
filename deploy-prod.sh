#!/bin/sh

set -e

dir=$1

if [ -z "$dir" ]; then
  echo "Usage: $0 <deploy directory>"
  exit 1
fi

if [ ! \( -d "$dir" -a -w "$dir" \) ]; then
  echo "Deploy directory '$dir' must exist and by writable by you"
  exit 1
fi
  

export PATH=./node_modules/.bin/:$PATH

./build.sh prod-server

cp -r build/* "$dir"

echo "Ensure '$dir/{tmp,cached,user-files}' is writable by the web server user"

