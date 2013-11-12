#!/bin/sh

set -e

dest=build

case "$1" in
    dev|prod|prod-server)
        ;;
    *)
        echo "Build to $dest"
        echo "Usage: ./build.sh dev|prod|prod-server"
        exit 1
        ;;
esac

mkdir -p "$dest"
mkdir -p "$dest"/css

case "$1" in
    dev)
        echo "Building 'dev'"
        echo "Linking in backend files for 'dev' deploy"
        (cd "$dest" ; for f in ../app/backend/*.hs ; do ln -s $f .; done )
        (cd "$dest" ; ln -s r-json.hs r-json.cgi)
        mkdir -p "$dest"/tmp "$dest"/user-files "$dest"/cached

        # Combine the lib CSS
        cat app/css/lib/*.css > "$dest"/css/lib.css

        # Link our CSS
        (cd "$dest"/css ; for f in ../../app/css/*.css ; do ln -s $f .; done )

        # Link the HTML
        (cd "$dest" ; for f in ../app/html/* ; do ln -s $f .; done )
        ;;
    *)
        echo "Building 'production'"
        echo "Combining css and minifying..."
        # Combine the lib CSS
        cat app/css/lib/*.css | cleancss > "$dest"/css/lib.css
        # Minify our CSS
        for f in app/css/*.css; do
            cat "$f" | cleancss > "$dest"/css/`basename "$f"`
        done

        cp -r app/html/ "$dest"
        ;;
esac

cp -r app/css/lib/images "$dest"/css/
cp -r app/images "$dest"

echo "Compiling CoffeeScript and bundling all js..."
for f in app/js/*-req.coffee; do
    t="$dest/"$(basename "${f%-req.coffee}")".js"
    echo "Building $f -> $t"

    case "$1" in
        dev)
            browserify --debug -t coffeeify -t hbsfy $f > $t
            ;;
        *)
            b=${t/.js/-big.js}
            browserify -t coffeeify -t hbsfy $f > $b
            uglifyjs $b > $t
            rm $b
            ;;
    esac
done


case "$1" in
    prod-server)
        # Build the backend
        (cd app/backend ; ghc -O2 --make r-json)
        cp app/backend/r-json "$dest"/r-json.cgi

        # Copy production server specific files
        cp -r kegg "$dest"
        cp htaccess "$dest"/.htaccess
        mkdir -p "$dest"/tmp "$dest"/cached "$dest"/user-files
        ;;
esac
        

case "$1" in
    dev)
        echo "Dev build ready.  Now run (cd $dest ; ../server.py)"
        ;;
    prod-server)
        echo "Production server build ready in $dest/"
        echo "Ensure '$dest/{tmp,cached,user-files}' is writable by the web server user"
        ;;
    *)
        echo "Production build ready in $dest/"
        ;;
esac
