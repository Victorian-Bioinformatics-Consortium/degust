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
mkdir -p "$dest"/public/css

case "$1" in
    dev)
        echo "Building 'dev'"
        echo "Linking in backend files for 'dev' deploy"
        (cd "$dest" ;
            for f in ../app/backend/*.rb ; do
                rm -f `basename "$f"`
                ln -s $f .
            done
            ln -s ../app/backend/r-templates .
        )
        mkdir -p "$dest"/tmp "$dest"/user-files "$dest"/cached

        echo "Linking in CSS and HTML"
        # Combine the lib CSS
        cat app/css/lib/*.css > "$dest"/public/css/lib.css

        # Link our CSS
        (cd "$dest"/public/css ;
            for f in ../../../app/css/*.css ; do
                rm -f `basename "$f"`
                ln -s "$f" .
            done )

        # Link the HTML
        (cd "$dest"/public ;
            for f in ../../app/html/* ; do
                rm -f `basename "$f"`
                ln -s "$f" .
            done )
        ;;
    prod|prod-server)
        echo "Building '$1'"
        echo "Combining css and minifying..."
        # Combine the lib CSS
        cat app/css/lib/*.css | cleancss > "$dest"/public/css/lib.css
        # Minify our CSS
        for f in app/css/*.css; do
            t="$dest"/public/css/`basename "$f"`
            rm -f "$t"
            cat "$f" | cleancss > "$t"
        done

        rm -f "$dest"/public/*.html
        cp -r app/html/* "$dest"/public
        ;;
    *)
        echo "This did not happen..."
        exit 1
esac

cp -r app/css/lib/images "$dest"/public/css/
cp -r app/images "$dest"/public

echo "Compiling CoffeeScript and bundling all js..."
for f in app/js/*-req.coffee; do
    t="$dest/public/"$(basename "${f%-req.coffee}")".js"
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
    dev)
        (cd "$dest"
            rm -f kegg
            ln -s ../kegg .
        )
        ;;
    prod-server)
        echo "Copying backend"
        cp app/backend/*.rb "$dest"/
        cp -r app/backend/r-templates "$dest"

        # Copy production server specific files
        cp -r kegg "$dest"
        cp htaccess-main "$dest"/.htaccess
        for f in tmp cached user-files; do
            dir = "$dest/$f"
            mkdir -p "$dir"
            chmod o= "$dir"
            cp htaccess-deny "$dir"/.htaccess
        done
        ;;
esac


case "$1" in
    dev)
        echo "Dev build ready.  Now run (cd $dest ; rerun -b -d ../app 'ruby ./degust-serve.rb')"
        ;;
    prod-server)
        echo "Production server build ready in $dest/"
        echo "Ensure '$dest/{tmp,cached,user-files}' is writable by the web server user"
        ;;
    *)
        echo "Production build ready in $dest/"
        ;;
esac
