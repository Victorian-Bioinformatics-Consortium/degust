Compiling the degust backend can be tricky, particularly getting the correct
haskell libraries installed.  The easiest approach is using a cabal sandbox.
Here is a sequence of commands known to successfully build degust on an
ubuntu-14.04 clean install.

These instructions were tested on a vagrant machine.  Provision an ubuntu 14.04 vagrant box

    vagrant init ubuntu-14.04
    vagrant ssh


Within the new vagrant box, install prerequisites

    sudo apt-get update
    sudo apt-get install -y git libz-dev libpcre3-dev npm nodejs-legacy cabal-install r-bioc-limma r-bioc-edger
    cabal update
    cabal install cabal-install

Check out degust, and install haskell and nodejs libraries

    git clone https://github.com/Victorian-Bioinformatics-Consortium/degust.git
    cd degust
    ~/.cabal/bin/cabal sandbox init
    ~/.cabal/bin/cabal install --only-dep --force-reinstall
    npm install browserify clean-css hbsfy@1.3 handlebars-runtime uglify-js coffeeify

Build and run the server

    PATH=./node_modules/.bin:~/.cabal/bin:$PATH bash ./build.sh prod-server
    (cd build ; ../server.py)
