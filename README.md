# Degust (formerly known as DGE-Vis)

* Visualise RNA-seq differential expression data.
* Perform your own DGE analysis, or use the inbuilt server to analyse from your own "counts" file.

Access a public web service running [Degust](http://www.vicbioinformatics.com/degust).

View a [short video](https://www.youtube.com/watch?v=ucucQ_LtZ1g) of the interface in use.

Read a summary on the <a href='http://victorian-bioinformatics-consortium.github.io/degust/'>Degust home page</a>.

## Example Screenshot

![Degust screenshot](screenshot-2.png)

# FAQ

### How can a gene have zero counts for all samples but has a non-zero fold-change?

This can happen when using the backend of Degust.  Degust uses voom to perform the expression analysis.  Voom adds a small constant (0.5), to each count, normalizes for library size, then takes the log.  This means when you have a count of 0 across all samples, but different library sizes, it is possible to compute a non-zero fold-change.

We recommend setting **Min read count** on the configuration page to a small value, say 10.

### What is the **Min read count** setting?

This is the minimum number of reads required in at least one sample to keep the gene in the analysis.  That is, a given gene is omitted if the number of reads across all samples is below this setting.

### Chrome dies with an "Aw, Snap" error when trying to download the table

This appears to be a problem with Chrome when there are many (thousands) of genes in the table.  We suggest using Firefox if this happens.

### I don't see an option to display an MDS plot

The MDS plot is only available when you have included "count" columns

### How is the MDS plot calculated

  * First genes that don't pass the "FDR cut-off" or "abs log FC" filters are ignored.  Using these filters is "cheating" when doing an MDS plot to look at replicate clustering
  * The remaining genes have the counts for each replicate log-transformed.
  * The genes are then ranked by decreasing variance.  That is, the most variable genes are "at the top"
  * Then the top "Skip genes" are ignored.
  * And the next "Num genes" are selected.
  * These selected genes are used to compute an MDS (or PCA) plot

# Installation

If you do not want to use the [public Degust installation](http://www.vicbioinformatics.com/degust), you may install your own.

You first need to grab a copy of Degust.

        git clone git@github.com:Victorian-Bioinformatics-Consortium/degust

Degust can be installed in two ways:

  1. Perform your own DGE analysis, and use only the [web frontend from Degust](#frontend-installation-only)
  2. Install the [frontend and back-end software](#full-installation) to perform analysis and visualise the results.

## Frontend installation only

To use the frontend visualisation, you will need to have done your own DGE analysis with a tool like edgeR or voom.  You will need CSV file contain a line per gene, and the following columns:

  * ID - containing a unique identifier for each gene (required)
  * Adjusted p-value - The adjusted p-value (FDR or similar) for that gene (required)
  * Log intensity for each condition - Used to compute the log fold-change (required)
  * Average intensity across the conditions - Used for the MA-plot (required)
  * Gene info - Arbitrary information columns to display in the gene list table (optional)
  * Read counts - Read counts for each replicate, only used for display purposes (optional)

The simplest approach is to download [degust.py](http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/degust.py) then run it with your csv file as a parameter.  This will create a single HTML page that you view or share.  Run ``degust.py --help` to find the parameters to specify the column names for your CSV.

## Full installation

## Run tests locally

There are some javascript tests which can be run locally (or with travis).  Build the js:

    ./build-tests.sh

Then you can either run the tests in your browser (navigate to http://localhost:8000/)

    (cd tests/js/ ; python -mSimpleHTTPServer)

Or, if you have <a href='http://phantomjs.org/'>phantomjs</a> installed you can run the tests from the command line: `./test-js.sh`

## Contributing ##
Feel free to contribute with pull requests, bug reports or enhancement suggestions.

## Development

### To build
For building from sources, you will need nodejs and the following modules.

    npm install -g browserify
    npm install -g clean-css
    npm install hbsfy
    npm install handlebars-runtime
    npm install uglify-js
    npm install coffeeify

    # Builds files into build/ for deployment
    ./build.sh prod

### For development
This will watch the js & coffeescript files and rebuild CoffeeScript as needed.

    npm install -g watchify

    ./build.sh dev
    ./build-watchify.sh &
    (cd build ; ../server.py)

To build the degust.py script for embedding csv into an html file for local

    ./build-embed.sh local

It is also useful to access the pages with "debug=1" (eg. http://vicbioinformatics.com/degust/compare.html?code=example&debug=1) which enables extra debug logging to the console.


### To build with the analysis back-end
The above production build only includes the front-end.  To also build the back-end you can use the following.  (The haskell library requirements are not well documented yet.)

    ./build.sh prod-server

Requirements:

  * Python
  * CoffeeScript
  * R and the following libraries
    * limma
    * edgeR
  * GHC 6.12 or later, and the following libraries:
    * pureMD5 >= 2.1
    * json >= 0.7
    * regex-pcre >= 0.94
    * hamlet >= 1.1
    * shakespeare-text >= 1.1
    * strict-io >= 0.2
    * lens >= 3.9

The resulting build/ directory can then be installed as a CGI site.

#### Troubleshooting

  * The directories "tmp/", "cached/" and "user-files/" under the CGI directory must be writable by the web-server user
  * Any runtime errors relating to R will be logged in the directory "tmp/" under the CGI directory
  * If your R libraries are not installed in the default location, you may need to edit r-json.hs and modify the setting for R_LIBS_SITE

## Known Issues

#### Heatmap 

  * Clustering algorithm is naive greedy N^2.  So, not fast, and not a great clustering. 

#### Documentation

  * Installing the full back-end is barely documented

## License ##
Degust is released under the GPL v3 (or later) license, see <a href='http://github.com/Victorian-Bioinformatics-Consortium/degust/blob/master/COPYING.txt'>COPYING.txt</a>
