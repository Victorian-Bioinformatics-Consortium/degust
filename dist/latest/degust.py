#!/usr/bin/env python

import argparse
import json
import re
import sys
import csv


def embed(csv, args):
    html="""
<html>
  <head profile="http://www.w3.org/2005/10/profile">
    <link rel="icon" type="image/png" href="images/favicon.png"/>

    <!-- Externals CSS -->
    <link rel="stylesheet" href='http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/css/lib.css' />

    <link rel="stylesheet" href='http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/css/common.css' type="text/css" />
    <link rel="stylesheet" href='http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/css/compare.css' type="text/css "/>

    <script type="text/javascript" src='http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/common.js'></script>
    <script type="text/javascript" src='http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/slickgrid.js'></script>
    <script type="text/javascript" src='http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/compare.js'></script>
  </head>
  <body>
    <div id="replace-me" class="container">
      <div class="jumbotron">
        <h1>Degust</h1>
        <p><a href='http://victorian-bioinformatics-consortium.github.io/degust/'>Degust</a> is preparing your data...  prepare for degustation...</p>
        <img src='http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/images/front-loader.gif'>
      </div>
    </div>

    <script type="text/javascript">
      window.settings = { };
    </script>
  </body>
</html>

         """
    enc = json.dumps(csv)
    columns = \
      ["{idx:%s, name: 'FDR', type: 'fdr'}"%json.dumps(args.fdr)] + \
      ["{idx:%s, name: 'Average', type: 'avg'}"%json.dumps(args.avg)] + \
      ["{idx:%s, name: %s, type: 'primary'}"%(json.dumps(args.primary), json.dumps(args.primary))] + \
      ["{idx:%s, name: %s, type:'fc'}"%(json.dumps(c),json.dumps(c)) for c in args.logFC] + \
      ["{idx:%s, name: %s, type:'info'}"%(json.dumps(c),json.dumps(c)) for c in args.info]

    settings = ["html_version: '0.8'",
                "asset_base: 'http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/'",
                "csv_data: data", 
                "csv_format: true",
                "name: %s"%json.dumps(args.name),
                "columns:[%s]"%(",".join(columns)),
                ]
    if args.notour:
        settings += ["show_tour: false"]

    window_settings = "window.settings = {%s};"%(",".join(settings))
    s = html.replace('window.settings = { };', "var data=%s;\n\n%s"%(enc,window_settings), 1)
    return s

def check_args(args, csv_file):
    # Check args match csv file.
    reader = csv.reader(csv_file.split('\n'), delimiter=',')
    headers = reader.next()
    err = False
    if args.avg is None:
        sys.stderr.write("Column for average expression not defined (use --avg) necessary for the ma-plot\n")
        err=True
     
    if args.avg not in headers:
        sys.stderr.write("Column for average expression not found (%s)\n"%args.avg)
        err=True
     
    if args.fdr not in headers:
        sys.stderr.write("Column for FDR not found (%s)\n"%args.fdr)
        err=True

    if args.logFC is None:
        sys.stderr.write("ERROR: No columns defined for log-fold-change, --logFC\n")
        err=True
    else:
        for f in args.logFC:
            if f not in headers:
                sys.stderr.write("ERROR: Column for logFC not found, --logFC : (%s)\n"%f)
                err=True
     
    if args.info is None:
        sys.stderr.write("ERROR: No columns defined for per-gene information, eg. gene IDs (use --info)\n")
        err=True
    else:
        for f in args.info:
            if f not in headers:
                sys.stderr.write("ERROR: Column for info not found (%s)\n"%f)
                err=True
    return err

parser = argparse.ArgumentParser(description='Produce a standalone Degust html file from a CSV file containing DGE.')
parser.add_argument('csvfile', type=argparse.FileType('r'), 
                    nargs='?', default='-', 
                    help="CSV file to process (default stdin)")
parser.add_argument('-o','--out', type=argparse.FileType('w'), 
                    default='-', 
                    help="Output file (default stdout)")

parser.add_argument('--name', default='Unnamed', 
                    help='Name for this DGE comparison')
parser.add_argument('--notour',  
                    help='Do not show the tour on first load')
parser.add_argument('--primary', default='pri', 
                    help='Name for the primary condition that the fold-changes are relative to')
parser.add_argument('--avg',
                    help='Name for average intensity column in CSV file')
parser.add_argument('--fdr', default='adj.P.Val', 
                    help='Name for "FDR" column in CSV file (default "adj.P.Val")')
parser.add_argument('--logFC',
                    help='Comma separated names for "logFC" columns in CSV file')
parser.add_argument('--info',
                    help='Comma separated names for info columns in CSV file')

args = parser.parse_args()

#print args
if args.info:  args.info = args.info.split(",")
if args.logFC: args.logFC = args.logFC.split(",")

if args.csvfile == sys.stdin:
    sys.stderr.write("Reading from stdin...\n")

csv_file = args.csvfile.read()

err = check_args(args, csv_file)

if not err:
    args.out.write(embed(csv_file, args))

