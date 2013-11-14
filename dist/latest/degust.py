#!/usr/bin/env python

import argparse
import json
import re
import sys


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
    <div class="navbar navbar-inverse navbar-fixed-top">
      <div class="container">
        <div class="navbar-header">
          <a class="navbar-brand" href="index.html">Degust : </a>
          <span class="navbar-brand exp-name"></span>
        </div>
        <ul class="nav navbar-nav navbar-right">
          <li><a class="log-link" href="#">Logs</a>
          <li><a class="config hide" href="#">Configure</a>
          <li><a href="#about-modal" data-toggle="modal">About</a>
        </ul>
      </div>
    </div>

    <div class='log-list'>
      <h4>Log messages</h4>
    </div>
    
    <div id='main-loading'><img src='http://victorian-bioinformatics-consortium.github.io/degust/dist/latest/images/ajax-loader.gif'></div>


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
      ["{idx:%s, name:%s, type:'fc'}"%(json.dumps(c),json.dumps(c)) for c in args.logFC] + \
      ["{idx:%s, name:%s, type:'info'}"%(json.dumps(c),json.dumps(c)) for c in args.info]
      
    settings = ("window.settings = {html_version: '0.8', csv_data: data, csv_format: true, name: %s,"
                "  columns:[%s]};")%(json.dumps(args.name), ",".join(columns))
    s = html.replace('window.settings = { };', "var data=%s;\n\n%s"%(enc,settings), 1)
    return s



parser = argparse.ArgumentParser(description='Produce a standalone Degust html file from a CSV file containing DGE.')
parser.add_argument('csvfile', type=argparse.FileType('r'), 
                    nargs='?', default='-', 
                    help="CSV file to process (default stdin)")
parser.add_argument('-o','--out', type=argparse.FileType('w'), 
                    default='-', 
                    help="Output file (default stdout)")

parser.add_argument('--name', default='Unnamed', 
                    help='Name for this DGE comparison')
parser.add_argument('--primary', default='pri', 
                    help='Name for the primary condition that the fold-changes are relative to')
parser.add_argument('--avg',
                    help='Name for average intensity column in CSV file')
parser.add_argument('--fdr', default='adj.P.Val', 
                    help='Name for "FDR" column in CSV file (default "adj.P.Val")')
parser.add_argument('--logFC', default='logFC', nargs='*',
                    help='Names for "logFC" columns in CSV file')
parser.add_argument('--info', default=['Feature'], nargs='*',
                    help='Names for info columns in CSV file - accepts multiple strings')

args = parser.parse_args()

#print args

if args.csvfile == sys.stdin:
    sys.stderr.write("Reading from stdin...\n")

csv = args.csvfile.read()

args.out.write(embed(csv, args))

