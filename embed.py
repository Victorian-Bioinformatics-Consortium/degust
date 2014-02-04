#!/usr/bin/env python

import argparse
import json
import re
import sys
import csv


def embed(csv, args):
    html="""
           HTML-HERE
         """
    enc = json.dumps(csv)
    columns = \
      ["{idx:%s, name: %s, type:'info'}"%(json.dumps(c),json.dumps(c)) for c in args.info] + \
      ["{idx:%s, name: 'FDR', type: 'fdr'}"%json.dumps(args.fdr)] + \
      ["{idx:%s, name: 'Average', type: 'avg'}"%json.dumps(args.avg)] + \
      ["{idx:%s, name: %s, type: 'primary'}"%(json.dumps(args.primary), json.dumps(args.primary))] + \
      ["{idx:%s, name: %s, type:'fc'}"%(json.dumps(c),json.dumps(c)) for c in args.logFC] + \
      ["{idx:%s, name: %s, type:'link'}"%(json.dumps(c),json.dumps(c)) for c in args.link_col]

    settings = ["html_version: 'VERSION-HERE'",
                "asset_base: 'ASSET-HERE'",
                "csv_data: data", 
                "csv_format: %s"%("false" if args.tab else "true"),
                "name: %s"%json.dumps(args.name),
                "columns:[%s]"%(",".join(columns)),
                ]
    if args.notour:
        settings += ["show_tour: false"]
    if args.link_url:
        settings += ["link_url: %s"%json.dumps(args.link_url)]

    window_settings = "window.settings = {%s};"%(",".join(settings))
    s = html.replace('window.settings = { };', "var data=%s;\n\n%s"%(enc,window_settings), 1)
    return s

def check_args(args, csv_file):
    # Check args match csv file.
    delim = "\t" if args.tab else ","
    reader = csv.reader(csv_file.split('\n'), delimiter=delim)
    headers = reader.next()
    err = False
    if args.avg is None:
        sys.stderr.write("ERROR: Column for average expression not defined (use --avg) necessary for the ma-plot\n")
        err=True
    elif args.avg not in headers:
        sys.stderr.write("ERROR: Column for average expression not found (%s)\n"%args.avg)
        err=True
     
    if args.fdr not in headers:
        sys.stderr.write("ERROR: Column for FDR not found (%s)\n"%args.fdr)
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
parser.add_argument('--link-col',
                    help='Name for column to use with "--link-url"')
parser.add_argument('--link-url',
                    help='Gene info URL.  Used when double-clicking the gene-table.  Any "%%s" will be replaced with the value from the specified "--link-col"')
parser.add_argument('--tab', action='store_true', default=False,
                    help='Specify that the csv file is actually tab delimited')

args = parser.parse_args()

#print args
if args.info:  args.info = args.info.split(",")
if args.logFC: args.logFC = args.logFC.split(",")
args.link_col = [args.link_col] if args.link_col else []

# print args

if args.csvfile == sys.stdin:
    sys.stderr.write("Reading from stdin...\n")

csv_file = args.csvfile.read()

err = check_args(args, csv_file)

if not err:
    args.out.write(embed(csv_file, args))

