#!/usr/bin/env ruby

# Process counts.csv from tail-tools (from /expression/peakwise/counts.csv)
# Remove any peaks with <10 counts across any sample
# Inlcude gene info for each peak (from peaks/relation-parent.gff)

raise "Requires ruby 1.9.x" if RUBY_VERSION !~ /^1.9/

require 'csv'

min_count = 10

peaks = Hash.new
CSV.foreach("relation-parent.gff", :headers => false, :col_sep => "\t") do |r|
  str = r[8]
  next if !str
  flds = Hash[str.split(/;/).map {|x| x.split(/=/,2)}]
  flds.default = ""
  (flds["has_in"].split(',') + flds["has_downstrand"].split(',')).each do |id|
    (peaks[id] ||= []).push flds
  end
end

def k(arr, key)
  arr.nil? ? "" : arr.map {|x| x[key]}.join(':')
end

File.open("counts.csv") do |f|
  headers = []
  info = nil
  counts = []
  tails = []
  prop = []
  f.each do |l|
    l.chomp!
    if l =~ /^#Groups/
      info = l.split(',')
    end
    if l !~ /^#/
      l.split(',').zip(info) do |a,b|
        a += "-" + b if b =~ /Count|Tail|Proportion/
        headers.push(a)
        counts.push(a) if b == 'Count'
        tails.push(a) if b=='Tail'
        prop.push(a) if b=='Proportion'
      end
      break
    end
  end
  new_headers = headers.reject {|k| counts.include?(k) || prop.include?(k) }
  CSV($stdout, :col_sep => ",") do |out|
    out << new_headers + ['Gene name','Gene ID','Product']
    csv = CSV.new(f, :headers => headers, :col_sep => ",")
    csv.each do |l|
      if counts.all? {|c| l[c].to_i >= min_count } && tails.all? {|c| l[c] != 'NA'}
        id = l['Feature']
        next if k(peaks[id],'Name')==''
        nl = new_headers.map do |h|
               if tails.include?(h)
                 sprintf("%.1f",l[h])
               else
                 l[h]
               end
             end
        nl.push(k(peaks[id],'Name'),k(peaks[id],'ID'),k(peaks[id],'Product'))
        out << nl
      end
    end
  end
end
