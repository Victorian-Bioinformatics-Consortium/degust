#!/usr/bin/env ruby

require 'sinatra'
require 'json'
require 'open3'
require 'csv'

require './settings'
require './r-params'
require './dge'

# Standard links to suppport
# http://vicbioinformatics.com/degust/compare.html?code=example

# Old links
# http://vicbioinformatics.com/degust/r-json.cgi?code=example

$debug=false

# Front page
get '/' do
  send_file "public/index.html"
end

# Other pages
get '/r-json.cgi' do
  main_dispatch
end

post '/r-json.cgi' do
  main_dispatch
end

def main_dispatch
  q = params[:query]
  code = params[:code]
  case q
  when nil
    redirect_main(code)
  when 'settings'
    get_settings(code)
  when 'partial_csv'
    do_partial_csv(code)
  when 'dge'
    dge = DGE.new(code, params[:method], JSON.parse(params[:fields]))
    send_file(dge.do_dge)
  when 'dge_r_code'
    dge = DGE.new(code, params[:method], JSON.parse(params[:fields]))
    dge.show_code
  when 'kegg_titles'
    do_kegg(code)
  when 'upload'
    do_upload
  when 'save'
    do_save(code, JSON.parse(params[:settings]))
  else
    status 404
    body 'Unknown query'
  end
end

# If there is a code, go to the new link (for old-style links)
def redirect_main(code)
  if code.nil?
    redirect to('/')
  else
    redirect to("compare.html?code=#{code}")
  end
end


class NoSuchCode < StandardError
end

error NoSuchCode do
  'No such code'
end

def get_settings(code)
  content_type :json
  s = Settings.new(code)
  s.user_settings.to_json
end


def do_kegg(code)
  settings = Settings.new(code)

  files = CSV.read('public/kegg/pathway/map_title.tab', :col_sep=>"\t")
  files.each do |file|
    begin
      str = File.read("public/kegg/kgml/map/map#{file[0]}.xml")
      file.push(str.scan(/name="ec:([.\d]+)"/).join(" "))
    rescue
      file.push("")
    end
  end

  CSV.generate(:col_sep =>"\t") do |csv|
    csv << ["code","title","ec"]
    files.each {|f| csv << f}
  end
end

def isValid(file)
  nlines = %x{wc -l "#{file.path}"}.to_i
  if nlines < 10
    raise "too few lines : #{nlines}"
  end
  if nlines > 100000
    raise "too many lines"
  end
  true
end

def do_upload
  unless params[:filename] &&
         (tmpfile = params[:filename][:tempfile]) &&
         (name = params[:filename][:filename])
    return "Error : no file selected"
  end

  isValid(tmpfile)
  ip = request.ip
  settings = Settings.create(tmpfile, request.ip)
  redirect to("compare.html?code=#{settings.code}")
end

def do_partial_csv(code)
  settings = Settings.new(code)
  res = ""
  n=0
  File.foreach(settings.counts_file) do |l|
    res += l
    n+=1
    break if n>=20
  end
  content_type 'text/csv'
  res
end

def do_save(code, user_settings)
  settings = Settings.new(code)


  raise "File is locked" if settings.as_hash['locked']

  settings.write_user_settings(user_settings)

  content_type :json
  {:result => "ok!"}.to_json
end
