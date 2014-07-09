#!/usr/bin/env ruby

require 'sinatra'
require 'json'
require 'open3'
require 'csv'


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
    dge = DGE_Method.new(code, params[:method], JSON.parse(params[:fields]))
    send_file(dge.do_dge)
  when 'dge_r_code'
    dge = DGE_Method.new(code, params[:method], JSON.parse(params[:fields]))
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
  s = DGE_Settings.new(code)
  s.user_settings.to_json
end


class DGE_Settings
  def initialize(unsafe_code)
    @code = sanitize_code(unsafe_code)
    @settings = load_settings(@code)
    user_settings['locked'] = @settings['locked']
  end

  def self.create(file, ip_addr)
    now = Time.now.to_i
    code=''
    while true
      code = Digest::MD5.hexdigest("#{now}#{rand}")
      begin
        File.open("#{DGE_Settings.user_dir}/#{code}", File::WRONLY|File::CREAT|File::EXCL)
      rescue
        # Do nothing, just loop
      else
        break
      end
    end

    settings = {
      :code => code,
      :create => Time.now,
      :locked => false,
      :remote_addr => ip_addr,
      :user_settings => {
        :name => "",
        :fdr_column => "",
        :analyze_server_side => true,
        :avg_column => "",
        :primary_name => "",
        :fc_columns => [],
        :hide_columns => [],
        :init_select => [],
        :skip => 0,
        :csv_format => true,
        :replicates => [],
        :info_columns => [],
      },
    }
    File.write(DGE_Settings.settings_file(code), settings.to_json)
    settings = DGE_Settings.new(code)

    FileUtils.cp(file.path, settings.counts_file)
    settings
  end

  def write_user_settings(s)
    f = DGE_Settings.settings_file(code)
    tmp = f + '.tmp'
    @settings['user_settings'] = s
    File.write(tmp, @settings.to_json)
    FileUtils.mv(tmp, f, :force => true)
  end

  def code
    @code
  end

  def as_hash
    @settings
  end

  def user_settings
    @settings['user_settings']
  end

  def counts_file
    "#{DGE_Settings.user_dir}/#{@code}-counts.csv"
  end

  private

  def self.user_dir
    "user-files"
  end

  def self.settings_file(code)
    "#{user_dir}/#{code}-settings.js"
  end

  def sanitize_code(code)
    code.gsub(/\.|\//,'')
  end

  def may_set(hsh, defaults)
    defaults.each do |k,v|
      hsh[k]=v if !hsh.has_key?(k)
    end
  end

  def load_settings(code)
    f = DGE_Settings.settings_file(code)
    if !File.exist?(f)
      raise NoSuchCode, "no such code : #{code}"
    end
    res = JSON.parse(File.read(f))

    defaults = {
      'init_select'  => [],
      'hide_columns' => [],
      'name' => "",
      'fc_columns' => [],
      'primary_name' => "",
      'avg_column' => "",
      'analyze_server_side' => true,
      'fdr_column' => "",
      'min_counts' => 0,
    }
    may_set(res['user_settings'], defaults)
    res
  end
end

class R_Params
  def initialize(settings)
    @settings = settings
  end

  def sep_char
    case @settings.user_settings['csv_format']
    when "TAB"; "\\t"
    when "CSV"; ","
    else ","
    end
  end

  def count_columns
    r_list_chr(count_columns_arr)
  end

  def design_matrix
    names = @settings.user_settings['replicates'].map{|rep| rep[0]}
    r_matrix(design_matrix_arr, names)
  end

  def contrast_matrix(flds)
    r_matrix(contrast_matrix_arr(flds), flds)
  end

  def export_cols
    r_list_chr(
               (@settings.user_settings['info_columns'] +
                count_columns_arr +
                [@settings.user_settings['ec_column']]
            ).compact
           )
  end

  private

  # Escape the given string for R
  def esc(str)
    "'" + str.to_s.gsub(/(['\\])/) {|x| "\\" + x} + "'"
  end

  # Convert the array of strings, to an R list of strings
  def r_list_chr(lst)
    r_list_lit(lst.map{|x| esc(x)})
  end

  # Convert the array of numbers (or other literals) to an R list
  def r_list_lit(lst)
    "c(" + (lst.map{|x| x}).join(',') + ")"
  end

  # Convert an array of arrays of ints (or other literals) to an R matrix with the given column names
  def r_matrix(mat, colNames)
    "matrix(c(" + (mat.map{|lst| r_list_lit(lst)}).join(',') + "), byrow=F, nrow=#{mat[0].length}, dimnames=list(c(),#{r_list_chr(colNames)}))"
  end

  def count_columns_arr
    @settings.user_settings['replicates'].map{|x| x[1]}.flatten
  end

  def design_matrix_arr
    mat = @settings.user_settings['replicates'].map do |rep|
      count_columns_arr.map do |c|
        rep[1].include?(c) ? 1 : 0
      end
    end
    mat
  end

  def contrast_matrix_arr(flds)
    ref = flds.shift
    flds.map do |f|
      @settings.user_settings['replicates'].map do |rep|
        case rep[0]
        when ref; -1
        when f; 1
        else 0
        end
      end
    end
  end

end

class DGE_Method
  def initialize(code, method, fields)
    (@code, @method, @fields) = [code, method, fields]
  end

  def do_dge
    hsh = param_hash
    fOut = runR(method_file, hsh)[:out]
    fOut.path
  end

  def show_code
    hsh = param_hash
    hsh[:file] = "outfile.csv"
    str = handlebars_simple(method_file, hsh)

    ver = runR("versions", hsh)

    str + "-"*60 + "\n" + ver[:stdout]
  end

  private

  def method_file
    case @method
    when 'voom'
      'voom'
    when 'edgeR'
      'edgeR'
    else
      'voom'
    end
  end

  def param_hash
    settings = DGE_Settings.new(@code)
    r_params = R_Params.new(settings)
    hsh = {
      :sep_char => r_params.sep_char,
      :counts_file => settings.counts_file,
      :counts_skip => settings.user_settings["skip"],
      :count_columns => r_params.count_columns,
      :min_counts => settings.user_settings["min_counts"],
      :design => r_params.design_matrix,

      :cont_matrix => r_params.contrast_matrix(@fields),
      :export_cols => r_params.export_cols,
    }
    hsh
  end

  def runR(r_file, hsh)
    fIn  = Tempfile.new('Rtmp', 'tmp')
    fOut = Tempfile.new('Rtmp', 'tmp')
    hsh[:file] = fOut.path

    str = handlebars_simple(r_file, hsh)
    File.write(fIn, str)

    out,err = Open3.capture3({"R_LIBS_SITE" => "/bio/sw/R:"}, "RScript --vanilla #{fIn.path}")

    if $debug
      stem="tmp/#{Time.now.to_i}"
      File.write(stem + '-stdin', str)
      File.write(stem + '-stdout', out)
      File.write(stem + '-stderr', err)
    end

    {:out => fOut, :stdout => out, :stderr => err, :in => str}
  end

  def handlebars_simple(fname, hsh)
    f = File.read("r-templates/#{fname.strip}.R")
    f.gsub(/{{(.*?)}}/) do |m|
      if $1[0] == '>'
        handlebars_simple($1[1..-1], hsh)
      else
        hsh[$1.to_sym]
      end
    end
  end
end


def do_kegg(code)
  settings = DGE_Settings.new(code)

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
  settings = DGE_Settings.create(tmpfile, request.ip)
  redirect to("compare.html?code=#{settings.code}")
end

def do_partial_csv(code)
  settings = DGE_Settings.new(code)
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
  settings = DGE_Settings.new(code)


  raise "File is locked" if settings.as_hash['locked']

  settings.write_user_settings(user_settings)

  content_type :json
  {:result => "ok!"}.to_json
end
