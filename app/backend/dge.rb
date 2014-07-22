require './settings'
require './r-params'

class DGE
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
    settings = Settings.new(@code)
    r_params = R_Params.new(settings)
    hsh = {
      :sep_char => r_params.sep_char,
      :counts_file => settings.counts_file,
      :counts_skip => settings.user_settings["skip"].to_i,
      :count_columns => r_params.count_columns,
      :min_counts => settings.user_settings["min_counts"].to_i,
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
