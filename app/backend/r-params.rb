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

