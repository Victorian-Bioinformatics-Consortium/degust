class Settings
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
        File.open("#{Settings.user_dir}/#{code}", File::WRONLY|File::CREAT|File::EXCL)
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
    File.write(Settings.settings_file(code), settings.to_json)
    settings = Settings.new(code)

    FileUtils.cp(file.path, settings.counts_file)
    settings
  end

  def write_user_settings(s)
    f = Settings.settings_file(code)
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
    "#{Settings.user_dir}/#{@code}-counts.csv"
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
    f = Settings.settings_file(code)
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
