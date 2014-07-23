num_loading = 0
start_loading = () ->
    num_loading += 1
    $('#loading').show()
    $('#dge-pc,#dge-ma,#dge-pca').css('opacity',0.4)
done_loading = () ->
    num_loading -= 1
    if num_loading==0
        $('#loading').hide()
        $('#dge-pc,#dge-ma,#dge-pca').css('opacity',1)

html_escape = (str) ->
    $('<div/>').text(str).html()

class WithoutBackend
    constructor: (@settings, @process_dge_data) ->
        $('.conditions').hide()
        $('.config').hide()
        $('a.show-r-code').hide()

    request_init_data: () ->
        start_loading()
        if @settings.csv_data
            # csv data is already in document
            log_info("Using embedded csv")
            @_data_ready(@settings.csv_data)
        else
            d3.text(@settings.csv_file, "text/csv", (err,dat) =>
                log_info("Downloaded csv")
                log_debug("Downloaded csv",dat,err)
                if err
                    log_error(err)
                    return
                @_data_ready(dat)
            )

    _data_ready: (dat) ->
        if @settings.csv_format
            data = d3.csv.parse(dat)
        else
            data = d3.tsv.parse(dat)
        @process_dge_data(data, @settings.columns)
        done_loading()

    request_kegg_data: (callback) ->
        log_error("Get KEGG data not supported without backend")

# BackendCommon - used by both WithBackendNoAnalysis and WithBackendAnalysis
class BackendCommon
    @config_url: () ->
        "config.html?code=#{window.my_code}"

    @script: (params) ->
        "r-json.cgi?code=#{window.my_code}" + if params then "&#{params}" else ""

    constructor: (@settings, configured) ->
        # Ensure we have been configured!
        if !configured
            window.location = BackendCommon.config_url()

        if @settings['locked']
            $('a.config').hide()
        else
            $('a.config').removeClass('hide')
            $('a.config').attr('href', BackendCommon.config_url())
            $('a.geneset-upload').attr('href', "geneset-upload.html?code=#{window.my_code}")

        @setup_genesets()

    setup_genesets: () ->
        window.xx = @settings['sets']
        console.log @settings['sets']
        if @settings['sets'] && @settings['sets'].length>0
            opts = "<option value=''>--- No set selected ---</option>"
            @settings['sets'].forEach( (s) ->
                opts += "<option value='#{s.file}'>#{s.name}</option>"
            )
            $('.geneset-filter select').html(opts)

            $('.geneset-filter').removeClass('hide')
            $('.geneset-filter').show()
        else
            $('.geneset-filter').hide()

    request_kegg_data: (callback) ->
        d3.tsv(BackendCommon.script('query=kegg_titles'), (err,ec_data) ->
            log_info("Downloaded kegg : rows=#{ec_data.length}")
            log_debug("Downloaded kegg : rows=#{ec_data.length}",ec_data,err)
            callback(ec_data)
        )

class WithBackendNoAnalysis
    constructor: (@settings, @process_dge_data) ->
        @backend = new BackendCommon(@settings, @settings.fc_columns.length > 0)

        $('.conditions').hide()
        $('a.show-r-code').hide()

    request_kegg_data: (callback) ->
        @backend.request_kegg_data(callback)

    request_init_data: () ->
        req = BackendCommon.script("query=csv")
        start_loading()
        d3.text(req, (err, dat) =>
            log_info("Downloaded DGE CSV: len=#{dat.length}")
            done_loading()
            if err
                log_error(err)
                return

            if @settings.csv_format
               data = d3.csv.parse(dat)
            else
               data = d3.tsv.parse(dat)
            log_info("Parsed DGE CSV : rows=#{data.length}")
            log_debug("Parsed DGE CSV : rows=#{data.length}",data,err)

            data_cols = @settings.info_columns.map((n) -> {idx: n, name: n, type: 'info' })
            data_cols.push({idx: '_dummy', type: 'primary', name:@settings.primary_name})
            @settings.fc_columns.forEach((n) ->
                data_cols.push({idx: n, type: 'fc', name: n})
            )
            data_cols.push({idx: @settings.fdr_column, name: @settings.fdr_column, type: 'fdr'})
            data_cols.push({idx: @settings.avg_column, name: @settings.avg_column, type: 'avg'})
            if @settings.ec_column?
                data_cols.push({idx: @settings.ec_column, name: 'EC', type: 'ec'})
            if @settings.link_column?
                data_cols.push({idx: @settings.link_column, name: 'link', type: 'link'})

            @process_dge_data(data, data_cols)
        )

class WithBackendAnalysis
    constructor: (@settings, @process_dge_data) ->
        @backend = new BackendCommon(@settings, @settings.replicates.length > 0)

        $('.conditions').show()
        $('a.show-r-code').show()

        $('select#dge-method').click((e) => @_select_sample())

    request_kegg_data: (callback) ->
        @backend.request_kegg_data(callback)

    request_init_data: () ->
        @_init_condition_selector()

    _request_dge_data: () ->
        columns = @current_selection
        return if columns.length <= 1

        # load csv file and create the chart
        method = @_get_dge_method()
        req = BackendCommon.script("query=dge&method=#{method}&fields=#{encodeURIComponent(JSON.stringify columns)}")
        start_loading()
        d3.csv(req, (err, data) =>
            log_info("Downloaded DGE counts : rows=#{data.length}")
            log_debug("Downloaded DGE counts : rows=#{data.length}",data,err)
            done_loading()
            if err
                log_error(err)
                return

            data_cols = @settings.info_columns.map((n) -> {idx: n, name: n, type: 'info' })
            pri=true
            columns.forEach((n) ->
                typ = if pri then 'primary' else 'fc'
                data_cols.push({idx: n, type: typ, name: n})
                pri=false
            )
            data_cols.push({idx: 'adj.P.Val', name: 'FDR', type: 'fdr'})
            data_cols.push({idx: 'AveExpr', name: 'AveExpr', type: 'avg'})
            if @settings.ec_column?
                data_cols.push({idx: @settings.ec_column, name: 'EC', type: 'ec'})
            if @settings.link_column?
                data_cols.push({idx: @settings.link_column, name: 'link', type: 'link'})
            @settings.replicates.forEach(([name,reps]) ->
                reps.forEach((rep) ->
                    data_cols.push({idx: rep, name: rep, type: 'count', parent: name})
                )
            )

            @process_dge_data(data, data_cols)
        )

    request_r_code: (callback) ->
        columns = @current_selection
        method = @_get_dge_method()
        req = BackendCommon.script("query=dge_r_code&method=#{method}&fields=#{encodeURIComponent(JSON.stringify columns)}")
        d3.text(req, (err,data) ->
            log_debug("Downloaded R Code : len=#{data.length}",data,err)
            callback(data)
        )

    _get_dge_method: () ->
        $('select#dge-method option:selected').val()

    _set_dge_method: (method) ->
        $('select#dge-method').val(method)

    # Display the sample selector.
    _select_sample: (e) ->
        current_dge_method = @_get_dge_method()

        # EditList will do nothing if the specified element is already in "edit" mode
        new EditList(
            elem: '.conditions'
            apply: () => @_update_samples()
            cancel: () =>
                @_set_selected_cols(@current_selection)
                @_set_dge_method(current_dge_method)
        )

    # This reads the state of the form checkboxes and stores it in @current_selection
    # Note we do it this way rather than a getter because we can't seem to capture
    # a checkbox change event *before* the checkbox is updated.  So, doing it
    # this way allows us to "cancel" an checkbox changes after the fact
    _update_selected_cols: () ->
        cols = []
        # Create a list of conditions that are selected
        $('#files input:checked').each( (i, n) =>
            name = $(n).data('rep')
            cols.push(name)
        )
        @current_selection = cols

    _set_selected_cols: (cols) ->
        $('#files input:checkbox').each( (i, n) =>
            name = $(n).data('rep')
            $(n).prop('checked', name in cols)
        )

    _update_samples: () ->
        @_update_selected_cols()
        @_request_dge_data()

    _init_condition_selector: () ->
        init_select = @settings['init_select'] || []
        $.each(@settings.replicates, (i, rep) =>
            name = rep[0]
            div = $("""<label>
                        <input type='checkbox' title='Select this condition' data-placement='right'> #{name}
                       </label>
                    """)

            $("#files").append(div)
            $("input",div).data('rep',name)
        )
        $("#files input").change((e) => @_select_sample(e))

        @_set_selected_cols(init_select)
        @_update_samples()

blue_to_brown = d3.scale.linear()
  .domain([0,0.05,1])
  .range(['brown', "steelblue", "steelblue"])
  .interpolate(d3.interpolateLab)

colour_cat20 = d3.scale.category20().domain([1..20])
colour_by_ec = (ec_col) ->
    (row) -> colour_cat20(row[ec_col])

colour_by_pval = (col) ->
    (d) -> blue_to_brown(d[col])

# Globals for widgets
parcoords = null
ma_plot = null
pca_plot = null
expr_plot = null    # Actually parcoords OR ma_plot depending which is active

gene_table = null
kegg = null
heatmap = null

g_data = null
g_backend = null
requested_kegg = false


# Globals for settings
show_counts = 'no'   # Possible values 'yes','no','cpm'
fdrThreshold = 1
fcThreshold = 0

numGenesThreshold = 50
numGenesSlider = null    # Need a handle on this to update number of genes
skipGenesThreshold = 50
skipGenesSlider = null   # Need a handle on this to update number of genes
pcaDimension = 1
pcaDimsSlider = null

searchStr = ""
kegg_filter = []
geneset_filter = []
geneset_filter_column = null
h_runfilters = null
g_tour_setup = false

kegg_mouseover = (obj) ->
    ec = obj.id
    rows = []
    ec_col = g_data.column_by_type('ec')
    return if ec_col==null
    for row in g_data.get_data()
        rows.push(row) if row[ec_col] == ec
    expr_plot.highlight(rows)

# highlight parallel coords (and/or kegg)
gene_table_mouseover = (item) ->
    expr_plot.highlight([item])
    ec_col = g_data.column_by_type('ec')
    kegg.highlight(item[ec_col])

gene_table_mouseout = () ->
    expr_plot.unhighlight()
    $('#gene-info').html('')
    kegg.unhighlight()

# Rules for guess best info link based on some ID
guess_link_info =
    [{re: /^ENS/, link: 'http://ensembl.org/Multi/Search/Results?q=%s;site=ensembl'},
     {re: /^CG/, link: 'http://flybase.org/cgi-bin/uniq.html?species=Dmel&cs=yes&db=fbgn&caller=genejump&context=%s'},
     {re: /^/, link: 'http://www.ncbi.nlm.nih.gov/gene/?&term=%s'},
    ]

# Guess the link using the guess_link_info table
guess_link = (info) ->
    return if !info?
    for o in guess_link_info
        return o.link if info.match(o.re)
    return null

# Open a page for the given gene.  Use either the defined link or guess one.
# The "ID" column can be specified as 'link' otherwise the first 'info' column is used
gene_table_dblclick = (item) ->
    cols = g_data.columns_by_type(['link'])
    if cols.length==0
        cols = g_data.columns_by_type(['info'])
    if cols.length>0
        info = item[cols[0].idx]
        link = if settings.link_url? then settings.link_url else guess_link(info)
        log_debug("Dbl click.  Using info/link",info,link)
        if link?
            link = link.replace(/%s/, info)
            window.open(link)
            window.focus()

get_default_plot_typ = () ->
    if g_data.columns_by_type(['fc','primary']).length>2
        'parcoords'
    else
        'ma'

may_set_plot_var = (typ) ->
    if typ == get_default_plot_typ()
        set_hash_var({plot: null})
    else
        set_hash_var({plot: typ})

activate_parcoords = () ->
    may_set_plot_var('parcoords')
    expr_plot = parcoords
    $('#dge-ma,#dge-pca').hide()
    $('#dge-pc').show()
    $('#select-pc').addClass('active')
    $('#select-ma,#select-pca').removeClass('active')
    $('.ma-fc-col-opt').hide()
    $('.pca-opts').hide()
    heatmap.enabled(true)
    update_data()

activate_ma_plot = () ->
    may_set_plot_var('ma')
    expr_plot = ma_plot
    $('#dge-pc,#dge-pca').hide()
    $('#dge-ma').show()
    $('#select-ma').addClass('active')
    $('#select-pc,#select-pca').removeClass('active')
    $('.ma-fc-col-opt').show()
    $('.pca-opts').hide()
    heatmap.enabled(true)
    update_data()

activate_pca_plot = () ->
    may_set_plot_var('mds')
    expr_plot = pca_plot
    $('#dge-pc,#dge-ma').hide()
    $('#dge-pca').show()
    $('#select-pca').addClass('active')
    $('#select-pc,#select-ma').removeClass('active')
    $('.ma-fc-col-opt').hide()
    heatmap.enabled(false)
    $('.pca-opts').show()
    numGenesSlider.set_max(100, 1, g_data.get_data().length, true)
    skipGenesSlider.set_max(0, 0, g_data.get_data().length, true)

    update_data()

calc_max_parcoords_width = () ->
    w = $('.container').width()
    w -= $('.conditions').outerWidth(true) if $('.conditions').is(':visible')
    w -= $('div.filter').outerWidth(true) if $('div.filter').is(':visible')

init_charts = () ->
    gene_table = new GeneTable(
        elem: '#grid'
        elem_info: '#grid-info'
        sorter: do_sort
        mouseover: gene_table_mouseover
        mouseout: gene_table_mouseout
        dblclick: gene_table_dblclick
        filter: gene_table_filter
        )

    parcoords = new ParCoords(
        elem: '#dge-pc'
        width: calc_max_parcoords_width()
        filter: expr_filter
        )

    ma_plot = new MAPlot({elem: '#dge-ma', filter: expr_filter})

    pca_plot = new GenePCA(
        elem: '#dge-pca'
        filter: expr_filter
        gene_table: gene_table
        sel_dimension: (d) => pcaDimsSlider.set_val(+d, true)
        params: () ->
            skip: +skipGenesThreshold
            num: +numGenesThreshold
            dims: [+pcaDimension, +pcaDimension+1]
        )

    kegg = new Kegg(
        elem: 'div#kegg-image'
        mouseover: kegg_mouseover
        mouseout: () -> expr_plot.unhighlight()
        )

    # update grid on brush
    parcoords.on("brush", (d) ->
        gene_table.set_data(d)
        heatmap.schedule_update(d)
    )
    ma_plot.on("brush", (d) ->
        gene_table.set_data(d)
        heatmap.schedule_update(d)
    )

    #Heatmap
    heatmap = new Heatmap(
        elem: '#heatmap'
        width: $('.container').width()
        mouseover: (d) ->
            expr_plot.highlight([d])
            msg = ""
            for col in g_data.columns_by_type(['info'])
              msg += "<span class='lbl'>#{col.name}: </span><span>#{d[col.idx]}</span>"
            $('#heatmap-info').html(msg)
        mouseout:  (d) ->
            expr_plot.unhighlight()
            $('#heatmap-info').html("")
    )


comparer = (x,y) -> (if x == y then 0 else (if x > y then 1 else -1))

do_sort = (args) ->
    column = g_data.column_by_idx(args.sortCol.field)
    gene_table.sort((r1,r2) ->
        r = 0
        x=r1[column.idx]; y=r2[column.idx]
        if column.type in ['fc_calc']
            r = comparer(Math.abs(x), Math.abs(y))
        else if column.type in ['fdr']
            r = comparer(x, y)
        else
            r = comparer(x,y)
        r * (if args.sortAsc then 1 else -1)
    )

set_gene_table = (data) ->
    column_keys = g_data.columns_by_type(['info','fdr'])
    column_keys = column_keys.concat(g_data.columns_by_type('fc_calc'))
    columns = column_keys.map((col) ->
        id: col.idx
        name: col.name
        field: col.idx
        sortable: true
        formatter: (i,c,val,m,row) ->
            if col.type in ['fc_calc']
                fc_div(val, col, row)
            else if col.type in ['fdr']
                if val<0.01 then val.toExponential(2) else val.toFixed(2)
            else
                val
    )

    gene_table.set_data(data, columns)

fc_div = (n, column, row) ->
    colour = if n>0.1 then "pos" else if n<-0.1 then "neg" else ""
    countStr = ""
    if show_counts=='yes'
        count_columns = g_data.assoc_column_by_type('count',column.name)
        vals = count_columns.map((c) -> row[c.idx])
        countStr = "<span class='counts'>(#{vals})</span>"
    else if show_counts=='cpm'
        count_columns = g_data.assoc_column_by_type('count',column.name)
        vals = count_columns.map((c) -> tot=g_data.get_total(c) ; (1000000 * row[c.idx]/tot).toFixed(1))
        countStr = "<span class='counts'>(#{vals})</span>"
    "<div class='#{colour}'>#{n.toFixed(2)}#{countStr}</div>"


gene_table_filter = (item) ->
    return true if searchStr == ""
    for col in g_data.columns_by_type('info')
        str = item[col.idx]
        return true if str? && typeof str == 'string' &&
                       str.toLowerCase().indexOf(searchStr)>=0
    false

# Filter to decide which rows to plot on the parallel coordinates widget
expr_filter = (row) ->
    if fcThreshold>0
        # Filter using largest FC between any pair of samples
        fc = g_data.columns_by_type('fc').map((c) -> row[c.idx])
        extent_fc = d3.extent(fc.concat([0]))
        if Math.abs(extent_fc[0] - extent_fc[1]) < fcThreshold
            return false

    # Filter by FDR
    pval_col = g_data.columns_by_type('fdr')[0]
    return false if row[pval_col.idx] > fdrThreshold

    # If a Kegg pathway is selected, filter to that.
    if kegg_filter.length>0
        ec_col = g_data.column_by_type('ec')
        return row[ec_col] in kegg_filter

    # If a gene set is selected, filter to that
    if geneset_filter.length>0
        return row[geneset_filter_column] in geneset_filter

    true

init_search = () ->
    $(".tab-search input").keyup (e) ->
                    Slick.GlobalEditorLock.cancelCurrentEdit()
                    this.value = "" if e.which == 27     # Clear on "Esc"
                    searchStr = this.value.toLowerCase()
                    $(this).toggleClass('active', searchStr != "")
                    gene_table.refresh()

init_download_link = () ->
    $('a#csv-download').on('click', (e) ->
        e.preventDefault()
        items = gene_table.get_data()
        return if items.length==0
        cols = g_data.columns_by_type(['info','fc_calc','count','fdr'])
        keys = cols.map((c) -> c.name)
        rows=items.map( (r) -> cols.map( (c) -> r[c.idx] ) )
        window.open("data:text/csv,"+escape(d3.csv.format([keys].concat(rows))), "file.csv")
    )

redraw_plot = () ->
    expr_plot.brush()

init_slider = () ->
    # wire up the slider to apply the filter to the model
    new Slider(
          id: "#fdrSlider"
          input_id: "input.fdr-fld"
          step_values: [0, 1e-6, 1e-5, 1e-4, 0.001, .01, .02, .03, .04, .05, 0.1, 1]
          val: fdrThreshold
          validator: (v) ->
             n = Number(v)
             !(isNaN(n) || n<0 || n>1)
          on_change: (v) ->
             Slick.GlobalEditorLock.cancelCurrentEdit()
             if (fdrThreshold != v)
               window.clearTimeout(h_runfilters)
               h_runfilters = window.setTimeout(redraw_plot, 10)
               fdrThreshold = v
    )
    new Slider(
          id: "#fcSlider"
          input_id: "input.fc-fld"
          step_values: (Number(x.toFixed(2)) for x in [0..5] by 0.01)
          val: fcThreshold
          validator: (v) ->
             n = Number(v)
             !(isNaN(n) || n<0)
          on_change: (v) ->
             Slick.GlobalEditorLock.cancelCurrentEdit()
             if (fcThreshold != v)
               window.clearTimeout(h_runfilters)
               h_runfilters = window.setTimeout(redraw_plot, 10)
               fcThreshold = v
    )
    numGenesSlider = new Slider(
          id: "#numGenesSlider"
          input_id: "input.num-genes-fld"
          val: numGenesThreshold
          validator: (v) ->
             n = Number(v)
             !(isNaN(n) || n<0)
          on_change: (v) ->
             Slick.GlobalEditorLock.cancelCurrentEdit()
             if (numGenesThreshold != v)
               window.clearTimeout(h_runfilters)
               h_runfilters = window.setTimeout(redraw_plot, 10)
               numGenesThreshold = v
    )
    skipGenesSlider = new Slider(
          id: "#skipGenesSlider"
          input_id: "input.skip-genes-fld"
          val: skipGenesThreshold
          validator: (v) ->
             n = Number(v)
             !(isNaN(n) || n<0)
          on_change: (v) ->
             Slick.GlobalEditorLock.cancelCurrentEdit()
             if (skipGenesThreshold != v)
               window.clearTimeout(h_runfilters)
               h_runfilters = window.setTimeout(redraw_plot, 10)
               skipGenesThreshold = v
    )
    pcaDimsSlider = new Slider(
          id: "#pcaDimsSlider"
          input_id: "input.pca-dims-fld"
          step_values: [1..10]
          val: pcaDimension
          fmt: (v) -> v+" vs "+(v+1)
          validator: (v) ->
             n = Number(v)
             !(isNaN(n) || n<0)
          on_change: (v) ->
            if (pcaDimension != v)
                window.clearTimeout(h_runfilters)
                h_runfilters = window.setTimeout(redraw_plot, 10)
                pcaDimension = v
    )
    $('#fc-relative').change((e) ->
        update_data()
    )
    $('#ma-fc-col').change((e) ->
        update_data()
    )
    $('#show-counts').change((e) ->
        update_flags()
        gene_table.invalidate()
    )

geneset_selected = () ->
    code = $('.geneset-filter select option:selected').val()
    if !code
        geneset_filter = []
        update_data()
    else
        url = "r-json.cgi?code=#{code}&query=geneset_get"
        d3.tsv(url, (err, dat) ->
            possible_keys = d3.keys(dat[0])
            columns = g_data.columns_by_type('info').map((c) -> c.name)
            key=null
            for k in possible_keys
                if k in columns
                    key=k
                    break
            if !key?
                log_error "No valid key column in data set : #{possible_keys}"
            else
                log_info "Loaded geneset.  Matching on key=#{key} : rows=#{dat.length}"

            geneset_filter_column = key
            geneset_filter = dat.map((r) -> r[key])
            update_data()
        )

calc_kegg_colours = () ->
    ec_dirs = {}
    ec_col = g_data.column_by_type('ec')
    return if ec_col==null
    fc_cols = g_data.columns_by_type('fc_calc')[1..]
    for row in g_data.get_data()
        ec = row[ec_col]
        continue if !ec
        for col in fc_cols
            v = row[col.idx]
            dir = if v>0.1 then "up" else if v<-0.1 then "down" else "same"
            ec_dirs[ec]=dir if !ec_dirs[ec]
            ec_dirs[ec]='mixed' if ec_dirs[ec]!=dir
    return ec_dirs

kegg_selected = () ->
    code = $('select#kegg option:selected').val()
    title = $('select#kegg option:selected').text()

    set_filter = (ec) ->
        kegg_filter = ec
        update_data()

    if !code
        set_filter([])
    else
        ec_colours = calc_kegg_colours()
        kegg.load(code, ec_colours, set_filter)
        $('div#kegg-image').dialog({width:500, height:600, title: title, position: {my: "right top", at:"right top+60", of: $('body')} })

process_kegg_data = (ec_data) ->
    return if requested_kegg
    requested_kegg = true
    opts = "<option value=''>--- No pathway selected ---</option>"

    have_ec = {}
    ec_col = g_data.column_by_type('ec')
    for row in g_data.get_data()
        have_ec[row[ec_col]]=1

    ec_data.forEach (row) ->
        num=0
        for ec in row.ec.split(" ").filter((s) -> s.length>0)
            num++ if have_ec[ec]
        if num>0
            opts += "<option value='#{row.code}'>#{row.title} (#{num})</option>"
    $('select#kegg').html(opts)
    $('.kegg-filter').show()

process_dge_data = (data, columns) ->
    g_data = new GeneData(data, columns)

    # Setup FC "relative" pulldown
    opts = ""
    for col,i in g_data.columns_by_type(['fc','primary'])
        opts += "<option value='#{i}'>#{html_escape col.name}</option>"
    opts += "<option value='-1'>Average</option>"
    $('select#fc-relative').html(opts)

    # Setup MA-plot pulldown
    opts = ""
    for col,i in g_data.columns_by_type(['fc','primary'])
        opts += "<option value='#{i}' #{if i==1 then 'selected' else ''}>#{html_escape col.name}</option>"
    $('select#ma-fc-col').html(opts)

    if g_data.column_by_type('ec') == null
        $('.kegg-filter').hide()
    else if !requested_kegg
        g_backend.request_kegg_data(process_kegg_data)

    if g_data.columns_by_type('count').length == 0
        $('.show-counts-opt').hide()
        $('#select-pca').hide()
    else
        $('.show-counts-opt').show()
        $('#select-pca').show()


    set_plot = (typ) ->
        switch typ
            when 'mds'       then activate_pca_plot() ; true
            when 'ma'        then activate_ma_plot() ; true
            when 'parcoords' then activate_parcoords() ; true
            else false

    set_plot(get_hash_vars()['plot']) || set_plot(get_default_plot_typ())

    # First time throught?  Setup the tutorial tour
    if !g_tour_setup
        g_tour_setup = true
        setup_tour(if settings.show_tour? then settings.show_tour else true)

update_flags = () ->
    show_counts = $('select#show-counts option:selected').val()

# Called whenever the data is changed, or the "checkboxes" are modified
update_data = () ->
    update_flags()

    # Set the 'relative' column
    fc_relative = $('select#fc-relative option:selected').val()
    if fc_relative<0
        fc_relative = 'avg'
    else
        fc_relative = g_data.columns_by_type(['fc','primary'])[fc_relative]
    g_data.set_relative(fc_relative)

    dims = g_data.columns_by_type('fc_calc')
    ec_col = g_data.column_by_type('ec')
    pval_col = g_data.column_by_type('fdr')
    color = colour_by_pval(pval_col)

    extent = ParCoords.calc_extent(g_data.get_data(), dims)

    if expr_plot == parcoords
        parcoords.update_data(g_data.get_data(), dims, extent, color)
    else if expr_plot == ma_plot
        ma_fc = $('select#ma-fc-col option:selected').val()
        ma_fc = g_data.columns_by_type(['fc','primary'])[ma_fc].name
        col = g_data.columns_by_type('fc_calc').filter((c) -> c.name == ma_fc)
        log_error("Can't find proper column for MA-plot") if col.length!=1
        ma_plot.update_data(g_data.get_data(), col, g_data.columns_by_type('avg'),
                            color,
                            g_data.columns_by_type('info'),
                            pval_col)
    else if expr_plot == pca_plot
        cols = g_data.columns_by_type('fc_calc').map((c) -> c.name)
        count_cols = g_data.columns_by_type('count').filter((c) -> cols.indexOf(c.parent)>=0)
        pca_plot.update_data(g_data.get_data(), count_cols)

    set_gene_table(g_data.get_data())

    # Update the heatmap
    if heatmap.enabled()
        heatmap_dims = g_data.columns_by_type('fc_calc_avg')
        heatmap_extent = ParCoords.calc_extent(g_data.get_data(), heatmap_dims)
        heatmap.update_columns(heatmap_dims, heatmap_extent, pval_col)
        heatmap.schedule_update(g_data.get_data())

    # Ensure the brush callbacks are called (updates heatmap & table)
    expr_plot.brush()

show_r_code = () ->
    g_backend.request_r_code((d) ->
        $('div#code-modal .modal-body pre').text(d)
        $('div#code-modal').modal()
    )

render_page = (template) ->
    # Show the main html
    opts =
        asset_base: settings.asset_base || ''
        home_link: settings.home_link || 'index.html'

    body = $(template(opts))
    $('#replace-me').replaceWith(body)
    $('#main-loading').hide()
    setup_nav_bar()
    $('[title]').tooltip()

render_main_page = () -> render_page(require("../templates/compare-body.hbs"))
render_fail_page = () -> render_page(require("../templates/fail.hbs"))

init_page = (use_backend) ->
    render_main_page()

    g_data = new GeneData([],[])

    if use_backend
        if settings.analyze_server_side
            g_backend = new WithBackendAnalysis(settings, process_dge_data)
        else
            g_backend = new WithBackendNoAnalysis(settings, process_dge_data)
    else
        g_backend = new WithoutBackend(settings, process_dge_data)


    title = settings.name || "Unnamed"
    $(".exp-name").text(title)
    document.title = title

    fdrThreshold = settings['fdrThreshold'] if settings['fdrThreshold'] != undefined
    fcThreshold  = settings['fcThreshold']  if settings['fcThreshold'] != undefined

    $("select#kegg").change(kegg_selected)
    $(".geneset-filter select").change(geneset_selected)

    $('#select-pc a').click((e) ->  e.preventDefault(); activate_parcoords())
    $('#select-ma a').click((e) ->  e.preventDefault(); activate_ma_plot())
    $('#select-pca a').click((e) -> e.preventDefault(); activate_pca_plot())
    $('a.show-r-code').click((e) -> e.preventDefault(); show_r_code())

    init_charts()
    init_search()
    init_slider()
    init_download_link()

    g_backend.request_init_data()

init = () ->
    code = get_url_vars()["code"]
    if !code?
        init_page(false)
    else
        window.my_code = code
        $.ajax({
            type: "GET",
            url: BackendCommon.script("query=settings"),
            dataType: 'json'
        }).done((json) ->
            window.settings = json
            init_page(true)
         ).fail((x) ->
            log_error "Failed to get settings!",x
            render_fail_page()
            pre = $("<pre></pre>")
            pre.text("Error failed to get settings : #{x.responseText}")
            $('.error-msg').append(pre)
        )


$(document).ready(() -> init() )
