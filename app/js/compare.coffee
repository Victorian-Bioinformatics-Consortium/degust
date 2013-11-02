config_url = () -> "config.html?code=#{window.my_code}"

num_loading = 0
start_loading = () ->
    num_loading += 1
    $('#loading').show()
    $('#dge-pc,#dge-ma').css('opacity',0.4)
done_loading = () ->
    num_loading -= 1
    if num_loading==0
        $('#loading').hide()
        $('#dge-pc,#dge-ma').css('opacity',1)

html_escape = (str) ->
    $('<div/>').text(str).html()

class WithoutBackend
    constructor: (@settings, @process_dge_data) ->
        $('.conditions').hide()
        $('.config').hide()
        $('a.show-r-code').hide()

    request_init_data: () ->
        start_loading()
        d3.text(@settings.csv_file, "text/csv", (err,dat) =>
            log_info("Downloaded csv")
            log_debug("Downloaded csv",dat,err)
            if err
                log_error(err)
                return

            if settings.csv_format
               data = d3.csv.parse(dat)
            else
               data = d3.tsv.parse(dat)
            @process_dge_data(data, settings.columns)

            done_loading()
        )

    request_kegg_data: (callback) ->
        log_error("Get KEGG data not supported without backend")

class WithBackendNoAnalysis
    constructor: (@settings, @process_dge_data) ->
        # Ensure we have been configured!
        if @settings.fc_columns.length == 0
            window.location = config_url()

        $('.conditions').hide()
        $('a.show-r-code').hide()
        if @settings['locked']
            $('a.config').hide()
        else
            $('a.config').show()
            # TODO - factor out this and the one below
            $('a.config').attr('href', config_url())

    _script: (params) ->
        "r-json.cgi?code=#{window.my_code}&#{params}"

    request_kegg_data: (callback) ->
        d3.tsv(@_script('query=kegg_titles'), (err,ec_data) ->
            log_info("Downloaded kegg : rows=#{ec_data.length}")
            log_debug("Downloaded kegg : rows=#{ec_data.length}",ec_data,err)
            callback(ec_data)
        )

    request_init_data: () ->
        start_loading()
        d3.text(@_script("query=csv"), (err, dat) =>
            if err
                log_error(err)
                return

            if settings.csv_format
               data = d3.csv.parse(dat)
            else
               data = d3.tsv.parse(dat)

            log_info("Downloaded DGE : rows=#{data.length}")
            log_debug("Downloaded DGE : rows=#{data.length}",data,err)
            done_loading()

            data_cols = settings.info_columns.map((n) -> {idx: n, name: n, type: 'info' })
            data_cols.push({idx: '_dummy', type: 'primary', name:settings.primary_name})
            settings.fc_columns.forEach((n) ->
                data_cols.push({idx: n, type: 'fc', name: n})
            )
            data_cols.push({idx: settings.fdr_column, name: settings.fdr_column, type: 'fdr'})
            data_cols.push({idx: settings.avg_column, name: settings.avg_column, type: 'avg'})
            if settings.ec_column?
                data_cols.push({idx: settings.ec_column, name: 'EC', type: 'ec'})

            @process_dge_data(data, data_cols)
        )



class WithBackendAnalysis
    constructor: (@settings, @process_dge_data) ->
        # Ensure we have been configured!
        if @settings.replicates.length == 0
            window.location = config_url()

        $('.conditions').show()
        $('a.show-r-code').show()
        if @settings['locked']
            $('a.config').hide()
        else
            $('a.config').show()
            $('a.config').attr('href', config_url())

    _script: (params) ->
        "r-json.cgi?code=#{window.my_code}&#{params}"

    request_init_data: () ->
        @_init_condition_selector()

    request_kegg_data: (callback) ->
        d3.tsv(@_script('query=kegg_titles'), (err,ec_data) ->
            log_info("Downloaded kegg : rows=#{ec_data.length}")
            log_debug("Downloaded kegg : rows=#{ec_data.length}",ec_data,err)
            callback(ec_data)
        )

    request_dge_data: (columns) ->
        return if columns.length <= 1

        # load csv file and create the chart
        req = @_script("query=dge&fields=#{JSON.stringify columns}")
        start_loading()
        d3.csv(req, (err, data) =>
            log_info("Downloaded DGE : rows=#{data.length}")
            log_debug("Downloaded DGE : rows=#{data.length}",data,err)
            done_loading()

            data_cols = settings.info_columns.map((n) -> {idx: n, name: n, type: 'info' })
            pri=true
            columns.forEach((n) ->
                typ = if pri then 'primary' else 'fc'
                data_cols.push({idx: n, type: typ, name: n})
                pri=false
            )
            data_cols.push({idx: 'adj.P.Val', name: 'FDR', type: 'fdr'})
            data_cols.push({idx: 'AveExpr', name: 'AveExpr', type: 'avg'})
            if settings.ec_column?
                data_cols.push({idx: settings.ec_column, name: 'EC', type: 'ec'})
            settings.replicates.forEach(([name,reps]) ->
                reps.forEach((rep) ->
                    data_cols.push({idx: rep, name: rep, type: 'count', parent: name})
                )
            )

            @process_dge_data(data, data_cols)

            # Disable backend clustering for now - FIXME
            # if data.length<5000
            #     req = @_script("query=clustering&fields=#{JSON.stringify columns}")
            #     d3.csv(req, (err, data) ->
            #         log_info("Downloaded clustering : rows=#{data.length}",data,err)
            #         heatmap.set_order(data.map((d) -> d.id))
            #         heatmap.schedule_update()
            #     )
        )

    request_r_code: (callback) ->
        columns = @_get_selected_cols()
        req = @_script("query=dge_r_code&fields=#{JSON.stringify columns}")
        d3.text(req, (err,data) ->
            log_debug("Downloaded R Code : len=#{data.length}",data,err)
            callback(data)
        )


    _select_sample: (e) ->
        el = $(e.target).parent('div')
        $(el).toggleClass('selected')
        @_update_samples()

    _get_selected_cols: () ->
        cols = []
        # Create a list of conditions that are selected
        $('#files div.selected').each( (i, n) =>
            rep_name = $(n).data('rep')
            cols.push(rep_name)
        )
        cols

    _update_samples: () ->
        @request_dge_data(@_get_selected_cols())

    _init_condition_selector: () ->
        init_select = @settings['init_select'] || []
        $.each(@settings.replicates, (i, rep) =>
            name = rep[0]
            div = $("<div class='rep_#{i}'>"+
                    "  <a class='file' href='#' title='Select this condition' data-placement='right'>#{name}</a>"+
                    "</div>")
            $("#files").append(div)
            div.data('rep',name)

            # Select those in init_select
            if name in init_select
                div.addClass('selected')
        )
        $("#files a.file").click((e) => @_select_sample(e))
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

parcoords = null
ma_plot = null
expr_plot = null    # Actually parcoords OR ma_plot depending which is active

gene_table = null
kegg = null
heatmap = null

g_data = null
g_backend = null
requested_kegg = false


show_ave_fc = false
show_counts = false
fdrThreshold = 1
fcThreshold = 0
searchStr = ""
kegg_filter = []
h_runfilters = null

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

gene_table_dblclick = (item) ->
    cols = g_data.columns_by_type(['info'])
    if cols.length>0
        str = item[cols[0].idx]
        window.open("http://ensembl.org/Multi/Search/Results?q=#{str};site=ensembl")
        window.focus()

activate_parcoords = () ->
    expr_plot = parcoords
    $('#dge-ma').hide()
    $('#dge-pc').show()
    $('#select-pc').addClass('active')
    $('#select-ma').removeClass('active')
    $('.ma-fc-col-opt').hide()
    update_data()

activate_ma_plot = () ->
    expr_plot = ma_plot
    $('#dge-pc').hide()
    $('#dge-ma').show()
    $('#select-ma').addClass('active')
    $('#select-pc').removeClass('active')
    $('.ma-fc-col-opt').show()
    update_data()

init_charts = () ->
    parcoords = new ParCoords({elem: '#dge-pc', filter: expr_filter})
    ma_plot = new MAPlot({elem: '#dge-ma', filter: expr_filter})

    gene_table = new GeneTable({elem: '#grid', elem_info: '#grid-info', sorter: do_sort, mouseover: gene_table_mouseover, mouseout: gene_table_mouseout, dblclick: gene_table_dblclick, filter: gene_table_filter})
    kegg = new Kegg({elem: 'div#kegg-image', mouseover: kegg_mouseover, mouseout: () -> expr_plot.unhighlight()})

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
    if show_counts
        count_columns = g_data.assoc_column_by_type('count',column.name)
        counts = count_columns.map((c) -> row[c.idx])
        countStr = "<span class='counts'>(#{counts})</span>"
    "<div class='#{colour}'>#{n.toFixed(2)}#{countStr}</div>"


gene_table_filter = (item) ->
    return true if searchStr == ""
    for col in g_data.columns_by_type('info')
        str = item[col.idx]
        return true if str && str.toLowerCase().indexOf(searchStr)>=0
    false

# Filter to decide which rows to plot on the parallel coordinates widget
expr_filter = (row) ->
    if fcThreshold>0
        keep = false
        col_type = 'fc_calc'
        for col in g_data.columns_by_type(col_type)
            if Math.abs(row[col.idx]) > fcThreshold
                keep = true
                break
        return false if !keep

    pval_col = g_data.columns_by_type('fdr')[0]
    return false if row[pval_col.idx] > fdrThreshold

    if kegg_filter.length>0
        ec_col = g_data.column_by_type('ec')
        return row[ec_col] in kegg_filter

    true

init_search = () ->
    $(".tab-search input").keyup (e) ->
                    Slick.GlobalEditorLock.cancelCurrentEdit()
                    this.value = "" if e.which == 27     # Clear on "Esc"
                    searchStr = this.value.toLowerCase()
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
    $('#fc-relative').change((e) ->
        update_data()
    )
    $('#ma-fc-col').change((e) ->
        update_data()
    )
    $('#show-counts-cb').on("click", (e) ->
        update_flags()
        gene_table.invalidate()
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
    else
        $('.show-counts-opt').show()


    if g_data.columns_by_type(['fc','primary']).length>2
        activate_parcoords()
    else
        activate_ma_plot()

update_flags = () ->
    show_counts = $('#show-counts-cb').is(":checked")

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

    set_gene_table(g_data.get_data())

    # Update the heatmap
    heatmap.schedule_update(g_data.get_data())
    heatmap.update_columns(dims, extent, pval_col)

    # Ensure the brush callbacks are called (updates heatmap & table)
    expr_plot.brush()

show_r_code = () ->
    g_backend.request_r_code((d) ->
        $('div#code-modal .modal-body pre').text(d)
        $('div#code-modal').modal()
    )

init_page = (use_backend) ->
    g_data = new GeneData([],[])

    if use_backend
        if settings.analyze_server_side
            g_backend = new WithBackendAnalysis(settings, process_dge_data)
        else
            g_backend = new WithBackendNoAnalysis(settings, process_dge_data)
    else
        # TODO check existence of necessary files!
        g_backend = new WithoutBackend(settings, process_dge_data)

    $(".exp-name").text(settings.name || "Unnamed")

    fdrThreshold = settings['fdrThreshold'] if settings['fdrThreshold'] != undefined
    fcThreshold  = settings['fcThreshold']  if settings['fcThreshold'] != undefined

    $("select#kegg").change(kegg_selected)

    $('#select-pc a').click(() -> activate_parcoords())
    $('#select-ma a').click(() -> activate_ma_plot())
    $('a.show-r-code').click(() -> show_r_code())

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
        # TODO.  Pull out this 'script' def.  And the 2 in the classes above (and perhaps the one in config.coffee)
        script = (params) -> "r-json.cgi?code=#{window.my_code}" + if params then "&#{params}" else ""
        window.my_code = code
        $.ajax({
            type: "GET",
            url: script("query=settings"),
            dataType: 'json'
        }).done((json) ->
            window.settings = json
            init_page(true)
         ).fail((x) ->
            log_error "Failed to get settings!"
        )


$(document).ready(() -> setup_nav_bar() )
$(document).ready(() -> init() )
$(document).ready(() -> $('[title]').tooltip())
