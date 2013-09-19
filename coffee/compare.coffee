
num_loading = 0
start_loading = () ->
    num_loading += 1
    $('#loading').show()
    $('#dge-pc').css('opacity',0.4)
done_loading = () ->
    num_loading -= 1
    if num_loading==0
        $('#loading').hide()
        $('#dge-pc').css('opacity',1)

html_escape = (str) ->
    $('<div/>').text(str).html()

class WithoutBackend
    constructor: (@settings, @process_dge_data) ->
        $('.conditions').hide()
        $('.config').hide()

    request_init_data: () ->
        start_loading()
        d3.text(@settings.csv_file, "text/csv", (err,dat) =>
            msg_info("Downloaded csv",dat,err)
            if err
                msg_error(err)
                return

            if settings.csv_format
               data = d3.csv.parse(dat)
            else
               data = d3.tsv.parse(dat)
            @process_dge_data(data, settings.columns)

            done_loading()
        )

    request_kegg_data: (callback) ->
        msg_error("Get KEGG data not supported without backend")

class WithBackend
    constructor: (@settings, @process_dge_data) ->
        $('.conditions').show()
        if @settings['locked']
            $('a.config').hide()
        else
            $('a.config').show()
            $('a.config').attr('href', @_script("query=config"))

    request_init_data: () ->
        @_init_condition_selector()

    request_kegg_data: (callback) ->
        d3.tsv(@_script('query=kegg_titles'), (err,ec_data) ->
            msg_info("Downloaded kegg : rows=#{ec_data.length}",ec_data,err)
            callback(ec_data)
        )

    request_dge_data: (columns) ->
        return if columns.length <= 1

        # load csv file and create the chart
        req = @_script("query=dge&fields=#{JSON.stringify columns}")
        start_loading()
        d3.csv(req, (err, data) =>
            msg_info("Downloaded DGE : rows=#{data.length}",data,err)
            done_loading()

            data_cols = settings.info_columns.map((n) -> {idx: n, name: n, type: 'info' })
            pri=true
            columns.forEach((n) ->
                typ = if pri then 'primary' else 'fc'
                data_cols.push({idx: n, type: typ, name: n})
                pri=false
            )
            data_cols.push({idx: 'adj.P.Val', name: 'FDR', type: 'fdr'})
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
            #         msg_info("Downloaded clustering : rows=#{data.length}",data,err)
            #         heatmap.set_order(data.map((d) -> d.id))
            #         heatmap.schedule_update()
            #     )
        )

    _script: (params) ->
        "r-json.cgi?code=#{window.my_code}&#{params}"

    _select_sample: (e) ->
        el = $(e.target).parent('div')
        $(el).toggleClass('selected')
        @_update_samples()

    _update_samples: () ->
        cols = []
        # Create a list of conditions that are selected
        $('#files div.selected').each( (i, n) =>
            rep_name = $(n).data('rep')
            cols.push(rep_name)
        )
        @request_dge_data(cols)

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
  .domain([0,1])
  .range(["brown", "steelblue"])
  .interpolate(d3.interpolateLab)

colour_cat20 = d3.scale.category20().domain([1..20])
colour_by_ec = (ec_col) ->
    (row) -> colour_cat20(row[ec_col])

colour_by_pval = (col) ->
    (d) -> blue_to_brown(d[col])

parcoords = null
gene_table = null
kegg = null
heatmap = null
maplot = null

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
    parcoords.highlight(rows)

# highlight parallel coords (and/or kegg)
gene_table_mouseover = (item) ->
    parcoords.highlight([item])
    ec_col = g_data.column_by_type('ec')
    kegg.highlight(item[ec_col])

gene_table_mouseout = () ->
    parcoords.unhighlight()
    $('#gene-info').html('')
    kegg.unhighlight()

gene_table_dblclick = (item) ->
    window.open("http://www.ncbi.nlm.nih.gov/protein?term=#{item.id}", '_blank')
    window.focus()

init_charts = () ->
    parcoords = new ParCoords({elem: '#dge-pc', filter: parcoords_filter})
    $('#dge-pc').hide()
    maplot = new MAPlot({elem: '#dge-ma', filter: parcoords_filter})
    gene_table = new GeneTable({elem: '#grid', elem_info: '#grid-info', sorter: do_sort, mouseover: gene_table_mouseover, mouseout: gene_table_mouseout, dblclick: gene_table_dblclick, filter: gene_table_filter})
    kegg = new Kegg({elem: 'div#kegg-image', mouseover: kegg_mouseover, mouseout: () -> parcoords.unhighlight()})

    # update grid on brush
    parcoords.on("brush", (d) ->
        gene_table.set_data(d)
        heatmap.schedule_update(d)
    )

    #Heatmap
    heatmap = new Heatmap(
        elem: '#heatmap'
        width: $('.container').width()
        mouseover: (d) ->
            parcoords.highlight([d])
            msg = ""
            for col in g_data.columns_by_type(['info'])
              msg += "<span class='lbl'>#{col.name}: </span><span>#{d[col.idx]}</span>"
            $('#heatmap-info').html(msg)
        mouseout:  (d) ->
            parcoords.unhighlight()
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
parcoords_filter = (row) ->
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
               h_runfilters = window.setTimeout((() -> parcoords.brush()), 10)
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
               h_runfilters = window.setTimeout((() -> parcoords.brush()), 10)
               fcThreshold = v
    )
    $('#fc-relative').change((e) ->
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

process_counts_data = (dat) ->
    # If there is an ec column, fill in the kegg pull down

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

process_dge_data = (data, columns) ->
    g_data = new GeneData(data, columns)

    # Setup FC "relative" pulldown
    opts = ""
    for col,i in g_data.columns_by_type(['fc','primary'])
        opts += "<option value='#{i}'>#{html_escape col.name}</option>"
    opts += "<option value='-1' selected>Average</option>"
    $('select#fc-relative').html(opts)

    if g_data.column_by_type('ec') == null
        $('.kegg-filter').hide()
    else if !requested_kegg
        g_backend.request_kegg_data(process_kegg_data)

    if g_data.columns_by_type('count').length == 0
        $('.show-counts-opt').hide()
    else
        $('.show-counts-opt').show()

    update_data()

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
    parcoords.update_data(g_data.get_data(), dims, extent, color)
    maplot.update_data(g_data.get_data(), g_data.columns_by_type('expr'), color)

    set_gene_table(g_data.get_data())

    # Update the heatmap
    heatmap.update_columns(dims, extent, pval_col)
    heatmap.schedule_update(g_data.get_data())

    # Ensure the parcoords brush callbacks are called (updates heatmap & table)
    parcoords.brush()

init = () ->
    g_data = new GeneData([],[])

    if window.use_backend
        g_backend = new WithBackend(settings, process_dge_data)
    else
        g_backend = new WithoutBackend(settings, process_dge_data)

    $(".exp-name").text(settings.name || "Unnamed")

    fdrThreshold = settings['fdrThreshold'] if settings['fdrThreshold'] != undefined
    fcThreshold  = settings['fcThreshold']  if settings['fcThreshold'] != undefined

    $("select#kegg").change(kegg_selected)

    init_charts()
    init_search()
    init_slider()
    init_download_link()

    g_backend.request_init_data()

$(document).ready(() -> init() )
$(document).ready(() -> $('[title]').tooltip())
