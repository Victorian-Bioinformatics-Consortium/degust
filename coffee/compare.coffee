
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

class WithoutBackend
    constructor: (@settings, @process_dge_data) ->
        $('.conditions').hide()
        $('.config').hide()

    request_init_data: (callback) ->
        d3.text(@settings.csv, "text/csv", (dat,err) =>
            msg_info("Downloaded csv",dat,err)
            if err
                msg_error(err)
                return
            callback(dat)

            @process_dge_data(d3.csv.parse(dat))     # FIXME: Need a better abstraction than repeating this!
        )

    request_kegg_data: (callback) ->
        msg_error("Get KEGG data not supported without backend")

class WithBackend
    constructor: (@settings, @process_dge_data) ->
        if @settings.id_column < 0
            window.location = @_script("query=config")

        $('.conditions').show()
        if @settings['locked']
            $('a.config').hide()
        else
            $('a.config').show()
            $('a.config').attr('href', @_script("query=config"))
        @_init_condition_selector()

    request_init_data: (callback) ->
        d3.text(@_script("query=counts"), "text/csv", (dat,err) =>
            msg_info("Downloaded counts",dat,err)
            if err
                msg_error(err)
                return
            callback(dat)
            @have_counts = true
            if @after_counts
                @after_counts()
            @after_counts = null
        )

    request_kegg_data: (callback) ->
        d3.tsv(@_script('query=kegg_titles'), (ec_data,err) ->
            msg_info("Downloaded kegg : rows=#{ec_data.length}",ec_data,err)
            callback(ec_data)
        )

    request_dge_data: (columns) ->
        return if columns.length <= 1

        #Ugly hack to ensure "Counts" are done before "DGE" - FIXME
        if !@have_counts
            @after_counts = () => @request_dge_data(columns)
            return

        # load csv file and create the chart
        req = @_script("query=dge&fields=#{JSON.stringify columns}")
        start_loading()
        d3.csv(req, (data, err) =>
            msg_info("Downloaded DGE : rows=#{data.length}",data,err)
            done_loading()
            @process_dge_data(data)

            req = @_script("query=clustering&fields=#{JSON.stringify columns}")
            d3.csv(req, (data,err) ->
                msg_info("Downloaded clustering : rows=#{data.length}",data,err)
                heatmap.set_order(data.map((d) -> d.id))
                heatmap.redraw()
            )
        )

    _script: (params) ->
        "r-json.cgi?code=#{window.my_code}&#{params}"

    _select_primary: (e) ->
        el = $(e.target).parent('div')
        $('#files .primary').removeClass('primary')
        $(el).addClass('primary')
        $(el).removeClass('selected')
        @_select_sample(e)

    _select_sample: (e) ->
        el = $(e.target).parent('div')
        $(el).toggleClass('selected')
        $('#files div:not(.selected)').removeClass('primary')
        if $('#files div.primary').length==0
            $('#files div.selected:first').addClass('primary')
        @_update_samples()

    _update_samples: () ->
        cols = []
        # Create a list of conditions that are selected, ensure the "primary" condition is first
        $('#files div.selected').each( (i, n) =>
            rep_id = $(n).data('rep')
            if $(n).hasClass('primary')
                cols.unshift(rep_id)
            else
                cols.push(rep_id)
        )
        @request_dge_data(cols)

    _init_condition_selector: () ->
        $.each(@settings.replicates, (i, rep) =>
            name = @settings.replicate_names[i]
            div = $("<div class='rep_#{i}'>"+
                    "  <a class='file' href='#' title='Select this condition' data-placement='right'>#{name}</a>"+
                    "  <a class='pri' href='#' title='Make primary condition' data-placement='right'>pri</a>" +
                    "</div>")
            $("#files").append(div)
            div.data('rep',i)
        )
        $("#files a.file").click((e) => @_select_sample(e))
        $("#files a.pri").click((e) => @_select_primary(e))

        # Select some samples
        init_select = @settings['init_select'] || []
        $.each(init_select, (i,sel) ->
            $("div[class='rep_#{sel}']").addClass('selected')
        )
        $('#files div.selected:first').addClass('primary')
        @_update_samples()



blue_to_brown = d3.scale.linear()
  .domain([0,1])
  .range(["brown", "steelblue"])
  .interpolate(d3.interpolateLab)

colour_cat20 = d3.scale.category20().domain([1..20])

unknown_colour = 16
ec_code = (id) ->
    ec = g_data.get_ec_value(id)
    return unknown_colour if !ec
    Number(ec[0])

colour_by_ec = (col) ->
    (d) -> colour_cat20(ec_code(d.id))

colour_by_pval = (col) ->
    (d) -> blue_to_brown(d[col])

parcoords = null
dataView = null
grid = null
kegg = null
heatmap = null

g_data = null
g_backend = null


expr_col   = (k) -> k.indexOf("ABS.")==0             # Remove?

show_ave_fc = false
show_counts = false
plot_avg_exp = false
annot_genes_only = false
pval_colour = false
fdrThreshold = 1
fcThreshold = 0
searchStr = ""
kegg_filter = []
h_runfilters = null

kegg_mouseover = (obj) ->
    ec = obj.id
    d = []
    ec_col = g_data.column_by_type('ec')
    return if ec_col==null
    for row in g_data.get_data()
        d.push(row) if row[ec_col] == ec
    parcoords.highlight(d)
    #gridUpdateData(d)

init_charts = () ->
    parcoords = new ParCoords({elem: '#dge-pc', pcFilter: parcoordsFilter})

    options =
      enableCellNavigation: true
      enableColumnReorder: false
      multiColumnSort: false
      forceFitColumns: true

    dataView = new Slick.Data.DataView()
    grid = new Slick.Grid("#grid", dataView, [], options)
    kegg = new Kegg({elem: 'div#kegg-image', mouseover: kegg_mouseover, mouseout: () -> parcoords.unhighlight()})

    dataView.setFilter(tabFilter)

    dataView.onRowCountChanged.subscribe( (e, args) ->
      grid.updateRowCount()
      grid.render()
      update_info()
    )

    dataView.onRowsChanged.subscribe( (e, args) ->
      grid.invalidateRows(args.rows)
      grid.render()
    )

    # highlight row in chart
    grid.onMouseEnter.subscribe( (e,args) ->
        i = grid.getCellFromEvent(e).row
        d = dataView.getItem(i)
        parcoords.highlight([d])
        ec_col = g_data.column_by_type('ec')
        kegg.highlight(d[ec_col])
    )
    grid.onMouseLeave.subscribe( (e,args) ->
        parcoords.unhighlight()
        $('#gene-info').html('')
        kegg.unhighlight()
    )
    grid.onDblClick.subscribe( (e,args) ->
          feature = grid.getDataItem(args.row).id
          window.open("http://www.ncbi.nlm.nih.gov/protein?term=#{feature}", '_blank')
          window.focus()
    )

    grid.onSort.subscribe( (e,args) -> do_sort(args) )
    grid.onViewportChanged.subscribe( (e,args) -> update_info() )

    # update grid on brush
    parcoords.on("brush", (d) ->
        gridUpdateData(d)
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


update_info = () ->
    view = grid.getViewport()
    btm = d3.min [view.bottom, dataView.getLength()]
    $('#grid-info').html("Showing #{view.top}..#{btm} of #{dataView.getLength()}")

comparer = (x,y) -> (if x == y then 0 else (if x > y then 1 else -1))

do_sort = (args) ->
    column = g_data.column_by_idx(args.sortCol.field)
    dataView.sort((r1,r2) ->
        r = 0
        x=r1[column.idx]; y=r2[column.idx]
        if column.type in ['fc','afc']
            r = comparer(Math.abs(x), Math.abs(y))
        else if column.type in ['pval']
            r = comparer(x, y)
        else
            r = comparer(x,y)
        r * (if args.sortAsc then 1 else -1)
    )

gridUpdateData = (data, columns) ->
    grid.setColumns([]) if columns
    dataView.beginUpdate()
    dataView.setItems(data)
    dataView.reSort()
    dataView.endUpdate()
    grid.setColumns(columns) if columns

update_grid = (data) ->
    column_keys = g_data.columns_by_type(['info','pval'])
    column_keys = column_keys.concat(g_data.columns_by_type(if show_ave_fc then 'afc' else 'fc'))
    columns = column_keys.map((col) ->
        id: col.idx
        name: col.name
        field: col.idx
        sortable: true
        formatter: (i,c,val,m,row) ->
            if col.type in ['fc','afc']
                fc_div(val, col, row)
            else if col.type in ['pval']
                if val<0.01 then val.toExponential(2) else val.toFixed(2)
            else
                val
    )

    gridUpdateData(data, columns)

fc_div = (n, column, row) ->
    colour = if n>0.1 then "pos" else if n<-0.1 then "neg" else ""
    countStr = ""
    if show_counts
        count_columns = g_data.assoc_column_by_type('counts',column.parent)
        counts = count_columns.map((c) -> row[c.idx])
        countStr = "<span class='counts'>(#{counts})</span>"
    "<div class='#{colour}'>#{n.toFixed(2)}#{countStr}</div>"


tabFilter = (item) ->
    return true if searchStr == ""
    for col in settings.info_columns
        str = item[col]
        return true if str && str.toLowerCase().indexOf(searchStr)>=0
    false

# Filter to decide which rows to plot on the parallel coordinates widget
parcoordsFilter = (row) ->
    if fcThreshold>0
        keep = false
        col_type = if show_ave_fc then 'afc' else 'fc'
        for col in g_data.columns_by_type(col_type)
            if Math.abs(row[col.idx]) > fcThreshold
                keep = true
                break
        return false if !keep

    pval_col = g_data.columns_by_type('pval')[0]
    return false if row[pval_col.idx] > fdrThreshold
    #return false if annot_genes_only && !g_data.get_ec_value(id)

    if kegg_filter.length>0
        ec_col = g_data.column_by_type('ec')
        return row[ec_col] in kegg_filter

    true

init_search = () ->
    $(".tab-search input").keyup (e) ->
                    Slick.GlobalEditorLock.cancelCurrentEdit()
                    this.value = "" if e.which == 27     # Clear on "Esc"
                    searchStr = this.value.toLowerCase()
                    dataView.refresh()

init_download_link = () ->
    $('a#csv-download').on('click', (e) ->
        e.preventDefault()
        items = dataView.getItems()
        return if items.length==0
        keys = d3.keys(items[0])
        rows=items.map( (r) -> keys.map( (k) -> r[k] ) )
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
    $('#plot-avgfc-cb').on("click", (e) ->
        update_data()
    )
    $('#show-counts-cb').on("click", (e) ->
        update_flags()
        grid.invalidate()
    )
    $('#annot-genes-cb').on("click", (e) ->
        update_data()
    )
    $('#pval-col-cb').on("click", (e) ->
        update_data()
    )

calc_kegg_colours = () ->
    ec_dirs = {}
    ec_col = g_data.column_by_type('ec')
    return if ec_col==null
    fc_cols = g_data.columns_by_type('fc')[1..]
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

update_flags = () ->
    show_ave_fc = $('#plot-avgfc-cb').is(":checked")
    show_counts = $('#show-counts-cb').is(":checked")
    annot_genes_only = $('#annot-genes-cb').is(":checked")
    pval_colour = $('#pval-col-cb').is(":checked")

process_counts_data = (dat) ->
    data = null
    if settings.csv_format
       data = d3.csv.parseRows(dat)
    else
       data = d3.tsv.parseRows(dat)

    names = settings.column_names
    rep_names = settings.replicate_names
    columns = []
    columns.push({is_id: true, column_idx: settings.id_column, name: names[settings.id_column]})
    settings.info_columns.forEach (col) ->
        columns.push({column_idx: col, name: names[col], type: 'info'})

    settings.replicates.forEach (rep) ->
        rep[1].forEach (col) ->
            columns.push({type: 'counts', column_idx: col, name:names[col], parent: rep_names[rep[0]]})

    if settings.ec_column != undefined
        columns.push({type: 'ec', column_idx: settings.ec_column, name:"EC"})

    g_data.add_data('counts',data[1+settings.skip..], columns)

    # If there is an ec column, fill in the kegg pull down
    ec_col = g_data.column_by_type('ec')
    if ec_col == null
        $('.kegg-filter').hide()
    else
        g_backend.request_kegg_data(process_kegg_data)

process_kegg_data = (ec_data) ->
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

process_dge_data = (data) ->
    expression_cols = {}
    columns = [{is_id: true, column_idx: 'id'}]
    pri_col=null
    d3.keys(data[0]).forEach (k, i) ->
        if k=='adj.P.Val'     # FIXME - shouldn't be hardcoded!
            columns.push({column_idx: k, name:k, type: 'pval', numeric: true})
        else if k=='id'
            # Do nothing, already added
        else if expr_col(k)
            if pri_col==null
                 pri_col = k
            rep_num = k.substring(4)-1
            if rep_num != settings.replicates[rep_num][0]
                msg_error("BAD Replicate column",rep_num,settings.replicates)
            name = settings.replicate_names[rep_num]
            columns.push({column_idx: k, name: name, type: 'expr', numeric: true})
            columns.push({name:"FC #{name}", type: 'fc', parent: name, func: (r) -> r[k] - r[pri_col]})
            columns.push({name:"AFC #{name}", type: 'afc', parent: name, func: (r) -> r[k] - r['AveExpr']})  # FIXME - shouldn't have hardcoded column name
        else
            columns.push({column_idx: k, name:k})

    g_data.add_data('dge', data, columns)

    update_data()

# Called whenever the data is changed, or the "checkboxes" are modified
update_data = () ->
    update_flags()

    dims = g_data.columns_by_type(if show_ave_fc then 'afc' else 'fc')
    ec_col = g_data.column_by_type('ec')
    pval_col = g_data.column_by_type('pval')
    color = if pval_colour then colour_by_pval(pval_col) else colour_by_ec(ec_col)

    extent = ParCoords.calc_extent(g_data.get_data(), dims)
    parcoords.update_data(g_data.get_data(), dims, extent, color)

    update_grid(g_data.get_data())

    # Update the heatmap
    heatmap.update_columns(dims, extent, pval_col)
    heatmap.schedule_update(g_data.get_data())

    # Ensure the parcoords brush callbacks are called (updates heatmap & table)
    parcoords.brush()

init = () ->
    g_data = new DataContainer(settings)

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

    g_backend.request_init_data(process_counts_data)

$(document).ready(() -> init() )
$(document).ready(() -> $('[title]').tooltip())
