script = (params) -> "r-json.cgi?code=#{window.my_code}&#{params}"

select_primary = (e) ->
           el = $(e.target).parent('div')
           $('#files .primary').removeClass('primary')
           $(el).addClass('primary')
           $(el).removeClass('selected')
           select_sample(e)

select_sample = (e) ->
           el = $(e.target).parent('div')
           $(el).toggleClass('selected')
           $('#files div:not(.selected)').removeClass('primary')
           if $('#files div.primary').length==0
                $('#files div.selected:first').addClass('primary')
           update_samples()

update_samples= () ->
           cols = []
           $('#files div.selected').each (i, n) ->
                rep_id = $(n).data('rep')
                if $(n).parent('div').hasClass('primary')
                    cols.unshift(rep_id)
                else
                    cols.push(rep_id)
           request_data(cols)

blue_to_brown = d3.scale.linear()
  .domain([0,1])
  .range(["brown", "steelblue"])
  .interpolate(d3.interpolateLab)

pval_col = 'adj.P.Val'
ave_expr_col = 'AveExpr'

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

ec_number = (id) ->
    ec = g_data.get_ec_value(id)
    return "" if !ec
    ec

parcoords = null
dataView = null
grid = null
kegg = null
heatmap = null

info_columns = []   # Columns used to display info about the genes
ec_column = null    # Column (if any) with EC number
column_names = []   # Column names
counts_lookup = []  # Hash from column name to replicate raw counts

g_data = null

kegg_mouseover = (obj) ->
    ec = obj.id
    d = []
    for row in g_data.get_data()
        d.push(row) if ec_number(row.id) == ec
    parcoords.highlight(d)
    #gridUpdateData(d)

init_chart = () ->
    parcoords = new ParCoords({elem: '#dge-pc', pcFilter: pcFilter})

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
      #console.log("onRowCountChanged",e,args)
      grid.updateRowCount()
      grid.render()
      update_info()
    )

    dataView.onRowsChanged.subscribe( (e, args) ->
      #console.log("onRowsChanged")
      grid.invalidateRows(args.rows)
      grid.render()
    )

    # highlight row in chart
    grid.onMouseEnter.subscribe( (e,args) ->
        i = grid.getCellFromEvent(e).row
        d = dataView.getItem(i)
        parcoords.highlight([d])
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
            for k in info_columns
              msg += "<span class='lbl'>#{k}: </span><span>#{d[k]}</span>"
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
    cname = args.sortCol.field
    dataView.sort((r1,r2) ->
      r = 0
      x=r1[cname]; y=r2[cname]
      if fc_col(cname) || ave_fc_col(cname)
        r = comparer(Math.abs(x), Math.abs(y))
      else if cname == pval_col || expr_col(cname)
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
    #console.log("Added #{data.length} items")

grid_include = (key) -> (!show_ave_fc && fc_col(key)) ||
                        (show_ave_fc && ave_fc_col(key)) ||
                        key==pval_col

update_grid = (data) ->
    column_keys = g_data.columns_by_type('info')
    column_keys = column_keys.concat(g_data.columns_by_type(pval_col))
    column_keys = column_keys.concat(g_data.columns_by_type(if show_ave_fc then 'afc' else 'fc'))
    columns = column_keys.map((col) ->
        id: col.idx
        name: col.name
        field: col.idx
        sortable: true
        formatter: (i,c,val,m,row) ->
            if fc_col(m.name) || ave_fc_col(m.name)
                #console.log(i,c,val,m,row)
                fc_div(val, m.name, row)
            else if expr_col(m.name)
                Number(val).toFixed(2)
            else if m.name==pval_col
                pval=Number(val)
                if pval<0.01 then pval.toExponential(2) else pval.toFixed(2)
            else
                val
    )

    gridUpdateData(data, columns)

fc_div = (n, column, row) ->
    colour = if n>0.1 then "pos" else if n<-0.1 then "neg" else ""
    colName = column.substring(column.indexOf(' ')+1)
    countStr = ""
    #console.log(n,column,row)
    if show_counts
        counts = g_data.get_counts(colName)
        countStr = "<span class='counts'>(#{counts})</span>"
    "<div class='#{colour}'>#{n.toFixed(2)}#{countStr}</div>"


expr_col   = (k) -> k.indexOf("ABS.")==0
fc_col     = (k) -> k.indexOf("FC ")==0
ave_fc_col = (k) -> k.indexOf("AFC ")==0

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

tabFilter = (item) ->
    return true if searchStr == ""
    for col in info_columns
        str = item[col]
        return true if str && str.toLowerCase().indexOf(searchStr)>=0
    false

pcFilter = (item) ->
    if fcThreshold>0
        keep = false
        for k,v of item
          if ((!show_ave_fc && fc_col(k)) || (show_ave_fc && ave_fc_col(k))) && Math.abs(v) > fcThreshold
            keep = true
            break
        return false if !keep

    return false if item[pval_col] > fdrThreshold
    return false if annot_genes_only && !g_data.get_ec_value(id)

    # TODO - this is a very inefficient place to filter on ec.  Disabled for now.  Remove?
    # ec_codes = $('.ec-filter .selected').map((i,d) -> $(d).data('code'))
    # if ec_codes.length>0
    #     return ec_code(item.id) in ec_codes
    #
    if kegg_filter.length>0
        return ec_number(item.id) in kegg_filter

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
               h_runfilters = window.setTimeout(parcoords.brush, 10)
               fdrThreshold = v
    )
    new Slider(
          id: "#fcSlider"
          input_id: "input.fc-fld"
          step_values: (Number(x.toFixed(2)) for x in [0..10] by 0.01)
          val: fcThreshold
          validator: (v) ->
             n = Number(v)
             !(isNaN(n) || n<0)
          on_change: (v) ->
             Slick.GlobalEditorLock.cancelCurrentEdit()
             if (fcThreshold != v)
               window.clearTimeout(h_runfilters)
               h_runfilters = window.setTimeout(parcoords.brush, 10)
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
    for row in g_data.get_data()
        ec = ec_number(row.id)
        continue if !ec
        first=true
        for k,v of row
            if fc_col(k)
                #console.log(first,k)
                if first
                    first = false
                    continue
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

request_init_data = () ->
    d3.text(script("query=counts"), "text/csv", (dat,err) ->
        if err
            $('div.container').text("ERROR : #{err}")
            return
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
            d3.tsv(script('query=kegg_titles'), (ec_data) ->
                opts = "<option value=''>--- No pathway selected ---</option>"

                have_ec = {}
                dat = g_data.get_data()
                for row of dat
                    have_ec[row[ec_col]]=1

                ec_data.forEach (row) ->
                    num=0
                    for ec in row.ec.split(" ").filter((s) -> s.length>0)
                        num++ if have_ec[ec]
                    if num>0
                        opts += "<option value='#{row.code}'>#{row.title} (#{num})</option>"
                $('select#kegg').html(opts)
            )
    )

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

request_data = (columns) ->
    return if columns.length <= 1

    # load csv file and create the chart
    req = script("query=dge&fields=#{JSON.stringify columns}")
    start_loading()
    d3.csv(req, (data) ->
        done_loading()

        expression_cols = {}
        columns = [{is_id: true, column_idx: 'id'}]
        pri_col=null
        d3.keys(data[0]).forEach (k, i) ->
            if expr_col(k)
                if pri_col==null
                     pri_col = k
                rep_num = k.substring(4)-1
                if rep_num != settings.replicates[rep_num][0]
                    console.log("BAD Replicate column")
                name = settings.replicate_names[rep_num]
                columns.push({column_idx: k, name: name, type: 'expr', numeric: true})
                columns.push({name:"FC #{name}", type: 'fc', func: (r) -> r[k] - r[pri_col]})
                columns.push({name:"AFC #{name}", type: 'afc', func: (r) -> r[k] - r[ave_expr_col]})
            else if k==pval_col
                columns.push({column_idx: k, name:k, type: 'pval', numeric: true})
            else
                columns.push({column_idx: k, name:k})

        g_data.add_data('dge', data, columns)

        update_data()
    )

    req = script("query=clustering&fields=#{JSON.stringify columns}")
    d3.csv(req, (data) ->
      heatmap.set_order(data.map((d) -> d.id))
      heatmap.redraw()
    )

update_data = () ->
    update_flags()

    dims = g_data.columns_by_type(if show_ave_fc then 'afc' else 'fc').map (c) -> c.idx
    ec_col = g_data.column_by_type('ec')
    pval_col = g_data.column_by_type('pval')
    color = if pval_colour then colour_by_pval(pval_col) else colour_by_ec(ec_col)
    parcoords.update_data(g_data.get_data(), dims, color)

    update_grid(g_data.get_data())

    heatmap.update_columns(dims, extent, pval_col)
    heatmap.schedule_update(data)

init_condition_selector = () ->
    $.each(settings.replicates, (i, rep) ->
        name = settings.replicate_names[i]
        div = $("<div class='rep_#{i}'>"+
                "  <a class='file' href='#' title='Select this condition' data-placement='right'>#{name}</a>"+
                "  <a class='pri' href='#' title='Make primary condition' data-placement='right'>pri</a>" +
                "</div>")
        $("#files").append(div)
        div.data('rep',i)
    )
    $("#files a.file").click(select_sample)
    $("#files a.pri").click(select_primary)

    # Select some samples
    init_select = settings['init_select'] || []
    $.each(init_select, (i,sel) ->
        $("div[class='rep_#{i}']").addClass('selected')
    )
    $('#files div.selected:first').addClass('primary')
    update_samples()

init = () ->
    g_data = new DataContainer()
    if settings.id_column < 0
        window.location = script("query=config")
    info_columns = settings['info_columns'] || []
    ec_column = settings['ec_column']
    column_names = settings['column_names']

    if settings['locked']
        $('a.config').hide()
    else
        $('a.config').attr('href', script("query=config"))

    $(".exp-name").text(settings.name || "Unnamed")

    fdrThreshold = settings['fdrThreshold'] if settings['fdrThreshold'] != undefined
    fcThreshold  = settings['fcThreshold']  if settings['fcThreshold'] != undefined

    $("select#kegg").change(kegg_selected)
    init_chart()
    init_search()
    init_slider()
    init_download_link()
    request_init_data()

    init_condition_selector()

$(document).ready(() -> init() )
$(document).ready(() -> $('[title]').tooltip())
