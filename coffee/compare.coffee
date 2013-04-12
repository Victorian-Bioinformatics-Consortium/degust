script = (params) -> "r-json.cgi?code=#{window.my_code}&#{params}"

select_primary = (e) ->
           el = $(e.target).parent('div')
           $('#files .primary').removeClass('primary')
           $(el).addClass('primary')
           $(el).removeClass('selected')
           select(e)

select_sample = (e) ->
           el = $(e.target).parent('div')
           $(el).toggleClass('selected')
           $('#files div:not(.selected)').removeClass('primary')
           if $('#files div.primary').length==0
                $('#files div.selected:first').addClass('primary')
           update_samples()

update_samples= () ->
           cols = []
           $('#files div.selected a.file').each (i, n) ->
                name = $(n).html()
                if $(n).parent('div').hasClass('primary')
                    cols.unshift(name)
                else
                    cols.push(name)
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
    return unknown_colour if !current_annot[id]
    ec = current_annot[id]['EC_NUMBER']
    Number(ec[0])

colour_by_ec = (d) -> colour_cat20(ec_code(d.id))

colour_by_pval = (d) -> blue_to_brown(d[pval_col])

ec_number = (id) ->
    if current_annot[id]!=undefined && current_annot[id]['EC_NUMBER']
        return current_annot[id]['EC_NUMBER']
    else
        ""

init_ec_legend = () ->
    ec_label = ["Oxidoreductases", "Transferases", "Hydrolases", "Lyases", "Isomerases", "Ligases"]
    for ec,i in (["EC#{i} #{ec_label[i-1]}",i] for i in [1..6]).concat([["Unknown",unknown_colour]])
        entry = $("<div></div>").data('code', ec[1])
        entry.append $("<span></span>").width(15).height(2)
                                       .css('display','inline-block')
                                       .css('margin','0 5px 2px 0')
                                       .css('background-color', colour_cat20(ec[1]))
        entry.append $("<span></span>").html(ec[0])
        $('.ec-filter').append(entry)
        entry.on('click', ((entry_local) ->
                              (e) -> $(entry_local).toggleClass('selected') ; update_data(current_data)
                           )(entry))

parcoords = null
dataView = null
grid = null
kegg = null
heatmap = null

id_col = null       # Column used for the ID
info_columns = []   # Columns used to display info about the genes

annot_to_html = (obj) ->
    res=""
    for k,v of obj
        res += "<div><span class='lbl'>#{k}: </span><span>#{v}</span></div>"
    res

kegg_mouseover = (obj) ->
    ec = obj.id
    d = []
    for row in current_data
        d.push(row) if ec_number(row[id_col]) == ec
    parcoords.highlight(d)
    #gridUpdateData(d)

init_chart = () ->
    init_ec_legend()
    parcoords = d3.parcoords()("#dge-pc")
        .alpha(0.4)
        .reorderable()
        .brushable() # enable brushing

    parcoords.setFilter(pcFilter)

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
        annot = current_annot[d[id_col]]
        if annot != undefined
            $('#gene-info').html(annot_to_html(annot))
            kegg.highlight(ec_number(d[id_col]))
    )
    grid.onMouseLeave.subscribe( (e,args) ->
        parcoords.unhighlight()
        $('#gene-info').html('')
        kegg.unhighlight()
    )
    grid.onDblClick.subscribe( (e,args) ->
          feature = grid.getDataItem(args.row)[id_col]
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
                        (show_expr && expr_col(key)) ||
                        key==pval_col || (key in info_columns)

update_grid = (data) ->
  column_keys = d3.keys(data[0])
  columns = column_keys.filter((key,i) -> grid_include(key)).map((key,i) ->
      id: key
      name: key
      field: key
      sortable: true
      formatter: (i,c,val,m,row) ->
          if fc_col(m.name) || ave_fc_col(m.name)
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
    if show_counts
        counts = []
        for k,v of current_counts[row[id_col]]
            counts.push(v) if k.indexOf(colName)==0
        countStr = "<span class='counts'>(#{counts})</span>"
    "<div class='#{colour}'>#{n.toFixed(2)}#{countStr}</div>"


expr_col   = (k) -> k.indexOf("ABS.")==0
fc_col     = (k) -> k.indexOf("FC ")==0
ave_fc_col = (k) -> k.indexOf("AFC ")==0

show_expr = false
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
        return true if item[col].toLowerCase().indexOf(searchStr)>=0
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
    return false if annot_genes_only && !current_annot[item.id]

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
    $('#plot-expr-cb').on("click", (e) ->
        update_data(current_data)
    )
    $('#plot-avgfc-cb').on("click", (e) ->
        update_data(current_data)
    )
    $('#show-counts-cb').on("click", (e) ->
        update_flags()
        grid.invalidate()
    )
    $('#annot-genes-cb').on("click", (e) ->
        update_data(current_data)
    )
    $('#pval-col-cb').on("click", (e) ->
        update_data(current_data)
    )

calc_kegg_colours = (data) ->
    ec_dirs = {}
    for row in current_data
        ec = ec_number(row[id_col])
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
        update_data(current_data)

    if !code
        set_filter([])
    else
        ec_colours = calc_kegg_colours(current_data)
        kegg.load(code, ec_colours, set_filter)
        $('div#kegg-image').dialog({width:500, height:600, title: title, position: {my: "right top", at:"right top", of: $('body')} })

update_flags = () ->
    show_expr = $('#plot-expr-cb').is(":checked")
    show_ave_fc = $('#plot-avgfc-cb').is(":checked")
    show_counts = $('#show-counts-cb').is(":checked")
    annot_genes_only = $('#annot-genes-cb').is(":checked")
    pval_colour = $('#pval-col-cb').is(":checked")

current_annot = {}
current_counts = {}

request_init_data = () ->
    d3.csv(script('query=counts'), (data) ->
        data.forEach (row) ->
          current_counts[row[id_col]] = row
        #console.log current_counts
    )

    d3.csv(script('query=annot'), (data) ->
        data.forEach (row) ->
            current_annot[row[id_col]] = row

        d3.tsv(script('query=kegg_titles'), (data) ->
            opts = "<option value=''>--- No pathway selected ---</option>"

            have_ec = {}
            for k,v of current_annot
                have_ec[v['EC_NUMBER']]=1

            data.forEach (row) ->
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

current_data = []
request_data = (columns) ->
    return if columns.length <= 1

    # load csv file and create the chart
    req = script("query=dge&fields=#{JSON.stringify columns}")
    start_loading()
    d3.csv(req, (data) ->
        done_loading()
        pri_col = ""
        for k,v of data[0]
            if expr_col(k)
                pri_col = k
                break
        ids = {}
        data.forEach (row, i) ->
            # slickgrid needs each data element to have an id
            if ids[row[id_col]]
                # TODO:  Duplicate ID raise a warning somewhere
                row.id = i
            else
                row.id = row[id_col] || i
            ids[row.id] = 1

            # Format numbers as numbers!
            for k,v of row
                row[k] = Number(v) if expr_col(k) || k==pval_col || k==ave_expr_col
            # Create FC columns
            for k,v of row
                if expr_col(k)
                    kStem = k.substring(4)
                    row["FC #{kStem}"]  = row[k] - row[pri_col]
                    row["AFC #{kStem}"] = row[k] - row[ave_expr_col]

        current_data = data
        update_data(current_data)
    )

    req = script("query=clustering&fields=#{JSON.stringify columns}")
    d3.csv(req, (data) ->
      heatmap.set_order(data.map((d) -> d.id))
      heatmap.redraw()
    )

update_data = (data) ->
    update_flags()
    dims = []
    for k,v of data[0]
        dims.push(k) if (!show_ave_fc && fc_col(k)) || (show_expr && expr_col(k)) || (show_ave_fc && ave_fc_col(k))

    parcoords.data(data)
             .color(if pval_colour then colour_by_pval else colour_by_ec)
             .dimensions(dims)
             .autoscale()

    # Calculate min/max for all dimensions - Want common scale across dimensions
    extents = []
    parcoords.dimensions().forEach (k) ->
        extents.push(d3.extent(data, (v) -> +v[k]))
    extent = d3.extent(d3.merge(extents))

    h = parcoords.height() - parcoords.margin().top - parcoords.margin().bottom;
    parcoords.dimensions().forEach (k) ->
        parcoords.yscale[k] = d3.scale.linear().domain(extent).range([h+1,1])

    parcoords.createAxes().render().reorderable().brushable()
    window.foo = parcoords

    update_grid(data)

    heatmap.update_columns(dims, extent, pval_col)
    #heatmap.schedule_update(data)

    parcoords.render()
    parcoords.brush()   # Reset any brushes that were in place

init = () ->
    if !settings['id_column']
        window.location = script("query=config")
    id_col = settings['id_column']
    info_columns = settings['info_columns'] || []

    $('a.config').attr('href', script("query=config"))

    fdrThreshold = settings['fdrThreshold'] if settings['fdrThreshold'] != undefined
    fcThreshold  = settings['fcThreshold']  if settings['fcThreshold'] != undefined

    order = settings['order'] || Object.keys(settings.replicates)
    $.each(order, (i, name) ->
      $("#files").append(
        "<div class='#{name}'>"+
        "  <a class='file' href='#' title='Select this condition' data-placement='right'>#{name}</a>"+
        "  <a class='pri' href='#' title='Make primary condition' data-placement='right'>pri</a>" +
        "</div>")
    )
    $("#files a.file").click(select_sample)
    $("#files a.pri").click(select_primary)
    $("select#kegg").change(kegg_selected)
    init_chart()
    init_search()
    init_slider()
    init_download_link()
    request_init_data()

    # Select some samples
    init_select = settings['init_select'] || []
    $.each(init_select, (i,name) ->
        cl = name.replace(/\./g, '\\.')
        $("div.#{cl}").addClass('selected')
    )
    $('#files div.selected:first').addClass('primary')
    update_samples()


$(document).ready(() -> init() )
$(document).ready(() -> $('[title]').tooltip())
