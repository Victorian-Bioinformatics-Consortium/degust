class Heatmap
    constructor: (@opts) ->
        @opts.h_pad ?= 20
        @opts.h ?= 50
        @opts.width ?= 1000
        @opts.label_width ?= 120
        @opts.limit ?= @opts.width - @opts.label_width

        @svg = d3.select(@opts.elem).append('svg')
        @svg.append('g').attr("class", "labels")
        @svg.append('g').attr("class", "genes").attr("transform", "translate(#{opts.label_width},0)")
        @svg.attr("width", @opts.width).attr("height", @opts.h * 2 + 10*@opts.h_pad)

        @info = @svg.append('text')
                    .attr("class", "info")
                    .attr("x", @opts.width-200)
                    .attr("y", 10)

    set_order: (@order) ->
        # Nothing

    _show_calc_info: (str) ->
        @info.text(str)

    # Calculate a rough ordering.  Proper clustering would be better!
    # The clustering is calculated by "calc_routine" which does part
    # of the computation then yields and reschedules itself until done.
    # This allows the large (slow) clusterings to be done without impacting
    # the browser too much
    _calc_order: () ->
        if @data.length==0
            @order = []
            return

        t1 = new Date()
        used = {0: true}
        order=[0]
        calc_routine = () =>
            yield_after = 50     # Should be a function of @data.length
            while order.length < @data.length && (yield_after--)>0
                row = @data[order[order.length-1]]
                best_i = best_d = null
                for i in [0..@data.length-1]
                    continue if used[i]
                    r = @data[i]
                    d = @_dist(row,r)
                    if !best_d or d<best_d
                        best_d = d
                        best_i = i
                order.push(best_i)
                used[best_i] = true
            if order.length >= @data.length
                # Done.  Render the heatmap
                @order = order.map((i) => @data[i].id)
                msg_debug("calc_order: took=#{new Date()-t1}ms for #{@data.length} points",order,@order)
                @_render_heatmap()
                @_show_calc_info("")
            else
                # Still more work.  Re-schedule ourselves
                scheduler.schedule('heatmap.calc', calc_routine, 50)
                @_show_calc_info("Clustered #{order.length} of #{@data.length}")

        # If there are many data points, start the calc after a few seconds to give an other
        # rendering a chance (ie. parcoords).  A better solution would be to have priorities
        # but that needs parcoords to be changed to use the same scheduling interface
        if @data.length>3000
            scheduler.schedule('heatmap.calc', calc_routine, 10*1000)
        else
            scheduler.schedule('heatmap.calc', calc_routine, 10)

    # Distance calc for 2 genes. (for clustering)
    _dist: (r1,r2) ->
        s = 0
        for c in @columns
            v = r1[c.idx]-r2[c.idx]
            s += v*v
            #s = d3.max([s,v])
        s

    schedule_update: (data) ->
        @data=data if data

        @svg.attr('opacity',0.4)
        @_calc_order()

    # update_columns(columns,extent,sel_column)
    #   columns - The DGE condition columns
    #   extent - the total range of all columns
    #   sel_column - column to order rendering by
    update_columns: (@columns, extent, @sel_column) ->
        @max = d3.max(extent.map(Math.abs))
        @colorScale = d3.scale.linear()
                        .domain([-@max, 0, @max])
                        .range(["red", "white", "blue"]);

        cols = @svg.select('#heatmap .labels').selectAll('.label')
                   .data(@columns)
        cols.enter().append('text').attr("class","label")
        cols.exit().remove()
        cols.attr('x', @opts.label_width)
            .attr('y', (d,i) => i * @opts.h + @opts.h/2)
            .attr("text-anchor", "end")
            .text((d) -> d.name)

    _render_heatmap: () ->
        @svg.attr('opacity',1)
        kept_data = {}
        sorted = @data[0..]
        sorted.sort((a,b) => a[@sel_column] - b[@sel_column])
        kept_data[d.id]=d for d in sorted[0..@opts.limit-1]

        row_ids={}
        num_kept=0
        for id in (@order || data.map((d) -> d.id))
            if kept_data[id]
                row_ids[id]=num_kept
                num_kept += 1

        @svg.attr("width", @opts.width).attr("height", @opts.h_pad + @opts.h * @columns.length)
        w = d3.min([@opts.h, (@opts.width - @opts.label_width) / num_kept])

        #console.log("max",@max,"kept",kept_data,"num", num_kept, w)

        # @_create_brush(w)

        genes = @svg.select("#heatmap .genes").selectAll("g.gene")
                    .data(d3.values(kept_data)) #, (d) -> d.id)

        genes.enter().append("g").attr("class","gene")
        genes.exit().remove()

        cells = genes.selectAll(".cell")
                     .data(((d) =>
                         if !row_ids[d.id]?
                             msg_info("missing row_ids[#{d.id}]")
                             return
                         res=[]
                         for i,c of @columns
                             res.push {row:row_ids[d.id], col:i, score: d[c.idx], id: d.id }
                         res),
                         (d) -> d.col)
        cells.enter().append("rect").attr('class','cell')
        cells.attr("x", (d) => d.row * w)
             .attr("y", (d) => d.col * @opts.h)
             .attr("width",  w)
             .attr("height", @opts.h)
             .style("fill", (d) => @colorScale(d.score))
        cells.exit().remove()

        genes.on('mouseover', @opts.mouseover) if @opts.mouseover
        genes.on('mouseout', @opts.mouseout) if @opts.mouseout

        #genes.on('mousedown', (e,l) -> console.log 'down', e,l)
        #genes.on('mouseup', (e,l) -> console.log 'up', e,l)
        #genes.on('mousemove', (e,l) -> console.log 'move', e,l)


    # NOT IN USE - not sure how to get it right.  How should it interact with parallel-coords or ma-plot?
    _create_brush: (w) ->
        # Create brush
        x = d3.scale.identity().domain([0, (@opts.width - @opts.label_width)])
        brush = d3.svg.brush()
             .x(x)
             #.extent([0,200])
             #.on("brush", () -> console.log 'brushed', brush.extent())
             .on("brushend", () =>
                ex = brush.extent()
                data = @svg.selectAll("rect.cell").data()
                d = data.filter((d) -> d.col=='0' && ex[0]<=d.row*w && ex[1]>=d.row*w)
                #console.log "in range=",d
                )

        @svg.select("g.brush").remove()
        gBrush = @svg.append("g")
             .attr("class", "brush")
             .attr("transform", "translate(#{@opts.label_width},0)")
             .call(brush)
            # .call(brush.event)
        gBrush.selectAll("rect")
              .attr("height", 100*@opts.h_pad + @opts.h * @columns.length);





window.Heatmap = Heatmap
