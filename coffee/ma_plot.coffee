class MAPlot
    constructor: (@opts) ->
        @opts.height ?= 300
        @opts.width ?= 600
        @opts.padding ?= 20


        @gDot = d3.select(@opts.elem).append('canvas')
        @gDot.attr("width", @opts.width)
             .attr("height", @opts.height)

        @svg = d3.select(@opts.elem).append('svg')
        @svg.attr("width", @opts.width)
            .attr("height", @opts.height)

        @gBrush = @svg.append('g')

        @gHighlight = @svg.append('g')

        #@tooltip = d3.select(@opts.elem).append("div")
        #             .attr("class", "tooltip")
        #             .style("opacity", 0)

        # Create a custom 'brush' event.  This will allow same API as par-coords
        @dispatch = d3.dispatch("brush")

    update_data: (@data, fc_dim, ave_dim, @coloring, @info_cols) ->
        if fc_dim.length!=1 || ave_dim.length!=1
            msg_info("Only support 2 dimensions for ma-plot",fc_dim,ave_dim)
            return

        @svg.select("g.brush").remove()
        @svg.selectAll(".axis").remove()

        @m_dim = m_dim = (d) -> d[fc_dim[0].idx]
        @a_dim = a_dim = (d) -> d[ave_dim[0].idx]

        @xScale = xScale = d3.scale.linear()
                     .domain(d3.extent(data, (d) -> a_dim(d)).map((x) -> x*1.05))
                     .range([@opts.padding, @opts.width-@opts.padding]);
        @yScale = yScale = d3.scale.linear()
                     .domain(d3.extent(data, (d) -> m_dim(d)).map((x) -> x*1.05))
                     .range([@opts.height-@opts.padding, @opts.padding]);

        xAxis = d3.svg.axis()
                  .scale(xScale)
                  .orient("bottom")
        yAxis = d3.svg.axis()
                  .scale(yScale)
                  .orient("left")
                  .ticks(5)

        @mybrush = d3.svg.brush()
          .x(xScale)
          .y(yScale)
          .clamp([false,false])
          .on("brush",  () => @_brushed())
        @gBrush.call(@mybrush)

        @_draw_dots()

        @svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(0," + yScale(0) + ")")
            .call(xAxis)

        @svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(" + @opts.padding + ",0)")
            .call(yAxis)


    _draw_dots: () ->
        ctx = @gDot[0][0].getContext("2d")
        ctx.clearRect(0,0,@opts.width, @opts.height)
        i=@data.length
        while(i--)
            do (i) =>
                d = @data[i]
                if @opts.filter(d)
                    ctx.fillStyle = @coloring(d)
                    ctx.globalAlpha = 0.7
                    ctx.beginPath()
                    ctx.arc(@xScale(@a_dim(d)), @yScale(@m_dim(d)), 3, 0, Math.PI*2)
                    ctx.fill()
                    #ctx.strokeStyle="#000000"
                    #ctx.stroke()

    _show_info: (row) ->
        fmt = (val) -> val.toFixed(2)

        @tooltip.transition().duration(200)
                .style("opacity", 0.8)
        info="<table>"
        @info_cols.forEach((c) ->
            info += "<tr><td><b>#{c.name}</b>:<td>#{row[c.idx]}"
        )
        info += "<tr><td><b>A</b>:<td>#{fmt @a_dim(row)}"
        info += "<tr><td><b>M</b>:<td>#{fmt @m_dim(row)}"
        info += "</table>"
        loc = d3.mouse(@svg[0][0])
        @tooltip.html(info)
                .style("left", (loc[0] + 10) + "px")
                .style("top",  (loc[1] + 15) + "px")

    _hide_info: () ->
        @tooltip.transition().duration(500)
                .style("opacity", 0)

    _brushed: () ->
        sel = @_selected()
        #console.log 'brushed', sel
        @dispatch.brush(sel)

    _selected: () ->
        if @mybrush.empty()
            @data.filter((d) => @opts.filter(d))
        else
            ex = @mybrush.extent()
            @data.filter((d) =>
                                y = @m_dim(d)
                                x = @a_dim(d)
                                @opts.filter(d) && x>=ex[0][0] && x<=ex[1][0] && y>=ex[0][1] && y<=ex[1][1]
                        )

    highlight: (rows) ->
        hi = @gHighlight.selectAll(".highlight")
                 .data(rows, (d) -> d.id)
        hi.exit().remove()
        hi.enter().insert("circle")
            .attr("class", "highlight")
            .attr("opacity", 1)
            .style("fill-opacity", 0)
            .style("stroke", "black")
            .style("stroke-width", 3)

        hi.attr("r", 15)
          .attr("cx", (d) => @xScale(@a_dim(d)))
          .attr("cy", (d) => @yScale(@m_dim(d)))
          .transition().duration(500)
          .attr("r", 7);

    unhighlight: () ->
        @svg.selectAll(".highlight").remove()

    on: (t,func) ->
        @dispatch.on(t, func)

    brush: () ->
        @_draw_dots()
        @_brushed()


window.MAPlot = MAPlot