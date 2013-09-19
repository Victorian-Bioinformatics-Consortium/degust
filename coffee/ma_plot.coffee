class MAPlot
    constructor: (@opts) ->
        @opts.height ?= 300
        @opts.width ?= 600
        @opts.padding ?= 20

        @svg = d3.select(@opts.elem).append('svg')
        @svg.attr("width", @opts.width).attr("height", @opts.height)

        # Create a custom 'brush' event.  This will allow same API as par-coords
        @dispatch = d3.dispatch("brush")


    update_data: (@data, fc_dim, ave_dim, coloring) ->
        if fc_dim.length!=1 || ave_dim.length!=1
            msg_info("Only support 2 dimensions for ma-plot",fc_dim,ave_dim)
            return

        #m_dim = (d) -> d[dims[0].idx]-d[dims[1].idx]
        #a_dim = (d) -> d[dims[0].idx]+d[dims[1].idx]
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
        @svg.call(@mybrush)

        dots = @svg.selectAll("circle")
                   .data(data)
        dots.enter().append("circle")
        dots.exit().remove()
        dots.attr("r", 3)
            .attr("cx", (d) -> xScale(a_dim(d)))
            .attr("cy", (d) -> yScale(m_dim(d)))
            .on('mouseover', (d) -> console.log(d,m_dim(d),a_dim(d)))
        dots.style('fill', coloring) if coloring
        @_hide_dots(dots)

        @svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(0," + yScale(0) + ")")
            .call(xAxis)

        @svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(" + @opts.padding + ",0)")
            .call(yAxis)

    _hide_dots: (dots) ->
        dots.style('opacity', (d) => if @opts.filter(d) then 0.7 else 0.05)

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

    # These methods just pass through
    highlight: (rows) ->
        hi = @svg.selectAll(".highlight")
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
        @_hide_dots(@svg.selectAll("circle"))
        @_brushed()


window.MAPlot = MAPlot