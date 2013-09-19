class MAPlot
    constructor: (@opts) ->
        @opts.height ?= 150
        @opts.width ?= 400
        @opts.padding ?= 5


        @svg = d3.select(@opts.elem).append('svg')
        @svg.attr("width", @opts.width).attr("height", @opts.height)

    update_data: (data, dims, coloring) ->
        if dims.length!=2
            msg_info("Only support 2 dimensions for ma-plot")
            return

        console.log(dims)

        m_dim = (d) -> d[dims[0].idx]-d[dims[1].idx]
        a_dim = (d) -> d[dims[0].idx]+d[dims[1].idx]

        xScale = d3.scale.linear()
                     .domain(d3.extent(data, (d) -> a_dim(d)))
                     .range([@opts.padding, @opts.width-@opts.padding]);
        yScale = d3.scale.linear()
                     .domain(d3.extent(data, (d) -> m_dim(d)))
                     .range([@opts.height-@opts.padding, @opts.padding]);

        xAxis = d3.svg.axis()
                  .scale(xScale)
                  .orient("bottom")
        yAxis = d3.svg.axis()
                  .scale(yScale)
                  .orient("left")
                  .ticks(5)

        @svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(0," + (@opts.height - @opts.padding) + ")")
            .call(xAxis)

        @svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(" + @opts.padding + ",0)")
            .call(yAxis)

        dots = @svg.selectAll("circle")
                   .data(data)
        dots.enter().append("circle")
        dots.exit().remove()
        dots.attr("r", 2)
            .attr("cx", (d) -> xScale(a_dim(d)))
            .attr("cy", (d) -> yScale(m_dim(d)))
            .on('mouseover', (d) -> console.log(d,m_dim(d),a_dim(d)))
        dots.style('fill', coloring) if coloring

    # These methods just pass through
    highlight: (d) ->
    unhighlight: () ->
    on: (t,func) ->
    brush: () ->


window.MAPlot = MAPlot