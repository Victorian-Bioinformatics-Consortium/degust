class Heatmap
    constructor: (@opts) ->
        @opts.h ?= 50
        @opts.width ?= 1000
        @opts.label_width ?= 120
        @opts.limit ?= @opts.width - @opts.label_width
        @opts.redraw_delay ?= 1000

        @svg = d3.select(@opts.elem).append('svg')
        @svg.append('g').attr("class", "labels")
        @svg.append('g').attr("class", "genes").attr("transform", "translate(#{opts.label_width},0)")
        @svg.attr("width", @opts.width).attr("height", @opts.h * 2)


        @redraw_scheduled = false

    set_order: (@order) ->
        # Nothing

    schedule_update: (data) ->
        @data=data if data
        scheduler.schedule('heatmap', () => @update_data(@data))
        @svg.attr('opacity',0.4)

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

    update_data: (@data) ->
        @redraw_scheduled = false
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

        @svg.attr("width", @opts.width).attr("height", @opts.h * @columns.length)
        w = d3.min([@opts.h, (@opts.width - @opts.label_width) / num_kept])

        #console.log("max",@max,"kept",kept_data,"num", num_kept, w)


        genes = @svg.select("#heatmap .genes").selectAll("g.gene")
                    .data(d3.values(kept_data)) #, (d) -> d.id)

        genes.enter().append("g").attr("class","gene")
        genes.exit().remove()

        cells = genes.selectAll(".cell")
                     .data(((d) =>
                         res=[]
                         for i,c of @columns
                             res.push {row:row_ids[d.id], col:i, score: d[c.idx] }
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



window.Heatmap = Heatmap
