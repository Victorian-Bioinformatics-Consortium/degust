
class ScatterPlot
    constructor: (elem, tot_width=570, tot_height=400) ->
        margin = {top: 20, right: 180, bottom: 40, left: 60}
        @width = tot_width - margin.left - margin.right
        @height = tot_height - margin.top - margin.bottom

        @x = d3.scale.linear()
               .range([0, @width])

        @y = d3.scale.linear()
               .range([@height, 0])

        @color = d3.scale.category10()

        @xAxis = d3.svg.axis()
                   .scale(@x)
                   .orient("bottom")
                   .tickSize(8,1)

        @yAxis = d3.svg.axis()
                   .scale(@y)
                   .orient("left")
                   .tickSize(8,1)

        @svg = d3.select(elem).append("svg")
                 .attr('class','scatter')
                 .attr("width", @width + margin.left + margin.right)
                 .attr("height", @height + margin.top + margin.bottom)
                .append("g")
                 .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

    # draw(data,labels)
    #   data - array of rows.  First row is all x-coordinates (dimension 1)
    #                          Second row is all y-coordinates (dimension 2)
    #   labels - array of samples.  sample.name, and (sample.parent for colouring)
    draw: (data, labels, dims) ->
        [dim1,dim2] = dims
        @x.domain(d3.extent(data[dim1-1]))
        @y.domain(d3.extent(data[dim2-1]))

        # Easier to plot with array of
        locs = d3.transpose(data)

        @svg.selectAll(".axis").remove()
        @svg.append("g")
            .attr("class", "x axis")
            .attr("transform", "translate(0," + @height + ")")
            .call(@xAxis)
          .append("text")
            .attr("class", "label")
            .attr("x", @width/2)
            .attr("y", 40)
            .style("text-anchor", "middle")
            .text("MDS Dimension #{dim1}");

        @svg.append("g")
            .attr("class", "y axis")
            .call(@yAxis)
          .append("text")
            .attr("class", "label")
            .attr("transform", "rotate(-90)")
            .attr("x", -100)
            .attr("y", -50)
            .style("text-anchor", "end")
            .text("MDS Dimension #{dim2}");

        dots = @svg.selectAll(".dot")
                   .data(locs)
        dots.exit().remove()

        # Create the dots and labels
        dot_g = dots.enter().append("g")
                    .attr("class", "dot")
        dot_g.append("circle")
             .attr("r", 3.5)
             .attr("cx",0)
             .attr("cy",0)
        dot_g.append("text")
             .attr('class',"labels")
             .attr('x',3)
             .attr('y',-3)

        # Ensure the correct colour and text
        dots.select("circle")
              .style("fill", (d,i) => @color(labels[i].parent))
        dots.select("text")
              .style("fill", (d,i) => @color(labels[i].parent))
              .text((d,i) -> labels[i].name)

        # And animate the moving dots
        dots.transition()
            .attr("transform", (d) => "translate(#{@x(d[dim1-1])},#{@y(d[dim2-1])})")

class BarGraph
    constructor: (@opts) ->
        @opts.tot_width  ||= 200
        @opts.tot_height ||= 200
        margin = {top: 20, right: 10, bottom: 30, left: 40}
        @width = @opts.tot_width - margin.left - margin.right
        @height = @opts.tot_height - margin.top - margin.bottom

        @x = d3.scale.ordinal()
               .rangeRoundBands([0, @width], .1)

        @y = d3.scale.linear()
               .range([@height, 0])

        @xAxis = d3.svg.axis()
                   .scale(@x)
                   .orient("bottom")
                   .tickSize(8,1)

        @yAxis = d3.svg.axis()
                   .scale(@y)
                   .orient("left")
                   .tickSize(8,1)

        @svg = d3.select(@opts.elem).append("svg")
                 .attr('class','bar-chart')
                 .attr("width", @width + margin.left + margin.right)
                 .attr("height", @height + margin.top + margin.bottom)
                .append("g")
                 .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
    draw: (data) ->
        @svg.selectAll("*").remove()
        @x.domain(data.map((d) -> d.lbl ))
        @y.domain([0, d3.max(data, (d) -> d.val)])

        @svg.append("text")
             .attr('class', 'title')
             .attr("x", @width/2)
             .attr("y", -10)
             .style("text-anchor", "middle")
             .text("Magnitude of each MDS dimension")

        @svg.append("g")
             .attr("class", "x axis")
             .attr("transform", "translate(0," + @height + ")")
             .call(@xAxis)
            .append("text")
             .attr('class', 'label')
             .attr("x", @width/2)
             .attr("y", 30)
             .style("text-anchor", "middle")
             .text("Dimension")

        @svg.append("g")
             .attr("class", "y axis")
             .call(@yAxis)
           .append("text")
             .attr('class', 'label')
             .attr("transform", "rotate(-90)")
             .attr("x", -60)
             .attr("y", -30)
             .style("text-anchor", "end")
             .text("Magnitude")

        @svg.selectAll(".bar")
            .data(data)
            .enter().append("rect")
              .attr("class", "bar")
              .attr("x", (d) => @x(d.lbl))
              .attr("width", @x.rangeBand())
              .attr("y", (d) => @y(d.val))
              .attr("height", (d) => @height - @y(d.val))
              .on('click', (d) => if @opts.click? then @opts.click(d))

class PCA
    @pca: (matrix) ->
        # We expect 1 row per sample.  Each column is a different gene
        # Subtract column-wise mean (need zero-mean for PCA).
        X = numeric.transpose(numeric.transpose(matrix).map((r) -> mean = 1.0*numeric.sum(r)/r.length; numeric.sub(r,mean)))

        sigma = numeric.dot(X,numeric.transpose(X))
        svd = numeric.svd(sigma)

        # scale the coordinates back
        # (from http://www.ats.ucla.edu/stat/r/pages/svd_demos.htm)
        r = numeric.dot(svd.V, numeric.sqrt(numeric.diag(svd.S)))

        # Want RMS distance (like in Limma), so divide by sqrt(n)
        r = numeric.div(r, Math.sqrt(matrix[0].length))
        r

    @variance: (X) ->
        sz = X.length
        numeric.sum(numeric.pow(X,2))/sz - Math.pow(numeric.sum(X),2)/(sz*sz)

log_moderation = 10.0

variance_key = "_variance"
transform_key = "_transformed_"

class GenePCA
    constructor: (@opts) ->
        @scatter = new ScatterPlot(@opts.elem)
        @barGraph = new BarGraph(
                           elem: @opts.elem
                           click: (d) => if @opts.sel_dimension?
                                             @opts.sel_dimension(d.lbl)
                        )

    # Note, this is naughty - it writes to the 'data' array a "_variance" column
    # and several "_transformed_" columns.
    # Also, compute the per-column (library) size for later normalization
    update_data: (@data, @columns) ->
        # Compute the library size for each column (for normalising in _compute_variance)
        @lib_size = {}
        @columns.forEach((c) => @lib_size[c.idx] = 0)
        @data.forEach((row) =>
            @columns.forEach((c) =>
                @lib_size[c.idx] += row[c.idx]
            ))


        @redraw()

    # Convert to log2 counts, and normalize for library size, and compute the gene variance.
    _compute_variance: (row) ->
        transformed =  @columns.map((col) =>
            norm = @lib_size[col.idx]/1000000.0
            val = row[col.idx]/norm
            t_val = Math.log(log_moderation + val)/Math.log(2)
            row[transform_key+col.idx] = t_val
            t_val
        )
        row[variance_key] = PCA.variance(transformed)

    redraw: () ->
        {skip:skip_genes, num:num_genes, dims:dims} = @opts.params()
        dims = [1,2] if dims.length!=2

        # Log transform counts
        kept_data = @data.filter((d) => @opts.filter(d))
                       #  .map((row) -> d3.zip(row, lib_size).map(([val,size]) ->
        kept_data.forEach((row) => @_compute_variance(row))

        top_genes = kept_data.sort((a,b) -> b[variance_key] - a[variance_key])
        top_genes = top_genes[skip_genes ... (skip_genes + num_genes)]

        @opts.gene_table.set_data(top_genes) if @opts.gene_table

        # Get the transformed counts
        transformed = top_genes.map((row) => @columns.map((col) ->
                                                row[transform_key+col.idx]))
        # Transpose to row per sample.
        by_gene = numeric.transpose(transformed)


        comp = numeric.transpose(PCA.pca(by_gene))
        # 'comp' now contains components.  Each row is a dimension

        @scatter.draw(comp, @columns, dims)

        @barGraph.draw(comp[0..9].map((v,i) ->
            range = d3.max(v) - d3.min(v)
            {lbl: "#{i+1}", val: range}
        ))

    brush: () ->
        @redraw()

    highlight: () ->
        # Pass
    unhighlight: () ->
        # Pass

window.GenePCA = GenePCA
