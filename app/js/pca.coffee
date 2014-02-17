require('./lib/numeric-1.2.6.js')

class ScatterPlot
    constructor: (elem, tot_width=800, tot_height=500) ->
        margin = {top: 20, right: 380, bottom: 80, left: 40}
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

        @yAxis = d3.svg.axis()
                   .scale(@y)
                   .orient("left");

        @svg = d3.select(elem).append("svg")
                 .attr("width", @width + margin.left + margin.right)
                 .attr("height", @height + margin.top + margin.bottom)
                .append("g")
                 .attr("transform", "translate(" + margin.left + "," + margin.top + ")")

    # draw(data,labels)
    #   data - array of rows.  First row is all x-coordinates
    #                          Second row is all y-coordinates
    #   labels - array of rows.  Each rows has 2 elements, first element is sample name
    #                             second element is key for coloring (for replicate)
    draw: (data, labels) ->
        @x.domain(d3.extent(data[0]))
        @y.domain(d3.extent(data[1]))

        # Easier to plot with array of
        locs = d3.transpose(data)

        @svg.selectAll(".axis").remove()
        @svg.append("g")
            .attr("class", "x axis")
            .attr("transform", "translate(0," + @height + ")")
            .call(@xAxis)
          .append("text")
            .attr("class", "label")
            .attr("x", @width)
            .attr("y", 10)
            .style("text-anchor", "start")
            .text("PCA dim 1");

        @svg.append("g")
            .attr("class", "y axis")
            .call(@yAxis)
          .append("text")
            .attr("class", "label")
            .attr("transform", "rotate(-90)")
            .attr("y", 6)
            .attr("dy", ".71em")
            .style("text-anchor", "end")
            .text("PCA dim 2");

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
             .style("fill", (d,i) => @color(labels[i][1]))
        dot_g.append("text")
             .attr('class',"labels")
             .text((d,i) -> labels[i][0])
             .attr('x',3)
             .attr('y',-3)
             .style("fill", (d,i) => @color(labels[i][1]))

        # Position the dots
        dots.transition()
            .attr("transform", (d) => "translate(#{@x(d[0])},#{@y(d[1])})")

class PCA
    @pca: (matrix) ->
        # Subtract mean (need zero-mean for PCA).  Global mean or column wise?
        #m = X.length
        #X = numeric.sub(X, numeric.sum(X)/(m*m))

        X = matrix.map((r) -> mean = 1.0*numeric.sum(r)/r.length; numeric.sub(r,mean))
        X = numeric.transpose(X)
        console.log(matrix,X)


        #sigma = numeric.div(numeric.dot(numeric.transpose(X), X), X.length)
        sigma = numeric.dot(numeric.transpose(X), X)
        svd = numeric.svd(sigma)
        console.log("svd",svd);
        #return numeric.dot(svd.U, numeric.diag(svd.S))
        return svd.U

    @most_variable: (X,num) ->
        num = Math.min(X.length-1, num)
        variances = X.map((row,i) ->
            sz = row.length;
            v = numeric.sum(numeric.pow(row,2))/sz - Math.pow(numeric.sum(row),2)/(sz*sz)
            return [v,i]
        )

        variances.sort((a,b) -> b[0] - a[0])
        #console.log("variances",variances)
        res = []
        for i in [0...num]
            res.push( X[variances[i][1]] )
        #console.log("res",res)
        return res;

    @to_dist: (a,b) ->
        diffs = numeric.sub(a,b)
        return Math.sqrt(numeric.sum(numeric.pow(diffs,2))/a.length)

class LogSlider
    constructor: (@elem, @txt_elem = null) ->
        @events = d3.dispatch("change")

        d3.select(@elem).on('change', () =>
            v = d3.select(@elem).property('value')
            d3.select(@txt_elem).property('value', Math.round(@slider_to_val(v))) if @txt_elem?
            @events.change()
        )

        if @txt_elem?
            d3.selectAll(@txt_elem).on('change', () =>
                v = +d3.select(@txt_elem)[0][0].value;
                @update_slider(v)
                @events.change()
            )

    on: (typ,cb) -> @events.on(typ, cb)

    val: () ->
        if @txt_elem?
            return +d3.select(@txt_elem)[0][0].value
        else
            return +d3.select(@elem).property('value')

    update_slider: (v) ->
        d3.select(@elem).property('value', @val_to_slider(v))

    set_max: (max) ->
        d3.select(@elem).property('max',Math.ceil(@val_to_slider(max)))
        @update_slider(@val())  # Redraw

    val_to_slider: (v) ->
        Math.log(v)/Math.log(1.3)

    slider_to_val: (v) ->
        Math.round(Math.pow(1.3,v))

# Implement Paul Harrison's glog moderation
log_moderation = 5.0
glog = (x,m) ->
    Math.log( (x + Math.sqrt(x*x + 4*m*m))/2 ) / Math.log(2)

class GenePCA
    constructor: (file, replicate_regexp) ->
        @top_n = new LogSlider('#top-n-slider','#top-n')
        @top_n.on('change', () => @redraw())
        @scatter = new ScatterPlot('#pca')

        # Hide labels if selected
        d3.select('#labels').on('change', () ->
            d3.selectAll('.labels').attr('display',
                if d3.select('#labels')[0][0].checked then null else 'none')
        )

        d3.csv(file, (data) =>
            # 'data' - one row per gene. First row is sample names.  First column is gene name
            max = data.length
            @top_n.set_max(max)
            d3.select('#max').text(""+max)

            @sample_names = d3.keys(data[0])[1..].map((name) ->
                m = name.match(replicate_regexp)
                key = if m then m[1] else null
                return [name, key])
            # Counts is an array for each gene.  Each of those is an array of counts
            counts = data.map((e) -> v = d3.values(e); v[1..].map((a) -> +a))

            lib_size = d3.transpose(counts).map((l) -> d3.sum(l))
            avg_lib_size = d3.mean(lib_size)
            console.log "lib_size",lib_size,avg_lib_size
            #console.log("names",window.names);
            #console.log("m - pre log",m);

            # Log transform counts
            @data = counts.map((row) -> d3.zip(row, lib_size).map(([val,size]) ->
                Math.log(10 + val)/Math.log(2)
                #glog(1000000.0 * val/size, 1000000.0 * log_moderation / avg_lib_size)
                ))

            @redraw()
        )

    redraw: () ->
        num_genes = @top_n.val()

        top_genes = PCA.most_variable(@data, num_genes)

        # Transpose to row per sample.
        top_genes = numeric.transpose(top_genes)

        #console.log("num",num_genes,"m",m);
        # Compute pair-wise distances
        dist = []
        for i in [0...top_genes.length]
            dist[i] ?= []
            for j in [i...top_genes.length]
                dist[j] ?= []
                if i==j
                    dist[i][j] = 0
                else
                    dist[i][j] = dist[j][i] = PCA.to_dist(top_genes[i], top_genes[j])

        console.log dist
        comp = PCA.pca(dist)
        #console.log("dist",dist,"comp",comp);
        @scatter.draw(numeric.transpose(comp), @sample_names)

#new GenePCA('x.csv', /^(.*)-rep\d/)
new GenePCA('x3.csv', /^(.*)_rp\d/)
