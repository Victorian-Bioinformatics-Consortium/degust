class ParCoords
    constructor: (@opts) ->
        @opts.h ?= 50
        @opts.width ?= 1000
        @opts.label_width ?= 120
        @opts.limit ?= @opts.width - @opts.label_width

        @parcoords = d3.parcoords()(@opts.elem)
            .alpha(0.4)
            .reorderable()
            .brushable() # enable brushing

        @parcoords.setFilter(@opts.pcFilter)   #FIXME - better name "pcFilter"

    update_data: (data, dims, extent, coloring) ->
        @parcoords.data(data)
             .dimensions(dims)
             .autoscale()
        @parcoords.color(coloring) if coloring

        extent ?= ParCoords.calc_extent(data,dims)

        h = @parcoords.height() - @parcoords.margin().top - @parcoords.margin().bottom;
        @parcoords.dimensions().forEach (k) =>
            @parcoords.yscale[k] = d3.scale.linear().domain(extent).range([h+1,1])

        @parcoords.createAxes().render().reorderable().brushable()

        @parcoords.render()
        @parcoords.brush()   # Reset any brushes that were in place

    # Static method 'calc_extent'
    @calc_extent: (data, dims) ->
        # Calculate min/max for all dimensions - Want common scale across dimensions
        extents = []
        dims.forEach (k) ->
            extents.push(d3.extent(data, (v) -> +v[k]))
        return d3.extent(d3.merge(extents))

    # These methods just pass through
    highlight: (d) -> @parcoords.highlight(d)
    unhighlight: () -> @parcoords.unhighlight()
    on: (t,func) -> @parcoords.on(t,func)        # FIXME - better abstraction for this
    brush: () -> @parcoords.brush()


window.ParCoords = ParCoords