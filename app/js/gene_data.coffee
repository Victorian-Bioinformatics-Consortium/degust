
# Each row (gene) has unique "id"  - given one of doesn't exist
# Each row has "fields".    eg. info columns, FDR, EC
# Each row has "conditions"
# Each condition has "fields".  eg. AFC, FC, [counts]
# A "column" definition.
#   idx: string    -- string is array of object
#   name: string
#   parent: string  -- Allow columns to have a "parent" column.  Use for counts to real column
#   type: string -- Known types: FDR, Abs (absolute expression), FC (fold-change relative to pri), Avg (Average expression), primary (fake column to include primary condition FC are relative to)
#

class GeneData
    constructor: (@data,@columns) ->
        @columns_by_type_cache = {}
        @_process_data()
        @set_relative( 'avg' )
        #msg_debug 'data',@data


    set_relative: (relative) ->
        if relative != @relative
            @relative = relative
            @_calc_fc()

    # Ensure numbers are numbers!
    # Add an "id" to each row.
    # Put smallest FDR first, want these *on top* of PC charts
    _process_data: () ->
        for d,i in @data
            d.id = i if !d.id?
            for c in @columns
                if c.type in ['fc','abs','avg','fdr','count']
                    d[c.idx] = +d[c.idx]
        fdr_col = @column_by_type('fdr')
        @data.sort((a,b) -> a[fdr_col] - b[fdr_col])
        null

    _calc_fc: () ->
        @columns_by_type_cache = {}
        primary = @columns_by_type('primary')[0]
        @relative = primary if !@relative?

        fc_cols = @columns_by_type('fc')
        new_cols = []
        for col,i in @columns
            do (col) =>
                if col.type not in ['fc_calc']
                    new_cols.push(col)
                if col.type in ['fc','primary']
                    new_cols.push(
                        idx: "_calc_#{i}"
                        name: col.name
                        type: 'fc_calc'
                        calc: (d) =>
                            v1 = if col==primary then 0 else d[col.idx]
                            v2 = if @relative==primary
                                     0
                                 else if @relative=='avg'
                                     d3.sum(fc_cols.map((c) -> d[c.idx]))/(1+fc_cols.length)
                                 else
                                     d[@relative.idx]
                            v1-v2
                    )
        @columns = new_cols
        for d in @data
            for col in @columns
                if col.type in ['fc_calc']
                    d[col.idx] = col.calc(d)


    # Returns a columns index (or null)
    column_by_type: (type) ->
        for col in @columns
            if col.type == type
                return col.idx
        return null

    # Lookup a column by index
    column_by_idx: (idx) ->
        for col in @columns
            if col.idx == idx
                return col
        return null

    # Returns a list of column definitions
    columns_by_type: (types) ->
        return @columns_by_type_cache[types] if @columns_by_type_cache[types]
        types=[types] if !(types instanceof Array)
        res = []
        for col in @columns
            res.push(col) if col.type in types
        @columns_by_type_cache[types] = res
        res

    # Lookup columns by the parent.  Used for find replicate 'count' columns
    assoc_column_by_type: (type, parent_name) ->
        res = []
        for col in @columns
            res.push(col) if col.type==type && col.parent==parent_name
        res

    #get_columns: () -> @columns

    get_data: () -> @data


window.GeneData = GeneData
