
# Each row (gene) has unique "id"
# Each row has "fields".    eg. info columns, FDR, EC
# Each row has "conditions"
# Each condition has "fields".  eg. AFC, FC, [counts]
# A "column" definition.
#   column_idx: integer/string    -- integer if data is array of arrays, string is array of object
#   name: string
#   parent: string  -- Allow columns to have a "parent" column.  Use for counts to real column
#   type: string -- Known types: FDR, Abs (absolute expression), FC (fold-change relative to pri), AFC (fold-change relative to average), Avg (Average expression)
#   is_pri: bool -- This is the "primary" condition used for comparison.
#

class GeneData
    constructor: (@data,@columns) ->
        @columns_by_type_cache = {}
        @_data_to_obj()
        @_calc_fc()


    # Ensure the data array is an array of objects
    _data_to_obj: () ->
        return if !(@data instanceof Array)
        for d,i in @data
            obj = {}
            for v,j in d
                obj[j] = v if v?
            @data[i] = obj
        null

    # Given Abs and pri, calculate FC relative to pri for each condition
    # Given Abs and Avg, calculate AFC relative to average
    _calc_fc: () ->
        abs_cols = @columns_by_type("abs")

        pri = null
        for col in @columns
            if col.is_pri
                pri = col
                break

        # Calculate FC columns relative to primary
        if pri && abs_cols.length > 0
            new_cols = {}
            for c in abs_cols
                n = {column_idx: "fc - #{c.column_idx}", name: c.name, type: 'fc'}
                new_cols[c.column_idx] = n
                @columns.push n
            for d in @data
                for c in abs_cols
                    d[new_cols[c.column_idx]] = d[c.column_idx] - d[pri]

        avg = @column_by_type('avg')
        # Calculate AFC columns relative to average
        if avg && abs_cols.length > 0
            new_cols = {}
            for c in abs_cols
                n = {column_idx: "afc - #{c.column_idx}", name: c.name, type: 'afc'}
                new_cols[c.column_idx] = n
                @columns.push n
            for d in @data
                for c in abs_cols
                    d[new_cols[c.column_idx]] = d[c.column_idx] - d[avg]

    # Returns a columns index (or null)
    column_by_type: (type) ->
        for col in @columns
            if col.type == type
                return col.column_idx
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

    #get_columns: () -> @columns

    get_data: () -> @data


window.GeneData = GeneData
