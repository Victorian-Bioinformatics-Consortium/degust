
# Each row (gene) has unique "id"
# Each row has "fields".    eg. info columns, FDR, EC
# Each row has "conditions"
# Each condition has "fields".  eg. AFC, FC, [counts]
# A "column" definition.
#   column_idx: integer/string    -- integer if data is array of arrays, string is array of object
#   is_id: boolean  -- May be omitted
#   name: string
#   parent: string  -- Allow columns to have a "parent" column.  Use for counts to real column
#   type: string -- Arbitrary string for use by user

class DataContainer
    constructor: () ->
        @data = {}

    add_data: (name, data, columns) ->
        @data[name] = {data: data, columns: columns}
        @join_data()
        msg_debug("All",@joined_columns,@joined_data,@data)

    join_data: () ->
        msg_debug "Joining data..."
        @columns_by_type_cache = {}
        @joined_data = []
        @joined_columns = []
        @joined_id_lookup = {}
        for n, dat of @data
            cols = []
            i=@joined_columns.length
            id_column = null
            for col in dat.columns
                id_column=col.column_idx if col.is_id
                col.idx = i++
                cols.push(col)
            @joined_columns.push.apply(@joined_columns, cols)

            j=0
            for row in dat.data
                id = row[id_column]
                continue if id=='' || id==null

                idx = @joined_id_lookup[id]
                if idx==undefined
                    idx = @joined_data.push([])-1
                    @joined_id_lookup[id]=idx
                for col in cols
                    if col.is_id
                        @joined_data[idx].id ?= id
                    if col.func
                        v = col.func(row)
                    else
                        v = row[col.column_idx]
                    v = +v if col.numeric
                    @joined_data[idx].push(v)

        msg_debug("Data joined")


    column_by_type: (type) ->
        for col in @joined_columns
            if col.type == type
                return col.idx
        return null

    column_by_idx: (idx) ->
        for col in @joined_columns
            if col.idx == idx
                return col
        return null

    columns_by_type: (types) ->
        return @columns_by_type_cache[types] if @columns_by_type_cache[types]
        types=[types] if !(types instanceof Array)
        res = []
        for col in @joined_columns
            res.push(col) if col.type in types
        @columns_by_type_cache[types] = res
        res

    get_columns: () -> @joined_columns

    get_data: () -> @joined_data

window.DataContainer = DataContainer