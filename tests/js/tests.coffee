
module('util')
test('util', () ->
     ok(msg_info, "testing msg_info")
)


module('gene_data')
test('empty gene_data', () ->
    gd = new GeneData([],[])
    equal(gd.get_data().length, 0, "zero data")
    equal(gd.column_by_type('fc'), null, "No column")
    equal(gd.columns_by_type('fc').length, 0, "No column")
)

asyncTest( "csv gene_data", () ->
    expect( 3 )
    cols = [{column_idx: 'id', name: "id"}
            {column_idx: 'adj.P.Val', name: 'FDR', type: 'fdr'}
            {column_idx: 'ABS.1', name: 'Cond 1', type: 'fc'}
            {column_idx: 'ABS.2', name: 'Cond 2', type: 'fc'}
            ]

    d3.csv("test1.csv", (d) ->
        gd = new GeneData(d, cols)
        equal(gd.get_data().length, 49)
        equal(gd.column_by_type('fdr'), 'adj.P.Val', "Find FDR column")
        equal(gd.columns_by_type('fc').length, 2, "Find FC columns")
        start()
    )
)
