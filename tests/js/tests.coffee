
dbls_equal = (arr1, arr2) ->
    return false if arr1.length != arr2.length
    for v,i in arr1
        return false if Math.abs( v-arr2[i] )>1e-3
    return true

module('util')
test('util', () ->
     ok(log_info, "testing log_info")
)


module('gene_data')
test('empty gene_data', () ->
    gd = new GeneData([],[])
    equal(gd.get_data().length, 0, "zero data")
    equal(gd.column_by_type('fc'), null, "No column")
    equal(gd.columns_by_type('fc').length, 0, "No column")
)

asyncTest( "csv gene_data", () ->
    expect( 4 )
    cols = [{idx: 'id', name: "id"}
            {idx: 'adj.P.Val', name: 'FDR', type: 'fdr'}
            {idx: 'ABS.1', name: 'Cond 1', type: 'abs'}
            {idx: 'ABS.2', name: 'Cond 2', type: 'abs'}
            {idx: 'AveExpr', name: 'Avg Expr', type: 'avg'}
            ]

    d3.csv("test1.csv", (d) ->
        gd = new GeneData(d, cols)
        row1 = gd.get_data()[0]

        equal(gd.get_data().length, 49, "expected data length")
        deepEqual(gd.column_by_idx('ABS.1'), cols[2], "Column idx lookup")
        equal(gd.column_by_type('fdr'), 'adj.P.Val', "Find FDR column")
        equal(gd.columns_by_type('abs').length, 2, "Find ABS columns")

        # TODO : fix these tests (test for changing 'relative' column)
        #equal(gd.columns_by_type('fc').length, 2, "Computed 2 FC columns")
        #equal(gd.columns_by_type('afc').length, 2, "Computed 2 AFC columns")
        #equal(gd.assoc_column_by_type('count','Cond 1').length, 0, "No parent columns")

        #fc_cols = gd.columns_by_type('fc')
        #ok(dbls_equal(fc_cols.map((c) -> row1[c.idx]), [0,0.0546]), "Expected FC values")

        #afc_cols = gd.columns_by_type('afc')
        #ok(dbls_equal(afc_cols.map((c) -> row1[c.idx]), [-0.0417, 0.0129], "Expected AFC values"))

        start()
    )
)

asyncTest( "assoc columns gene_data", () ->
    expect( 3 )
    cols = [{idx: 'Feature', name: "id"}
            {idx: 'adj.P.Val', name: 'FDR', type: 'fdr'}
            {idx: 'ABS.1', name: 'Cond 1', type: 'abs' }
            {idx: 'ABS.2', name: 'Cond 2', type: 'abs' }
            {idx: 'AveExpr', name: 'Avg Expr', type: 'avg'}
            {idx: 'Mut1_R1', name: 'Mut1_R1', type: 'count', parent: 'Cond 1'}
            {idx: 'Mut1_R2', name: 'Mut1_R2', type: 'count', parent: 'Cond 1'}
            {idx: 'Mut1_R3', name: 'Mut1_R3', type: 'count', parent: 'Cond 1'}
            ]

    d3.csv("test2.csv", (d) ->
        gd = new GeneData(d, cols)
        count_cols = gd.assoc_column_by_type('count','Cond 1')
        row1 = gd.get_data()[0]
        counts = count_cols.map( (c) -> row1[c.idx] )

        equal(count_cols.length, 3, "Parent columns")
        deepEqual(counts, [53,31,54], "Counts correct values")

        ok(row1.id != null, "Created an id column")

        start()
    )
)
