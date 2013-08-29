window.settings = {
  name: "Basic Example",
  csv: "basic-example.csv",
  csv_format: true,

  columns: [{idx: 'id', name: "id", type: 'info'},
            {idx: 'adj.P.Val', name: 'FDR', type: 'fdr'},
            {idx: 'ABS.1', name: 'Cond 1', type: 'abs', is_pri: true},
            {idx: 'ABS.2', name: 'Cond 2', type: 'abs'},
            {idx: 'AveExpr', name: 'Avg Expr', type: 'avg'},
           ]
};
