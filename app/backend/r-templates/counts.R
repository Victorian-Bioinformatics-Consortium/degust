  counts <- cbind(x[,c({{colsToRList extra_cols}})], counts)
  write.csv(counts, file="{{file}}", row.names=FALSE)

