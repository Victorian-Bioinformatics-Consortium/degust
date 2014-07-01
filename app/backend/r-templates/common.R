{{> libraries}}

x<-read.delim('{{counts_file}}',skip={{counts_skip}}, sep="{{sep_char}}", check.names=FALSE)
counts <- x[,{{columns}}]
keep <- apply(counts, 1, max) >= {{min_counts}}
x <- x[keep,]
counts <- counts[keep,]
design <- {{design}}

