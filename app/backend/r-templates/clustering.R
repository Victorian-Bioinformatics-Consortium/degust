{{< common}}

nf <- calcNormFactors(counts)
y<-voom(counts, design, plot=FALSE,lib.size=colSums(counts)*nf)

fit <- lmFit(y,design)

# Cluster ordering...
library(seriation)
d <- dist(fit$coefficients[,c({{count_columns}})])
c <- list(hclust = hclust(d))
s <- seriate(d, method='OLO', control=c)
order <- get_order(s[[1]])
write.csv(list(id=order), file="{{file}}", row.names=FALSE)

