{{> common}}

nf <- calcNormFactors(counts)
y<-voomWithQualityWeights(counts, design, plot=FALSE,lib.size=colSums(counts)*nf)

cont.matrix <- {{cont_matrix}}

fit <- lmFit(y,design)
fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2)

out <- topTable(fit2, n=Inf, sort.by='none')

out2 <- cbind(fit2$coef,
              out[, c('adj.P.Val','AveExpr')],
              x[, c({{export_cols}})] )

write.csv(out2, file="{{file}}", row.names=FALSE,na='')

