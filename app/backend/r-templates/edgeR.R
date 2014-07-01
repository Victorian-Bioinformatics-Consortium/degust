{{> common}}

cont.matrix <- {{cont_matrix}}

y <- DGEList(counts=counts)

y <- calcNormFactors(y, method="TMM")

y <- estimateGLMCommonDisp(y,design)
y <- estimateGLMTrendedDisp(y,design)
y <- estimateGLMTagwiseDisp(y,design)

fit <- glmFit(y,design)
lrt <- glmLRT(fit, contrast=cont.matrix)

out <- topTags(lrt, n=Inf, sort.by='none')$table

lfc <- as.matrix(out[, c(1:ncol(cont.matrix))])
colnames(lfc) <- colnames(cont.matrix)

# Output with column names for degust
out2 <- cbind(lfc,
              'adj.P.Val' = out[,'FDR'],
              'AveExpr'   = out[,'logCPM'],
              x[, c({{export_cols}})] )

write.csv(out2, file="{{file}}", row.names=FALSE,na='')


