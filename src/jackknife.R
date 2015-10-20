source(file.path("src", "heatmap.rf.r"))
library(randomForest)

# family standardized data
# family_standardized = load_data_table('RDP_family_AH_standarized.txt')
mapping_maria = load_data_table('mapping_maria_BL.txt')

infiles = c('genes_CEFITH_standarized.txt', 'RDP_family_AH_standarized.txt', 'RDP_genus_AH_standarized.txt', 'OTU_AH_standarized.txt')
# infiles = c('y.txt', 'x.txt')

insert.at = function(a, pos, ...){
  dots <- list(...)
  if (length(a) > pos) {
    stopifnot(length(dots)==length(pos))
    result <- vector("list",2*length(pos)+1)
    result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
    result[c(FALSE,TRUE)] <- dots
    unlist(result)
  } else {
    return(unlist(c(a, dots)))
  }
}

jackknife = function(X, y, name) {
  num_features = dim(X)[2]
  accuracy = numeric(length=num_features)
  mean_importance_matrix = matrix(NA, num_features, num_features)
  for (i in 1:num_features) {
    X.rf = heatmap.rf(X[,-i], y, nfolds=-1)
    accuracy[i] = sum(X.rf$y == X.rf$predicted)/length(X.rf$y)
    mean.importance = X.rf$mean.importance
    mean_importance_matrix[,i] = insert.at(mean.importance, i, NA)
  }
  write.table(mean_importance_matrix, file = file.path("results", paste("mean_importance_", name, sep="")), sep = "\t", quote = FALSE, append = FALSE)
  return(accuracy)
}

mat_acc = sapply(infiles, function(x) {jackknife(preprocess_normalized_mat(x), mapping_maria$Persistence, x)})
pdf(file.path("results", "jackknife_boxplot.pdf"))
names = c('genes', 'RDP_fam', 'RDP_gen', 'OTU')
# names = c('y', 'x')
boxplot(mat_acc, names=names, main="Jackknife Accuracy Performance by \n Feature Sets", ylab="Accuracy", las = 2)
dev.off()

indx = sapply(mat_acc, length)
mat_acc = as.data.frame(do.call(rbind, lapply(mat_acc, 'length<-', max(indx))))
colnames(mat_acc) = names
write.table(mat_acc, file=file.path("results", "jackknife_accuracy.txt"), sep = "\t", quote = FALSE, append = FALSE)
