source(file.path("src", "heatmap.rf.r"))
library(randomForest)

# family standardized data
# family_standardized = load_data_table('RDP_family_AH_standarized.txt')
mapping_maria = load_data_table('mapping_maria_BL.txt')

infiles = c('genes_CEFITH_standarized.txt', 'RDP_family_AH_standarized.txt', 'RDP_genus_AH_standarized.txt', 'OTU_AH_standarized.txt')

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
  print(colnames(X))
  for (i in 1:num_features) {
    X.rf = heatmap.rf(X[,-i], y, nfolds=-1)
    accuracy[i] = sum(X.rf$y == X.rf$predicted)/length(X.rf$y)
    mean.importance = X.rf$mean.importance
    mean_importance_matrix[,i] = insert.at(mean.importance, i, NA)
  }
  write.table(mean_importance_matrix, file = file.path("results", paste("mean_importance_", name)), sep = "\t", quote = FALSE, append = FALSE)
  return(accuracy)
}

# a = jackknife(preprocess_mat(infiles[1]), persistence)
mat_acc = sapply(infiles, function(x) {jackknife(preprocess_normalized_mat(x), mapping_maria$Persistence, x)})
boxplot(mat_acc, names=c('genes', 'RDP_fam', 'RDP_gen', 'OTU'), main="Jackknife Accuracy Performance by \n Feature Sets", ylab="Accuracy", las = 2)
write.table(mat_acc, file = file.path("results", "jackknife_accuracy.txt"), sep = "\t", quote = FALSE, append = FALSE)
