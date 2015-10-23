source('env.R')
source(file.path("src", "heatmap.rf.r"))
library(randomForest)

insert.at = function(a, pos, element) {
  len = length(a)
  if (pos > len) {
    c(a, element)
  } else if (pos == 1) {
    c(element, a)
  } else {
    c(a[1:pos-1], element, a[pos:len])
  }
}

jackknife = function(X, y, name) {
  num_features = dim(X)[2]
  accuracy = numeric(length=num_features)
  mean_importance_matrix = matrix(NA, num_features, num_features)
  for (i in 1:num_features) {
    X.rf = heatmap.rf(X[,-i], y, nfolds=-1)
    accuracy[i] = sum(X.rf$y == X.rf$predicted)/length(X.rf$y)
    mean.importance = as.numeric(X.rf$mean.importance)
    mean_importance_matrix[,i] = insert.at(mean.importance, i, NA)
  }
  write.table(mean_importance_matrix, file = file.path("results", paste("mean_importance_", name, sep="")), col.names = colnames(X), row.names = colnames(X), sep = "\t", quote = FALSE, append = FALSE)
  return(accuracy)
}

run_jackknife_infiles = function(XX, y, names) {
  mat_acc = list()
  for (i in 1:length(names)) {
    mat_acc[[i]] = jackknife(XX[[i]], y, names[i])
  }
  pdf(file.path("results", "jackknife_boxplot.pdf"))
  boxplot(mat_acc, names=names, main="Jackknife Accuracy Performance by \n Feature Sets", ylab="Accuracy", las = 2)
  dev.off()
  
  indx = sapply(mat_acc, length)
  mat_acc = as.data.frame(do.call(rbind, lapply(mat_acc, 'length<-', max(indx))))
  rownames(mat_acc) = names
  write.table(mat_acc, file=file.path("results", "jackknife_accuracy.txt"), sep = "\t", quote = FALSE, append = FALSE)
}

