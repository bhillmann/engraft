source(file.path("src", "heatmap.rf.r"))
library(randomForest)



# family standardized data
# family_standardized = load_data_table('RDP_family_AH_standarized.txt')
mapping_maria = load_data_table('mapping_maria_BL.txt')

infiles = c('genes_CEFITH_standarized.txt', 'RDP_family_AH_standarized.txt', 'RDP_genus_AH_standarized.txt', 'OTU_AH_standarized.txt')

preprocess_mat = function(infile) {
  df = load_data_table(infile)
  mat = t(df[,-1])
  colnames(mat) = df[,1]
  return(mat)
}

x = sapply(infiles, preprocess_mat)

jackknife = function(X, y) {
  num_features = dim(mat_genes)[1]
  accuracy = numeric(length=num_features)
  for (i in 1:num_features) {
    work.subset.rf = heatmap.rf(work.subset[-i,], persistence[-i], nfolds=-1)
    accuracy[i] = sum(work.subset.rf$y == work.subset.rf$predicted)/length(work.subset.rf$y)
  }
  return(accuracy)
}