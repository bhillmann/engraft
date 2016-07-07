source(file.path("src", "heatmap.rf.r"))
source(file.path("src", "cross_validation.R"))
library(randomForest)

permutation_test = function(X, y, num_permutations=999, nfolds=-1) {
  errors = replicate(num_permutations, {rf.cross.validation(X, sample(y), nfolds=nfolds)$err})
  output = rf.cross.validation(X, y, nfolds=nfolds)
  output$permutation_errs = append(errors, output$err)
  output
}
