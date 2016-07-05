source(file.path("src", "heatmap.rf.r"))
library(randomForest)

permutation_test = function(X, y, num_permutations=1000) {
  scramble = 1:length(y)
  accuracy = numeric(length=num_permutations)
  for (i in 1:num_permutations) {
    X.rf = heatmap.rf(X, y[scramble], nfolds=-1, response.type="prob")
    predicted = as.numeric(sapply(X.rf$predicted, function(x) x[2] > .5) + 1)
    accuracy[i] = sum(as.numeric(X.rf$y) == predicted)/length(X.rf$y)
    scramble = sample(scramble)
  }
  return(accuracy)
}
