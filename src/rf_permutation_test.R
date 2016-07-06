source(file.path("src", "heatmap.rf.r"))
library(randomForest)

permutation_test = function(X, y, num_permutations=1000) {
  # Set up the scramble vector
  scramble = 1:length(y)
  # Initialize the return vector
  accuracy = numeric(length=num_permutations)
  for (i in 1:num_permutations) {
    # Run hold one out cross-validation with a classifier
    X.rf = heatmap.rf(X, y[scramble], nfolds=-1, response.type="prob")
    # Evaluate the predicted vector
    predicted = as.numeric(sapply(X.rf$predicted, function(x) x[2] > .5) + 1)
    # Calculate the True Positive Rate
    accuracy[i] = sum(as.numeric(X.rf$y) == predicted)/length(X.rf$y)
    # Scramble the order vector
    scramble = sample(scramble)
  }
  return(accuracy)
}
