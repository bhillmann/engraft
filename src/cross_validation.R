# runs cross-validation 
# if predict.fun is NULL, uses S3 predict method
# if nfolds > length(y) or nfolds==-1, uses leave-one-out cross-validation
# ...: additional parameters for train.fun
#
# value:
# y: true values
# predicted: cv predicted values
# probabilities: cv predicted class probabilities (or NULL if unavailable)
# confusion.matrix: confusion matrix (true x predicted)
# nfolds: nfolds
# params: list of additional parameters
# importances: importances of features as predictors
"rf.cross.validation" <- function(x, y, nfolds=10, folds=NULL, verbose=FALSE, ...){
  require('randomForest')
  if(nfolds==-1) nfolds <- length(y)
  if(is.null(folds)) folds <- balanced.folds(y,nfolds=nfolds)
  result <- list()
  result$y <- as.factor(y)
  result$predicted <- result$y
  result$probabilities <- matrix(0, nrow=length(result$y), ncol=length(levels(result$y)))
  rownames(result$probabilities) <- rownames(x)
  colnames(result$probabilities) <- levels(result$y)
  result$importances <- matrix(0,nrow=ncol(x),ncol=nfolds)
  result$errs <- numeric(length(unique(folds)))
  
  # K-fold cross-validation
  for(fold in sort(unique(folds))){
    if(verbose) cat(sprintf('Fold %d...\n',fold))
    foldix <- which(folds==fold)
    model <- randomForest(x[-foldix,], factor(result$y[-foldix]), importance=TRUE, do.trace=verbose, ...)
    newx <- x[foldix,]
    if(length(foldix)==1) newx <- matrix(newx,nrow=1)
    result$predicted[foldix] <- predict(model, newx)
    probs <- predict(model, newx, type='prob')
    result$probabilities[foldix,colnames(probs)] <- probs
    result$errs[fold] <- mean(result$predicted[foldix] != result$y[foldix])
    result$importances[,fold] <- model$importance[,'MeanDecreaseAccuracy']
  }
  rownames(result$importances) <- colnames(x)
  result$err <- mean(result$predicted != result$y)
  result$nfolds <- nfolds
  result$params <- list(...)
  result$confusion.matrix <- t(sapply(levels(y), function(level) table(result$predicted[y==level])))
  return(result)    
}

# Get balanced folds where each fold has close to overall class ratio
"balanced.folds" <- function(y, nfolds=10){
  folds = rep(0, length(y))
  y <- as.factor(y)
  classes = levels(y)
  # size of each class
  Nk = table(y)
  # -1 or nfolds = len(y) means leave-one-out
  if (nfolds == -1 || nfolds == length(y)){
    invisible(1:length(y))
  }
  else{
    # Can't have more folds than there are items per class
    nfolds = min(nfolds, max(Nk))
    # Assign folds evenly within each class, then shuffle within each class
    for (k in 1:length(classes)){
      ixs <- which(y==classes[k])
      folds_k <- rep(1:nfolds, ceiling(length(ixs) / nfolds))
      folds_k <- folds_k[1:length(ixs)]
      folds_k <- sample(folds_k)
      folds[ixs] = folds_k
    }
    invisible(folds)
  }
}
