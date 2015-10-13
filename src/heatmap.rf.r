# this version of RF classification specifically also outputs var importance for use in constructing heatmaps
# runs cross-validation
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
# response.type: part of randomForest params, useful for plotting ROC curve (use "prob")
"heatmap.rf" <- function(x, y, nfolds=10, verbose=FALSE, response.type="response", ...){
    if(nfolds==-1) nfolds <- length(y)
    folds <- sample(rep(1:nfolds,ceiling(length(y)/nfolds)))

    # let it do classification if it needs to
    #if(class(y) != 'numeric') stop('y must be numeric for regression\n')

    result <- list()
    var.imp <- NULL
    result$y <- y
    if(response.type== "prob") {result$predicted <- list()
    } else {result$predicted <- result$y}

    # K-fold cross-validation
    for(fold in sort(unique(folds))){
        if(verbose) cat(sprintf('Fold %d...\n',fold))
        foldix <- which(folds==fold)
        if(class(y)=='numeric') y.foldix <- result$y[-foldix]
        else y.foldix <- droplevels(result$y[-foldix])
        model <- randomForest(x[-foldix,], y.foldix , importance=TRUE, do.trace=verbose,...)

        var.imp <- cbind(var.imp, importance(model, type=1))

        newx <- x[foldix,]
        if(length(foldix)==1) newx <- matrix(newx,nrow=1)

        if(response.type=="prob"){ result$predicted[[foldix]] <- predict(model, newx, type=response.type)
		} else {result$predicted[foldix] <- predict(model, newx, type=response.type)}

    }

	result$mean.importance <- rowMeans(var.imp)
	result$rmse <- sqrt(mean((y - result$predicted)**2))
	result$nfolds <- nfolds
    result$params <- list(...)
    return(result)
}
