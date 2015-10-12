setwd("~/Documents/Xime/1c_Prediction_models/CEFITH")
Sys.setenv(WORKING_DIR=getwd())

Sys.setenv(RSCRIPTS="~/Documents/Xime/Random_forest/")

source(paste(Sys.getenv("RSCRIPTS"),"heatmap.rf.r",sep="/"))

work.subset= read.table("TABLEgenes_RFinput.txt",header=TRUE)
n.otus = 60199 # change accordingly 

#Random forest
library(randomForest)
accuracy <- NULL
for(a in 1:1000)
{
  work.subset.rf <- heatmap.rf(work.subset[,2:n.otus],as.factor(work.subset$Persistence),nfolds=-1)  
  accuracy[a] <- sum(work.subset.rf$y == work.subset.rf$predicted)/length(work.subset.rf$y)
}
print(mean(accuracy))
print(sd(accuracy))
print(cbind(sort(work.subset.rf$mean.importance[work.subset.rf$mean.importance > .01], decreasing=T)))

out <- (cbind(sort(work.subset.rf$mean.importance[work.subset.rf$mean.importance > -1], decreasing=T)))
write.table (out, file = "RF_Output_CEFITHgenes.txt", sep = "\t", quote = FALSE, append = FALSE)


