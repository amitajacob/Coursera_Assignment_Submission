library(caret)
setwd("C:/Assignment/COURSERA/PRACTICAL MACHINE LEARNING")
trainingDs<-read.csv("pml-training.csv", header=TRUE, na.strings=c("NA", "#DIV/0!" ,""))
testDs<-read.csv("pml-testing.csv", header= TRUE, na.strings=c("NA", "#DIV/0!" ,""))
trainingDsFiltered <- trainingDs [, apply(trainingDs, 2, function(x) !any(is.na(x)))] 
trainingDsTrimmed <- trainingDsFiltered[,-c(1:8)]
testDsTrimmed<- testDs [,names(trainingDsTrimmed [,-52])]

partitionedTrain<-createDataPartition(y= trainingDsTrimmed$classe, p=0.75,list=F)
trainingData<- trainingDsTrimmed [partitionedTrain,]  
testData <- trainingDsTrimmed [-partitionedTrain,] 
set.seed(42)
fitControl<-trainControl(method="cv", number=5, allowParallel=T)
rffit<-train(classe~.,data= trainingData, method="rf", trControl=fitControl)
predrf<-predict(rffit, newdata= testData)


confusionMatrix(predrf, testData$classe)
prediction_20<-predict(rffit, newdata=testDsTrimmed)
# Output for the prediction of the 20 cases provided
 
getwd()
pml_write_files = function(yy) 
{
  n = length(yy)
  for (i in 1:n)
  {
    fname = paste0("problem_id_", i,".txt")
    write.table(yy[i],file=fname,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(prediction_20)
