library(DMwR)
library(Boruta)
library(dplyr)
library(caret)
library(randomForest)
library(hydroGOF)
library(e1071)
library(corrplot)

train<-read.csv("D:/My Stuff/Kaggle/Restaurant Revenue Prediction/train.csv")
test<-read.csv("D:/My Stuff/Kaggle/Restaurant Revenue Prediction/test.csv")
train<-train[,-1]
test<-test[,-1]
Revenue<-train$revenue

test$City.Group<-as.numeric(test$City.Group)
train$City.Group<-as.numeric(train$City.Group)

test$Type<-as.numeric(test$Type)
train$Type<-as.numeric(train$Type)



library(Boruta)
set.seed(123)
boruta_output <- Boruta( train$revenue~ ., data = na.omit(train), doTrace=2)
boruta_output$finalDecision

table(boruta_output$finalDecision)


final.boruta <- TentativeRoughFix(boruta_output)
final.boruta

getSelectedAttributes(boruta_output)
getSelectedAttributes(final.boruta, withTentative = F)
getSelectedAttributes(boruta_output)

boruta.df <- attStats(final.boruta)
print(boruta.df)
arrange(cbind(attr=rownames(attStats(boruta_output)), attStats(boruta_output)),desc(medianImp))


names(train)
train_impVar<-train[,getSelectedAttributes(final.boruta)]
names(train_impVar)

train_impVar


finalData<-train_impVar
good_columns<-names(train_impVar)
cbind()
finalData <- cbind(finalData[,c(good_columns)],Revenue)

inTraining <- createDataPartition(finalData$Revenue, p=0.6, list=FALSE)
trainingStd <- finalData[inTraining,]
testdataStd <- finalData[-inTraining,]
inVal <- createDataPartition(testdataStd$Revenue, p=0.5, list=FALSE)
crossvalStd <- testdataStd[inVal,]
testingStd <- testdataStd[-inVal,]


##xgbTree
curr.model <- train(trainingStd$Revenue~.,data=trainingStd,method = "xgbTree")
preds<- predict(curr.model,testdataStd)
preds
sqrt(mean((log(testdataStd$Revenue+1) - log(preds+1))^2))


## Knn
curr.model <- train(trainingStd$Revenue ~ .,data=trainingStd,method = "knn")
preds<- predict(curr.model,testdataStd)
preds
sqrt(mean((log(testdataStd$Revenue+1) - log(preds+1))^2))

## SVM
model <- svm(trainingStd$Revenue ~ .,data=trainingStd)
svmoutput<- predict(object = model, newdata = testdataStd)
svmoutput
sqrt(mean((log(testdataStd$Demanda_uni_equil+1) - log(svmoutput+1))^2))

## Random Forest
rfFit=randomForest(Demanda_uni_equil ~ .,data = trainingStd)
rfpreds<- predict(rfFit,testdataStd)
sqrt(mean((log(testdataStd$Demanda_uni_equil+1) - log(rfpreds+1))^2))


